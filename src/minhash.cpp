// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "codepoint_snapshot.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// minhash_pairs(): candidate pairs of strings whose character q-gram sets
// are likely to have a Jaccard similarity of at least a threshold, found
// with MinHash signatures (Broder, 1997) and banded locality-sensitive
// hashing (as in datasketch's MinHashLSH). Candidates can then be checked
// against their exact Jaccard similarity.

namespace {

typedef std::uint32_t cp_t;

inline std::uint64_t mix64(std::uint64_t x) {
    x ^= x >> 30;
    x *= 0xbf58476d1ce4e5b9ULL;
    x ^= x >> 27;
    x *= 0x94d049bb133111ebULL;
    x ^= x >> 31;
    return x;
}

// The set of a string's q-gram hashes (a non-empty string shorter than q
// is one gram), sorted and distinct, stored back to back for all strings.
struct ShingleSets {
    std::vector<std::uint64_t> values;
    std::vector<std::size_t> offsets;  // size n + 1
    std::vector<std::uint8_t> missing;
};

struct ShingleWorker : public Worker {
    const StringView* bytes;
    const CodepointView* points;  // null with use_bytes
    int q;
    std::vector<std::vector<std::uint64_t>>& per_item;

    ShingleWorker(const StringView* bytes_, const CodepointView* points_, int q_,
                  std::vector<std::vector<std::uint64_t>>& per_item_)
        : bytes(bytes_), points(points_), q(q_), per_item(per_item_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<cp_t> buffer;
        for (std::size_t i = begin; i < end; ++i) {
            if (bytes[i].is_na()) continue;
            const cp_t* s;
            int n;
            if (points) {
                s = points[i].data;
                n = static_cast<int>(points[i].size);
            } else {
                buffer.resize(bytes[i].size);
                for (std::size_t k = 0; k < bytes[i].size; ++k)
                    buffer[k] = static_cast<unsigned char>(bytes[i].data[k]);
                s = buffer.data();
                n = static_cast<int>(bytes[i].size);
            }
            std::vector<std::uint64_t>& out = per_item[i];
            if (n == 0) continue;
            const int width = n < q ? n : q;
            for (int start = 0; start + width <= n; ++start) {
                std::uint64_t h = 0x243F6A8885A308D3ULL + static_cast<std::uint64_t>(width);
                for (int k = 0; k < width; ++k) h = mix64(h ^ (s[start + k] + 0x9E3779B97F4A7C15ULL));
                out.push_back(h);
            }
            std::sort(out.begin(), out.end());
            out.erase(std::unique(out.begin(), out.end()), out.end());
        }
    }
};

ShingleSets shingle_sets(const StringVector& x, int q, bool use_bytes,
                         const char* name, int nthreads) {
    const StringSnapshot bytes(x, use_bytes ? SnapshotText::raw : SnapshotText::utf8);
    std::unique_ptr<CodepointSnapshot> points;
    if (!use_bytes) points.reset(new CodepointSnapshot(x, name));
    const std::size_t n = bytes.size();
    std::vector<std::vector<std::uint64_t>> per_item(n);
    ShingleWorker worker(bytes.data(), points ? points->data() : nullptr, q, per_item);
    dispatch_for(0, n, worker, static_cast<std::size_t>(bytes.total_bytes()) + n,
                 100000, nthreads, 256);
    ShingleSets sets;
    sets.offsets.assign(n + 1, 0);
    sets.missing.assign(n, 0);
    for (std::size_t i = 0; i < n; ++i) {
        sets.offsets[i + 1] = sets.offsets[i] + per_item[i].size();
        sets.missing[i] = bytes[i].is_na();
    }
    sets.values.reserve(sets.offsets[n]);
    for (auto& v : per_item) {
        sets.values.insert(sets.values.end(), v.begin(), v.end());
        std::vector<std::uint64_t>().swap(v);
    }
    return sets;
}

// Band keys: for item i and band b, a hash of its r minimum values.
struct SignatureWorker : public Worker {
    const ShingleSets& sets;
    const std::vector<std::uint64_t>& seeds;  // one per hash function
    int bands, rows;
    std::uint64_t* keys;  // n x bands

    SignatureWorker(const ShingleSets& sets_, const std::vector<std::uint64_t>& seeds_,
                    int bands_, int rows_, std::uint64_t* keys_)
        : sets(sets_), seeds(seeds_), bands(bands_), rows(rows_), keys(keys_) {}

    void operator()(std::size_t begin, std::size_t end) {
        const std::size_t perms = static_cast<std::size_t>(bands) * rows;
        std::vector<std::uint64_t> signature(perms);
        for (std::size_t i = begin; i < end; ++i) {
            const std::size_t from = sets.offsets[i], to = sets.offsets[i + 1];
            if (from == to) continue;
            std::fill(signature.begin(), signature.end(),
                      (std::numeric_limits<std::uint64_t>::max)());
            for (std::size_t s = from; s < to; ++s) {
                const std::uint64_t v = sets.values[s];
                for (std::size_t p = 0; p < perms; ++p) {
                    const std::uint64_t h = mix64(v ^ seeds[p]);
                    if (h < signature[p]) signature[p] = h;
                }
            }
            for (int b = 0; b < bands; ++b) {
                std::uint64_t key = 0x6A09E667F3BCC909ULL ^ static_cast<std::uint64_t>(b);
                for (int r = 0; r < rows; ++r)
                    key = mix64(key ^ signature[static_cast<std::size_t>(b) * rows + r]);
                keys[i * static_cast<std::size_t>(bands) + b] = key;
            }
        }
    }
};

struct BandEntry {
    std::uint64_t key;
    std::uint32_t band;
    std::uint32_t item;  // items of y follow the items of x

    bool operator<(const BandEntry& o) const {
        if (band != o.band) return band < o.band;
        if (key != o.key) return key < o.key;
        return item < o.item;
    }
};

double jaccard_sets(const ShingleSets& a, std::size_t i, const ShingleSets& b,
                    std::size_t j) {
    const std::uint64_t* p = a.values.data() + a.offsets[i];
    const std::uint64_t* pe = a.values.data() + a.offsets[i + 1];
    const std::uint64_t* r = b.values.data() + b.offsets[j];
    const std::uint64_t* re = b.values.data() + b.offsets[j + 1];
    const std::size_t na = static_cast<std::size_t>(pe - p);
    const std::size_t nb = static_cast<std::size_t>(re - r);
    std::size_t inter = 0;
    while (p < pe && r < re) {
        if (*p < *r) ++p;
        else if (*r < *p) ++r;
        else { ++inter; ++p; ++r; }
    }
    const std::size_t uni = na + nb - inter;
    return uni == 0 ? 0.0 : static_cast<double>(inter) / static_cast<double>(uni);
}

struct VerifyWorker : public Worker {
    const ShingleSets& a;
    const ShingleSets& b;
    const std::vector<std::uint64_t>& pairs;
    RVector<double> out;

    VerifyWorker(const ShingleSets& a_, const ShingleSets& b_,
                 const std::vector<std::uint64_t>& pairs_, NumericVector& out_)
        : a(a_), b(b_), pairs(pairs_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t k = begin; k < end; ++k) {
            const std::size_t i = static_cast<std::size_t>(pairs[k] >> 32);
            const std::size_t j = static_cast<std::size_t>(pairs[k] & 0xFFFFFFFFULL);
            out[k] = jaccard_sets(a, i, b, j);
        }
    }
};

}  // namespace

// y_present = false: pairs within x (i < j). Returns 1-based indices and
// the exact Jaccard similarity of each candidate pair.
// [[Rcpp::export]]
List fast_minhash_pairs_impl(const StringVector& x, const StringVector& y,
                             bool y_present, int q, int bands, int rows,
                             double seed, double threshold, bool verify,
                             bool use_bytes, int nthreads) {
    const std::size_t nx = static_cast<std::size_t>(x.size());
    const std::size_t ny = y_present ? static_cast<std::size_t>(y.size()) : 0;
    if (nx + ny >= (std::numeric_limits<std::uint32_t>::max)())
        stop("Too many strings.");
    const ShingleSets sx = shingle_sets(x, q, use_bytes, "x", nthreads);
    ShingleSets sy_storage;
    if (y_present) sy_storage = shingle_sets(y, q, use_bytes, "y", nthreads);
    const ShingleSets& sy = y_present ? sy_storage : sx;

    const std::size_t perms = static_cast<std::size_t>(bands) * rows;
    std::vector<std::uint64_t> seeds(perms);
    std::uint64_t state = static_cast<std::uint64_t>(seed) * 0x9E3779B97F4A7C15ULL + 1;
    for (std::size_t p = 0; p < perms; ++p) {
        state += 0x9E3779B97F4A7C15ULL;
        seeds[p] = mix64(state);
    }

    // Band keys for x, then y.
    const std::size_t total = nx + ny;
    std::vector<std::uint64_t> keys(total * static_cast<std::size_t>(bands), 0);
    {
        SignatureWorker wx(sx, seeds, bands, rows, keys.data());
        dispatch_for(0, nx, wx, sx.values.size() * perms, 200000, nthreads, 64);
        if (y_present) {
            SignatureWorker wy(sy, seeds, bands, rows,
                               keys.data() + nx * static_cast<std::size_t>(bands));
            dispatch_for(0, ny, wy, sy.values.size() * perms, 200000, nthreads, 64);
        }
    }
    std::vector<BandEntry> entries;
    entries.reserve(total * static_cast<std::size_t>(bands));
    for (std::size_t i = 0; i < total; ++i) {
        const ShingleSets& s = i < nx ? sx : sy;
        const std::size_t local = i < nx ? i : i - nx;
        if (s.offsets[local] == s.offsets[local + 1]) continue;  // NA or empty
        for (int b = 0; b < bands; ++b)
            entries.push_back(BandEntry{keys[i * static_cast<std::size_t>(bands) + b],
                                        static_cast<std::uint32_t>(b),
                                        static_cast<std::uint32_t>(i)});
    }
    std::vector<std::uint64_t>().swap(keys);
    std::sort(entries.begin(), entries.end());

    // Pairs sharing a bucket in any band, as (x item << 32) | other item.
    std::vector<std::uint64_t> pairs;
    for (std::size_t a = 0; a < entries.size();) {
        std::size_t b = a;
        while (b < entries.size() && entries[b].band == entries[a].band &&
               entries[b].key == entries[a].key)
            ++b;
        if (b - a > 1) {
            for (std::size_t u = a; u < b; ++u) {
                const std::uint32_t iu = entries[u].item;
                if (y_present && iu >= nx) break;  // items sorted: x first
                for (std::size_t v = u + 1; v < b; ++v) {
                    const std::uint32_t iv = entries[v].item;
                    if (y_present) {
                        if (iv < nx) continue;
                        pairs.push_back((static_cast<std::uint64_t>(iu) << 32) | (iv - nx));
                    } else {
                        pairs.push_back((static_cast<std::uint64_t>(iu) << 32) | iv);
                    }
                }
            }
            if (pairs.size() > (std::size_t(1) << 31))
                stop("Too many candidate pairs; raise `threshold` or use more rows per band.");
        }
        a = b;
    }
    std::vector<BandEntry>().swap(entries);
    std::sort(pairs.begin(), pairs.end());
    pairs.erase(std::unique(pairs.begin(), pairs.end()), pairs.end());

    NumericVector similarity = no_init(static_cast<R_xlen_t>(pairs.size()));
    VerifyWorker verifier(sx, sy, pairs, similarity);
    dispatch_for(0, pairs.size(), verifier, pairs.size() * 32, 100000, nthreads, 1024);

    std::size_t kept = 0;
    for (std::size_t k = 0; k < pairs.size(); ++k)
        if (!verify || similarity[k] >= threshold) ++kept;
    IntegerVector xi(kept), yi(kept);
    NumericVector sim(kept);
    std::size_t at = 0;
    for (std::size_t k = 0; k < pairs.size(); ++k) {
        if (verify && similarity[k] < threshold) continue;
        xi[at] = static_cast<int>(pairs[k] >> 32) + 1;
        yi[at] = static_cast<int>(pairs[k] & 0xFFFFFFFFULL) + 1;
        sim[at] = similarity[k];
        ++at;
    }
    return List::create(_["x_index"] = xi, _["y_index"] = yi, _["jaccard"] = sim);
}
