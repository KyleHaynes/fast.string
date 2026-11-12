// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "parallel_dispatch.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// Candidate pairs for record linkage: block_pairs() and
// sorted_neighbourhood(), plus cluster_pairs().
//
// Each pass gives every record an integer code: the rank of its key among
// the sorted distinct keys (NA for a missing key). A pass with half-window h
// pairs records whose codes differ by at most h, so h = 0 is exact blocking
// and h = (window - 1) / 2 is the sorted neighbourhood method as Python's
// recordlinkage implements it. Pairs found by several passes are reported
// once, with the first pass that found them.

namespace {

struct Pass {
    const int* x_codes;
    int half_window;
    int n_codes;
    std::vector<std::size_t> offsets;  // y records with code c: [offsets[c], offsets[c + 1])
    std::vector<int> members;          // y record indices, by code then index
    std::vector<unsigned char> skipped;  // codes whose block is too big
};

struct Candidate {
    int y;
    int pass;
};

struct PairChunk {
    std::vector<int> x;
    std::vector<int> y;
    std::vector<int> pass;
};

// Every worker below walks the x records of [x_from, x_to) in chunks of
// rows_per_chunk, so that a caller can ask for the pairs of one slice of the
// records at a time. rLink() does that when it filters while scoring: the
// candidates of a slice are scored and thinned before the next slice is
// generated, and the whole candidate set never has to exist at once.
struct PairWorker : public Worker {
    const std::vector<Pass>& passes;
    bool dedupe;
    std::size_t rows_per_chunk;
    std::size_t x_from;
    std::size_t x_to;
    std::vector<PairChunk>& chunks;

    PairWorker(const std::vector<Pass>& passes_, bool dedupe_,
               std::size_t rows_per_chunk_, std::size_t x_from_,
               std::size_t x_to_, std::vector<PairChunk>& chunks_)
        : passes(passes_), dedupe(dedupe_), rows_per_chunk(rows_per_chunk_),
          x_from(x_from_), x_to(x_to_), chunks(chunks_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<Candidate> found;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            PairChunk& out = chunks[chunk];
            const std::size_t first = x_from + chunk * rows_per_chunk;
            const std::size_t last = (std::min)(x_to, first + rows_per_chunk);
            for (std::size_t i = first; i < last; ++i) {
                found.clear();
                // Several passes can report the same partner, and a window
                // wider than one code interleaves partners from neighbouring
                // codes; either one means the list has to be put in order
                // before duplicates can be dropped.
                int contributors = 0;
                bool ordered = true;
                for (std::size_t p = 0; p < passes.size(); ++p) {
                    const Pass& pass = passes[p];
                    const int code = pass.x_codes[i];
                    if (code == NA_INTEGER) continue;
                    if (!pass.skipped.empty() && pass.skipped[code]) continue;
                    const int lo = (std::max)(1, code - pass.half_window);
                    const int hi = (std::min)(pass.n_codes,
                                              code + pass.half_window);
                    const std::size_t from = pass.offsets[lo];
                    const std::size_t to = pass.offsets[hi + 1];
                    const std::size_t before = found.size();
                    for (std::size_t k = from; k < to; ++k) {
                        const int j = pass.members[k];
                        if (dedupe && j <= static_cast<int>(i)) continue;
                        found.push_back(Candidate{j, static_cast<int>(p)});
                    }
                    if (found.size() == before) continue;
                    if (++contributors > 1 || pass.half_window != 0)
                        ordered = false;
                }
                if (found.empty()) continue;
                if (ordered) {
                    for (const Candidate& c : found) {
                        out.x.push_back(static_cast<int>(i) + 1);
                        out.y.push_back(c.y + 1);
                        out.pass.push_back(c.pass + 1);
                    }
                    continue;
                }
                std::sort(found.begin(), found.end(),
                          [](const Candidate& a, const Candidate& b) {
                              return a.y != b.y ? a.y < b.y : a.pass < b.pass;
                          });
                int previous = -1;
                for (const Candidate& c : found) {
                    if (c.y == previous) continue;
                    previous = c.y;
                    out.x.push_back(static_cast<int>(i) + 1);
                    out.y.push_back(c.y + 1);
                    out.pass.push_back(c.pass + 1);
                }
            }
        }
    }
};

// One exact pass is the common case (block_pairs() on a single key). A
// block's members are already in ascending record order and cannot repeat,
// so there is nothing to sort or de-duplicate -- and, unlike the general
// case, the number of pairs each record contributes is known before any is
// written: it is the size of its block, or of the part of the block above
// it when deduplicating. So the pairs are counted first and then written
// straight into the result, in parallel. That does away with the per-chunk
// buffers the general case grows and then copies out of, which at tens of
// millions of pairs is both the peak memory and a serial pass over all of
// it.
struct ExactRange {
    const int* from;
    const int* to;
};

inline ExactRange exact_partners(const Pass& pass, bool has_skipped,
                                 bool dedupe, std::size_t i) {
    const int code = pass.x_codes[i];
    if (code == NA_INTEGER) return ExactRange{nullptr, nullptr};
    if (has_skipped && pass.skipped[code]) return ExactRange{nullptr, nullptr};
    const int* from = pass.members.data() + pass.offsets[code];
    const int* to = pass.members.data() + pass.offsets[code + 1];
    // members ascend within a block, so when deduplicating the partners
    // above i are a suffix, found by binary search rather than by walking
    // the whole block.
    if (dedupe) from = std::upper_bound(from, to, static_cast<int>(i));
    return ExactRange{from, to};
}

struct ExactCountWorker : public Worker {
    const Pass& pass;
    bool dedupe;
    std::size_t rows_per_chunk;
    std::size_t x_from;
    std::size_t x_to;
    std::vector<std::size_t>& counts;

    ExactCountWorker(const Pass& pass_, bool dedupe_,
                     std::size_t rows_per_chunk_, std::size_t x_from_,
                     std::size_t x_to_, std::vector<std::size_t>& counts_)
        : pass(pass_), dedupe(dedupe_), rows_per_chunk(rows_per_chunk_),
          x_from(x_from_), x_to(x_to_), counts(counts_) {}

    void operator()(std::size_t begin, std::size_t end) {
        const bool has_skipped = !pass.skipped.empty();
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            const std::size_t first = x_from + chunk * rows_per_chunk;
            const std::size_t last = (std::min)(x_to, first + rows_per_chunk);
            std::size_t n = 0;
            for (std::size_t i = first; i < last; ++i) {
                const ExactRange r =
                    exact_partners(pass, has_skipped, dedupe, i);
                n += static_cast<std::size_t>(r.to - r.from);
            }
            counts[chunk] = n;
        }
    }
};

struct ExactFillWorker : public Worker {
    const Pass& pass;
    bool dedupe;
    std::size_t rows_per_chunk;
    std::size_t x_from;
    std::size_t x_to;
    const std::vector<std::size_t>& starts;
    int* out_x;
    int* out_y;

    ExactFillWorker(const Pass& pass_, bool dedupe_,
                    std::size_t rows_per_chunk_, std::size_t x_from_,
                    std::size_t x_to_, const std::vector<std::size_t>& starts_,
                    int* out_x_, int* out_y_)
        : pass(pass_), dedupe(dedupe_), rows_per_chunk(rows_per_chunk_),
          x_from(x_from_), x_to(x_to_), starts(starts_), out_x(out_x_),
          out_y(out_y_) {}

    void operator()(std::size_t begin, std::size_t end) {
        const bool has_skipped = !pass.skipped.empty();
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            const std::size_t first = x_from + chunk * rows_per_chunk;
            const std::size_t last = (std::min)(x_to, first + rows_per_chunk);
            std::size_t at = starts[chunk];
            for (std::size_t i = first; i < last; ++i) {
                const ExactRange r =
                    exact_partners(pass, has_skipped, dedupe, i);
                const int x = static_cast<int>(i) + 1;
                for (const int* k = r.from; k != r.to; ++k) {
                    out_x[at] = x;
                    out_y[at] = *k + 1;
                    ++at;
                }
            }
        }
    }
};

const char* const too_many_pairs =
    "These keys give more than 2^31 candidate pairs; block on more specific "
    "keys or use a smaller window.";

}  // namespace

// [[Rcpp::export]]
List fast_candidate_pairs_impl(const List& x_codes, const List& y_codes,
                               const IntegerVector& n_codes,
                               const IntegerVector& half_windows, bool dedupe,
                               double max_block, int nthreads,
                               double x_from_, double x_to_) {
    const std::size_t n_passes = static_cast<std::size_t>(x_codes.size());
    if (n_passes == 0) stop("At least one key is needed.");
    std::vector<IntegerVector> xs, ys;
    for (std::size_t p = 0; p < n_passes; ++p) {
        xs.push_back(as<IntegerVector>(x_codes[p]));
        ys.push_back(as<IntegerVector>(y_codes[p]));
    }
    const std::size_t n_x = static_cast<std::size_t>(xs[0].size());
    if (!(x_from_ >= 0) || !(x_to_ >= x_from_) ||
        x_to_ > static_cast<double>(n_x))
        stop("The range of records asked for is not within the keys.");
    const std::size_t x_from = static_cast<std::size_t>(x_from_);
    const std::size_t x_to = static_cast<std::size_t>(x_to_);
    const bool whole = x_from == 0 && x_to == n_x;

    std::vector<Pass> passes(n_passes);
    double bound = 0;
    std::size_t skipped_blocks = 0;
    for (std::size_t p = 0; p < n_passes; ++p) {
        Pass& pass = passes[p];
        const IntegerVector& xc = xs[p];
        const IntegerVector& yc = ys[p];
        pass.x_codes = xc.begin();
        pass.half_window = half_windows[p];
        pass.n_codes = n_codes[p];
        const std::size_t codes = static_cast<std::size_t>(pass.n_codes);
        // Counting sort of the y records by code.
        pass.offsets.assign(codes + 2, 0);
        for (R_xlen_t j = 0; j < yc.size(); ++j) {
            if (yc[j] != NA_INTEGER) ++pass.offsets[yc[j] + 1];
        }
        for (std::size_t c = 1; c < pass.offsets.size(); ++c)
            pass.offsets[c] += pass.offsets[c - 1];
        pass.members.resize(pass.offsets[codes + 1]);
        std::vector<std::size_t> fill(pass.offsets.begin(),
                                      pass.offsets.end() - 1);
        for (R_xlen_t j = 0; j < yc.size(); ++j) {
            if (yc[j] != NA_INTEGER)
                pass.members[fill[yc[j]]++] = static_cast<int>(j);
        }
        // Which blocks are too big is a property of the keys, so it is
        // decided over every x record whatever range was asked for: a block
        // must not be skipped in one slice and kept in the next.
        if (pass.half_window == 0 && max_block < 1e18) {
            std::vector<double> x_count(codes + 1, 0);
            for (std::size_t i = 0; i < n_x; ++i) {
                if (pass.x_codes[i] != NA_INTEGER) ++x_count[pass.x_codes[i]];
            }
            pass.skipped.assign(codes + 1, 0);
            for (std::size_t c = 1; c <= codes; ++c) {
                const double y_count = static_cast<double>(
                    pass.offsets[c + 1] - pass.offsets[c]);
                const double size = dedupe ? y_count : x_count[c] + y_count;
                if (size > max_block) {
                    pass.skipped[c] = 1;
                    ++skipped_blocks;
                }
            }
        }
        // An upper bound on the pairs this pass can produce from the records
        // asked for.
        double partners = 0;
        for (std::size_t i = x_from; i < x_to; ++i) {
            const int code = pass.x_codes[i];
            if (code == NA_INTEGER) continue;
            if (!pass.skipped.empty() && pass.skipped[code]) continue;
            const int lo = (std::max)(1, code - pass.half_window);
            const int hi = (std::min)(pass.n_codes, code + pass.half_window);
            partners += static_cast<double>(pass.offsets[hi + 1] -
                                            pass.offsets[lo]);
        }
        // Deduplicating, each pair is counted from both of its records --
        // but only over the whole file. The first slice of one holds the
        // records with the most partners above them, so a slice keeps the
        // whole count.
        bound += partners / (dedupe && whole ? 2.0 : 1.0);
    }

    const double int_max =
        static_cast<double>((std::numeric_limits<int>::max)());
    const std::size_t rows_per_chunk = 1024;
    const std::size_t n_rows = x_to - x_from;
    const std::size_t n_chunks = (n_rows + rows_per_chunk - 1) / rows_per_chunk;
    const std::size_t work = static_cast<std::size_t>(
        (std::min)(bound + static_cast<double>(n_rows), 1e18));

    if (n_passes == 1 && passes[0].half_window == 0) {
        std::vector<std::size_t> counts(n_chunks, 0);
        ExactCountWorker counter(passes[0], dedupe, rows_per_chunk, x_from,
                                 x_to, counts);
        dispatch_for(0, n_chunks, counter, n_rows, 200000, nthreads, 1);
        std::vector<std::size_t> starts(n_chunks + 1, 0);
        for (std::size_t c = 0; c < n_chunks; ++c)
            starts[c + 1] = starts[c] + counts[c];
        const std::size_t total = starts[n_chunks];
        if (static_cast<double>(total) > int_max) stop(too_many_pairs);
        IntegerVector out_x(Rcpp::no_init(total)), out_y(Rcpp::no_init(total)),
            out_pass(Rcpp::no_init(total));
        ExactFillWorker filler(passes[0], dedupe, rows_per_chunk, x_from, x_to,
                               starts, out_x.begin(), out_y.begin());
        dispatch_for(0, n_chunks, filler, work, 200000, nthreads, 1);
        std::fill(out_pass.begin(), out_pass.end(), 1);
        return List::create(_["x_index"] = out_x, _["y_index"] = out_y,
                            _["pass"] = out_pass,
                            _["skipped_blocks"] =
                                static_cast<double>(skipped_blocks));
    }

    if (bound > int_max) stop(too_many_pairs);
    std::vector<PairChunk> chunks(n_chunks);
    PairWorker worker(passes, dedupe, rows_per_chunk, x_from, x_to, chunks);
    dispatch_for(0, n_chunks, worker, work, 200000, nthreads, 1);

    std::size_t total = 0;
    for (const PairChunk& c : chunks) total += c.x.size();
    IntegerVector out_x(Rcpp::no_init(total)), out_y(Rcpp::no_init(total)),
        out_pass(Rcpp::no_init(total));
    std::size_t at = 0;
    for (const PairChunk& c : chunks) {
        std::copy(c.x.begin(), c.x.end(), out_x.begin() + at);
        std::copy(c.y.begin(), c.y.end(), out_y.begin() + at);
        std::copy(c.pass.begin(), c.pass.end(), out_pass.begin() + at);
        at += c.x.size();
    }
    return List::create(_["x_index"] = out_x, _["y_index"] = out_y,
                        _["pass"] = out_pass,
                        _["skipped_blocks"] = static_cast<double>(skipped_blocks));
}

// [[Rcpp::export]]
IntegerVector fast_cluster_pairs_impl(const IntegerVector& i,
                                      const IntegerVector& j, int n) {
    std::vector<int> parent(static_cast<std::size_t>(n));
    std::vector<int> size(static_cast<std::size_t>(n), 1);
    for (int k = 0; k < n; ++k) parent[k] = k;
    auto find = [&parent](int v) {
        while (parent[v] != v) {
            parent[v] = parent[parent[v]];  // path halving
            v = parent[v];
        }
        return v;
    };
    const R_xlen_t m = i.size();
    for (R_xlen_t k = 0; k < m; ++k) {
        int a = find(i[k] - 1);
        int b = find(j[k] - 1);
        if (a == b) continue;
        if (size[a] < size[b]) std::swap(a, b);
        parent[b] = a;
        size[a] += size[b];
    }
    // Clusters are numbered in the order of their smallest member.
    IntegerVector out(Rcpp::no_init(n));
    std::vector<int> label(static_cast<std::size_t>(n), 0);
    int next = 0;
    for (int k = 0; k < n; ++k) {
        const int root = find(k);
        if (label[root] == 0) label[root] = ++next;
        out[k] = label[root];
    }
    return out;
}
