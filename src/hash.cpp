// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "hash_core.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// fhash(): one hex digest per string. The workers write fixed-width hex into
// one buffer; the CHARSXPs are made afterwards on the main thread.

namespace {

enum HashAlgo { HASH_MD5 = 0, HASH_SHA1, HASH_SHA256, HASH_SHA512, HASH_XXH64 };

const char HEX_DIGITS[] = "0123456789abcdef";

inline void to_hex(const unsigned char* digest, std::size_t size, char* out) {
    for (std::size_t i = 0; i < size; ++i) {
        out[2 * i] = HEX_DIGITS[digest[i] >> 4];
        out[2 * i + 1] = HEX_DIGITS[digest[i] & 15];
    }
}

// Hashes each string with `digest(data, size, out)` into width-byte slots.
template <typename Digest>
struct HashWorker : public Worker {
    const StringView* strings;
    const Digest& digest;
    std::size_t width;
    char* out;

    HashWorker(const StringView* strings_, const Digest& digest_,
               std::size_t width_, char* out_)
        : strings(strings_), digest(digest_), width(width_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            if (strings[i].is_na()) continue;
            digest(reinterpret_cast<const unsigned char*>(strings[i].data),
                   strings[i].size, out + i * width);
        }
    }
};

template <typename Digest>
CharacterVector run_hash(const StringSnapshot& snapshot, std::size_t bytes,
                         const Digest& digest, int nthreads) {
    const std::size_t n = snapshot.size();
    const std::size_t width = 2 * bytes;
    std::unique_ptr<char[]> hex(new char[n * width + 1]);
    HashWorker<Digest> worker(snapshot.data(), digest, width, hex.get());
    const std::size_t work = static_cast<std::size_t>(
        snapshot.total_bytes() + 64u * static_cast<std::uint64_t>(n));
    dispatch_for(0, n, worker, work, 50000, nthreads, 256);
    CharacterVector result(n);
    for (std::size_t i = 0; i < n; ++i) {
        if (snapshot[i].is_na()) {
            SET_STRING_ELT(result, i, NA_STRING);
        } else {
            SET_STRING_ELT(result, i,
                           Rf_mkCharLenCE(hex.get() + i * width,
                                          static_cast<int>(width), CE_UTF8));
        }
    }
    return result;
}

template <typename Hash>
struct PlainDigest {
    void operator()(const unsigned char* data, std::size_t size,
                    char* out) const {
        Hash hash;
        hash.update(data, size);
        unsigned char digest[Hash::digest_size];
        hash.final(digest);
        to_hex(digest, Hash::digest_size, out);
    }
};

template <typename Hash>
struct HmacDigest {
    const fasthash::Hmac<Hash>& hmac;
    explicit HmacDigest(const fasthash::Hmac<Hash>& hmac_) : hmac(hmac_) {}
    void operator()(const unsigned char* data, std::size_t size,
                    char* out) const {
        unsigned char digest[Hash::digest_size];
        hmac.mac(data, size, digest);
        to_hex(digest, Hash::digest_size, out);
    }
};

struct Xxh64Digest {
    std::uint64_t seed;
    void operator()(const unsigned char* data, std::size_t size,
                    char* out) const {
        const std::uint64_t h = fasthash::xxh64(data, size, seed);
        unsigned char digest[8];
        fasthash::store_be64(digest, h);
        to_hex(digest, 8, out);
    }
};

template <typename Hash>
CharacterVector hash_with(const StringSnapshot& snapshot, bool keyed,
                          const RawVector& key, int nthreads) {
    if (!keyed)
        return run_hash(snapshot, Hash::digest_size, PlainDigest<Hash>(),
                        nthreads);
    const fasthash::Hmac<Hash> hmac(RAW(key),
                                    static_cast<std::size_t>(key.size()));
    return run_hash(snapshot, Hash::digest_size, HmacDigest<Hash>(hmac),
                    nthreads);
}

}  // namespace

// [[Rcpp::export]]
CharacterVector fast_hash_impl(const StringVector& x, int algo, bool keyed,
                               const RawVector& key, double seed,
                               int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    switch (algo) {
        case HASH_MD5:
            return hash_with<fasthash::Md5>(snapshot, keyed, key, nthreads);
        case HASH_SHA1:
            return hash_with<fasthash::Sha1>(snapshot, keyed, key, nthreads);
        case HASH_SHA256:
            return hash_with<fasthash::Sha256>(snapshot, keyed, key, nthreads);
        case HASH_SHA512:
            return hash_with<fasthash::Sha512>(snapshot, keyed, key, nthreads);
        case HASH_XXH64: {
            Xxh64Digest digest{static_cast<std::uint64_t>(seed)};
            return run_hash(snapshot, 8, digest, nthreads);
        }
    }
    stop("Unknown hash algorithm.");
}

// One digest of a whole table of strings: the fingerprint a review file keeps
// of the pairs it was made on.
//
// It is defined in R, in .review_fingerprint(): hash every row with xxh64,
// join the digests a chunk at a time, hash those, and repeat until one is
// left. Written that way it made a new R string for every row and then a
// factor to split them by, and on two million pairs that was twelve seconds
// of the thirteen it took to open a review. This computes the same digest --
// the same bytes hashed in the same order -- without any of the intermediate
// strings existing, so a file written before reads as unchanged after.
//
// A row is its columns joined by U+001F, as paste(sep = "\x1f") joins them,
// and `order` (1-based, or empty for none) is the order the rows are hashed
// in. paste() writes a missing value as the two letters "NA", and so does
// this when `na_as_text` says the rows were pasted; where they were not, a
// missing value has no digest and the answer is NA, for the caller to fall
// back on the R definition.

namespace {

struct FoldRowWorker : public Worker {
    const std::vector<const StringView*>& columns;
    const int* order;
    bool na_as_text;
    char* out;
    std::vector<unsigned char>& missing;

    FoldRowWorker(const std::vector<const StringView*>& columns_,
                  const int* order_, bool na_as_text_, char* out_,
                  std::vector<unsigned char>& missing_)
        : columns(columns_), order(order_), na_as_text(na_as_text_),
          out(out_), missing(missing_) {}

    void operator()(std::size_t begin, std::size_t end) {
        const Xxh64Digest digest{0};
        std::string row;
        const bool one = columns.size() == 1;
        for (std::size_t i = begin; i < end; ++i) {
            const std::size_t at =
                order ? static_cast<std::size_t>(order[i]) - 1 : i;
            if (one && !columns[0][at].is_na()) {
                const StringView& s = columns[0][at];
                digest(reinterpret_cast<const unsigned char*>(s.data), s.size,
                       out + i * 16);
                continue;
            }
            row.clear();
            for (std::size_t c = 0; c < columns.size(); ++c) {
                if (c) row.push_back('\x1f');
                const StringView& s = columns[c][at];
                if (s.is_na()) {
                    // Each chunk of rows belongs to one thread, so this is
                    // a write to a byte no other thread touches.
                    if (!na_as_text) missing[i] = 1;
                    row.append("NA");
                } else {
                    row.append(s.data, s.size);
                }
            }
            digest(reinterpret_cast<const unsigned char*>(row.data()),
                   row.size(), out + i * 16);
        }
    }
};

}  // namespace

// [[Rcpp::export]]
CharacterVector fast_fold_digest_impl(const List& columns,
                                      const IntegerVector& order,
                                      bool na_as_text, double chunk_,
                                      int nthreads) {
    const std::size_t n_columns = static_cast<std::size_t>(columns.size());
    if (n_columns == 0) stop("At least one column is needed.");
    // A chunk of one never folds: every pass would leave as many digests as
    // it found.
    if (!(chunk_ >= 2)) stop("`chunk` must be at least 2.");
    const std::size_t chunk = static_cast<std::size_t>(chunk_);
    std::vector<std::unique_ptr<StringSnapshot>> snapshots;
    std::vector<const StringView*> views;
    std::size_t n = 0;
    std::uint64_t bytes = 0;
    for (std::size_t c = 0; c < n_columns; ++c) {
        const StringVector column = as<StringVector>(columns[c]);
        snapshots.emplace_back(
            new StringSnapshot(column, SnapshotText::utf8));
        if (c == 0) n = snapshots.back()->size();
        else if (snapshots.back()->size() != n)
            stop("Every column must have the same length.");
        views.push_back(snapshots.back()->data());
        bytes += snapshots.back()->total_bytes();
    }
    if (n == 0) stop("There are no rows to digest.");
    const bool ordered = order.size() > 0;
    if (ordered) {
        if (static_cast<std::size_t>(order.size()) != n)
            stop("`order` must have one entry per row.");
        for (R_xlen_t i = 0; i < order.size(); ++i)
            if (order[i] == NA_INTEGER || order[i] < 1 ||
                static_cast<std::size_t>(order[i]) > n)
                stop("`order` must hold row numbers.");
    }

    std::vector<char> level(n * 16);
    std::vector<unsigned char> missing(n, 0);
    FoldRowWorker worker(views, ordered ? order.begin() : nullptr, na_as_text,
                         level.data(), missing);
    const std::size_t work = static_cast<std::size_t>(
        bytes + 64u * static_cast<std::uint64_t>(n));
    dispatch_for(0, n, worker, work, 50000, nthreads, 256);
    for (std::size_t i = 0; i < n; ++i)
        if (missing[i]) return CharacterVector::create(NA_STRING);

    const Xxh64Digest digest{0};
    std::size_t count = n;
    while (count > 1) {
        const std::size_t groups = (count + chunk - 1) / chunk;
        std::vector<char> folded(groups * 16);
        for (std::size_t g = 0; g < groups; ++g) {
            const std::size_t first = g * chunk;
            const std::size_t rows = (std::min)(chunk, count - first);
            digest(reinterpret_cast<const unsigned char*>(
                       level.data() + first * 16),
                   rows * 16, folded.data() + g * 16);
        }
        level.swap(folded);
        count = groups;
    }
    return CharacterVector::create(
        Rf_mkCharLenCE(level.data(), 16, CE_UTF8));
}
