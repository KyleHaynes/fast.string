// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "hash_core.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"

#include <cstddef>
#include <cstdint>
#include <memory>
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
