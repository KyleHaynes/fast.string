#ifndef FAST_STRING_HASH_CORE_H
#define FAST_STRING_HASH_CORE_H

// Message digests for fhash(): MD5 (RFC 1321), SHA-1, SHA-256 and SHA-512
// (FIPS 180-4), HMAC over any of them (RFC 2104), and XXH64 (the xxHash
// specification). Written from the specifications; nothing here touches the
// R API, so the workers can run them freely.

#include <cstddef>
#include <cstdint>
#include <cstring>

namespace fasthash {

inline std::uint32_t rotl32(std::uint32_t x, int r) {
    return (x << r) | (x >> (32 - r));
}
inline std::uint32_t rotr32(std::uint32_t x, int r) {
    return (x >> r) | (x << (32 - r));
}
inline std::uint64_t rotl64(std::uint64_t x, int r) {
    return (x << r) | (x >> (64 - r));
}
inline std::uint64_t rotr64(std::uint64_t x, int r) {
    return (x >> r) | (x << (64 - r));
}

inline std::uint32_t load_be32(const unsigned char* p) {
    return (std::uint32_t(p[0]) << 24) | (std::uint32_t(p[1]) << 16) |
        (std::uint32_t(p[2]) << 8) | std::uint32_t(p[3]);
}
inline std::uint32_t load_le32(const unsigned char* p) {
    return std::uint32_t(p[0]) | (std::uint32_t(p[1]) << 8) |
        (std::uint32_t(p[2]) << 16) | (std::uint32_t(p[3]) << 24);
}
inline std::uint64_t load_be64(const unsigned char* p) {
    return (std::uint64_t(load_be32(p)) << 32) | load_be32(p + 4);
}
inline std::uint64_t load_le64(const unsigned char* p) {
    return std::uint64_t(load_le32(p)) | (std::uint64_t(load_le32(p + 4)) << 32);
}
inline void store_be32(unsigned char* p, std::uint32_t v) {
    p[0] = static_cast<unsigned char>(v >> 24);
    p[1] = static_cast<unsigned char>(v >> 16);
    p[2] = static_cast<unsigned char>(v >> 8);
    p[3] = static_cast<unsigned char>(v);
}
inline void store_le32(unsigned char* p, std::uint32_t v) {
    p[0] = static_cast<unsigned char>(v);
    p[1] = static_cast<unsigned char>(v >> 8);
    p[2] = static_cast<unsigned char>(v >> 16);
    p[3] = static_cast<unsigned char>(v >> 24);
}
inline void store_be64(unsigned char* p, std::uint64_t v) {
    store_be32(p, static_cast<std::uint32_t>(v >> 32));
    store_be32(p + 4, static_cast<std::uint32_t>(v));
}

// The Merkle-Damgard buffering shared by the four digests. `Core` supplies
// block_size, digest_size, the state, compress() and the length encoding.
template <typename Core>
class BlockHash : public Core {
public:
    static const std::size_t block_size = Core::block_size;
    static const std::size_t digest_size = Core::digest_size;

    BlockHash() { init(); }

    void init() {
        Core::reset();
        used_ = 0;
        length_ = 0;
    }

    void update(const unsigned char* data, std::size_t size) {
        length_ += size;
        if (used_ != 0) {
            const std::size_t take =
                size < block_size - used_ ? size : block_size - used_;
            std::memcpy(buffer_ + used_, data, take);
            used_ += take;
            data += take;
            size -= take;
            if (used_ < block_size) return;
            Core::compress(buffer_);
            used_ = 0;
        }
        while (size >= block_size) {
            Core::compress(data);
            data += block_size;
            size -= block_size;
        }
        if (size != 0) {
            std::memcpy(buffer_, data, size);
            used_ = size;
        }
    }

    void final(unsigned char* out) {
        const std::size_t length_bytes = Core::length_bytes;
        const std::uint64_t bits = length_ * 8u;
        buffer_[used_++] = 0x80;
        if (used_ > block_size - length_bytes) {
            std::memset(buffer_ + used_, 0, block_size - used_);
            Core::compress(buffer_);
            used_ = 0;
        }
        std::memset(buffer_ + used_, 0, block_size - used_);
        Core::encode_length(buffer_ + block_size - length_bytes, bits);
        Core::compress(buffer_);
        Core::output(out);
    }

private:
    unsigned char buffer_[Core::block_size];
    std::size_t used_ = 0;
    std::uint64_t length_ = 0;
};

// ---------------------------------------------------------------------------
// MD5
// ---------------------------------------------------------------------------

struct Md5Core {
    static const std::size_t block_size = 64;
    static const std::size_t digest_size = 16;
    static const std::size_t length_bytes = 8;
    std::uint32_t h[4];

    void reset() {
        h[0] = 0x67452301u;
        h[1] = 0xEFCDAB89u;
        h[2] = 0x98BADCFEu;
        h[3] = 0x10325476u;
    }

    void compress(const unsigned char* block) {
        static const std::uint32_t K[64] = {
            0xD76AA478u, 0xE8C7B756u, 0x242070DBu, 0xC1BDCEEEu, 0xF57C0FAFu, 0x4787C62Au,
            0xA8304613u, 0xFD469501u, 0x698098D8u, 0x8B44F7AFu, 0xFFFF5BB1u, 0x895CD7BEu,
            0x6B901122u, 0xFD987193u, 0xA679438Eu, 0x49B40821u, 0xF61E2562u, 0xC040B340u,
            0x265E5A51u, 0xE9B6C7AAu, 0xD62F105Du, 0x02441453u, 0xD8A1E681u, 0xE7D3FBC8u,
            0x21E1CDE6u, 0xC33707D6u, 0xF4D50D87u, 0x455A14EDu, 0xA9E3E905u, 0xFCEFA3F8u,
            0x676F02D9u, 0x8D2A4C8Au, 0xFFFA3942u, 0x8771F681u, 0x6D9D6122u, 0xFDE5380Cu,
            0xA4BEEA44u, 0x4BDECFA9u, 0xF6BB4B60u, 0xBEBFBC70u, 0x289B7EC6u, 0xEAA127FAu,
            0xD4EF3085u, 0x04881D05u, 0xD9D4D039u, 0xE6DB99E5u, 0x1FA27CF8u, 0xC4AC5665u,
            0xF4292244u, 0x432AFF97u, 0xAB9423A7u, 0xFC93A039u, 0x655B59C3u, 0x8F0CCC92u,
            0xFFEFF47Du, 0x85845DD1u, 0x6FA87E4Fu, 0xFE2CE6E0u, 0xA3014314u, 0x4E0811A1u,
            0xF7537E82u, 0xBD3AF235u, 0x2AD7D2BBu, 0xEB86D391u,
        };
        static const int R[64] = {
            7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
            5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
            4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
            6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
        };
        std::uint32_t m[16];
        for (int i = 0; i < 16; ++i) m[i] = load_le32(block + 4 * i);
        std::uint32_t a = h[0], b = h[1], c = h[2], d = h[3];
        for (int i = 0; i < 64; ++i) {
            std::uint32_t f;
            int g;
            if (i < 16) {
                f = (b & c) | (~b & d);
                g = i;
            } else if (i < 32) {
                f = (d & b) | (~d & c);
                g = (5 * i + 1) % 16;
            } else if (i < 48) {
                f = b ^ c ^ d;
                g = (3 * i + 5) % 16;
            } else {
                f = c ^ (b | ~d);
                g = (7 * i) % 16;
            }
            const std::uint32_t next = d;
            d = c;
            c = b;
            b = b + rotl32(a + f + K[i] + m[g], R[i]);
            a = next;
        }
        h[0] += a;
        h[1] += b;
        h[2] += c;
        h[3] += d;
    }

    static void encode_length(unsigned char* p, std::uint64_t bits) {
        store_le32(p, static_cast<std::uint32_t>(bits));
        store_le32(p + 4, static_cast<std::uint32_t>(bits >> 32));
    }

    void output(unsigned char* out) const {
        for (int i = 0; i < 4; ++i) store_le32(out + 4 * i, h[i]);
    }
};

// ---------------------------------------------------------------------------
// SHA-1
// ---------------------------------------------------------------------------

struct Sha1Core {
    static const std::size_t block_size = 64;
    static const std::size_t digest_size = 20;
    static const std::size_t length_bytes = 8;
    std::uint32_t h[5];

    void reset() {
        h[0] = 0x67452301u;
        h[1] = 0xEFCDAB89u;
        h[2] = 0x98BADCFEu;
        h[3] = 0x10325476u;
        h[4] = 0xC3D2E1F0u;
    }

    void compress(const unsigned char* block) {
        std::uint32_t w[80];
        for (int t = 0; t < 16; ++t) w[t] = load_be32(block + 4 * t);
        for (int t = 16; t < 80; ++t)
            w[t] = rotl32(w[t - 3] ^ w[t - 8] ^ w[t - 14] ^ w[t - 16], 1);
        std::uint32_t a = h[0], b = h[1], c = h[2], d = h[3], e = h[4];
        for (int t = 0; t < 80; ++t) {
            std::uint32_t f, k;
            if (t < 20) {
                f = (b & c) | (~b & d);
                k = 0x5A827999u;
            } else if (t < 40) {
                f = b ^ c ^ d;
                k = 0x6ED9EBA1u;
            } else if (t < 60) {
                f = (b & c) | (b & d) | (c & d);
                k = 0x8F1BBCDCu;
            } else {
                f = b ^ c ^ d;
                k = 0xCA62C1D6u;
            }
            const std::uint32_t temp = rotl32(a, 5) + f + e + k + w[t];
            e = d;
            d = c;
            c = rotl32(b, 30);
            b = a;
            a = temp;
        }
        h[0] += a;
        h[1] += b;
        h[2] += c;
        h[3] += d;
        h[4] += e;
    }

    static void encode_length(unsigned char* p, std::uint64_t bits) {
        store_be64(p, bits);
    }

    void output(unsigned char* out) const {
        for (int i = 0; i < 5; ++i) store_be32(out + 4 * i, h[i]);
    }
};

// ---------------------------------------------------------------------------
// SHA-256
// ---------------------------------------------------------------------------

struct Sha256Core {
    static const std::size_t block_size = 64;
    static const std::size_t digest_size = 32;
    static const std::size_t length_bytes = 8;
    std::uint32_t h[8];

    void reset() {
        static const std::uint32_t H[8] = {
            0x6A09E667u, 0xBB67AE85u, 0x3C6EF372u, 0xA54FF53Au,
            0x510E527Fu, 0x9B05688Cu, 0x1F83D9ABu, 0x5BE0CD19u,
        };
        std::memcpy(h, H, sizeof(h));
    }

    void compress(const unsigned char* block) {
        static const std::uint32_t K[64] = {
            0x428A2F98u, 0x71374491u, 0xB5C0FBCFu, 0xE9B5DBA5u, 0x3956C25Bu, 0x59F111F1u,
            0x923F82A4u, 0xAB1C5ED5u, 0xD807AA98u, 0x12835B01u, 0x243185BEu, 0x550C7DC3u,
            0x72BE5D74u, 0x80DEB1FEu, 0x9BDC06A7u, 0xC19BF174u, 0xE49B69C1u, 0xEFBE4786u,
            0x0FC19DC6u, 0x240CA1CCu, 0x2DE92C6Fu, 0x4A7484AAu, 0x5CB0A9DCu, 0x76F988DAu,
            0x983E5152u, 0xA831C66Du, 0xB00327C8u, 0xBF597FC7u, 0xC6E00BF3u, 0xD5A79147u,
            0x06CA6351u, 0x14292967u, 0x27B70A85u, 0x2E1B2138u, 0x4D2C6DFCu, 0x53380D13u,
            0x650A7354u, 0x766A0ABBu, 0x81C2C92Eu, 0x92722C85u, 0xA2BFE8A1u, 0xA81A664Bu,
            0xC24B8B70u, 0xC76C51A3u, 0xD192E819u, 0xD6990624u, 0xF40E3585u, 0x106AA070u,
            0x19A4C116u, 0x1E376C08u, 0x2748774Cu, 0x34B0BCB5u, 0x391C0CB3u, 0x4ED8AA4Au,
            0x5B9CCA4Fu, 0x682E6FF3u, 0x748F82EEu, 0x78A5636Fu, 0x84C87814u, 0x8CC70208u,
            0x90BEFFFAu, 0xA4506CEBu, 0xBEF9A3F7u, 0xC67178F2u,
        };
        std::uint32_t w[64];
        for (int t = 0; t < 16; ++t) w[t] = load_be32(block + 4 * t);
        for (int t = 16; t < 64; ++t) {
            const std::uint32_t s0 =
                rotr32(w[t - 15], 7) ^ rotr32(w[t - 15], 18) ^ (w[t - 15] >> 3);
            const std::uint32_t s1 =
                rotr32(w[t - 2], 17) ^ rotr32(w[t - 2], 19) ^ (w[t - 2] >> 10);
            w[t] = w[t - 16] + s0 + w[t - 7] + s1;
        }
        std::uint32_t a = h[0], b = h[1], c = h[2], d = h[3];
        std::uint32_t e = h[4], f = h[5], g = h[6], hh = h[7];
        for (int t = 0; t < 64; ++t) {
            const std::uint32_t S1 = rotr32(e, 6) ^ rotr32(e, 11) ^ rotr32(e, 25);
            const std::uint32_t ch = (e & f) ^ (~e & g);
            const std::uint32_t t1 = hh + S1 + ch + K[t] + w[t];
            const std::uint32_t S0 = rotr32(a, 2) ^ rotr32(a, 13) ^ rotr32(a, 22);
            const std::uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
            const std::uint32_t t2 = S0 + maj;
            hh = g;
            g = f;
            f = e;
            e = d + t1;
            d = c;
            c = b;
            b = a;
            a = t1 + t2;
        }
        h[0] += a;
        h[1] += b;
        h[2] += c;
        h[3] += d;
        h[4] += e;
        h[5] += f;
        h[6] += g;
        h[7] += hh;
    }

    static void encode_length(unsigned char* p, std::uint64_t bits) {
        store_be64(p, bits);
    }

    void output(unsigned char* out) const {
        for (int i = 0; i < 8; ++i) store_be32(out + 4 * i, h[i]);
    }
};

// ---------------------------------------------------------------------------
// SHA-512
// ---------------------------------------------------------------------------

struct Sha512Core {
    static const std::size_t block_size = 128;
    static const std::size_t digest_size = 64;
    static const std::size_t length_bytes = 16;
    std::uint64_t h[8];

    void reset() {
        static const std::uint64_t H[8] = {
            0x6A09E667F3BCC908ULL, 0xBB67AE8584CAA73BULL, 0x3C6EF372FE94F82BULL,
            0xA54FF53A5F1D36F1ULL, 0x510E527FADE682D1ULL, 0x9B05688C2B3E6C1FULL,
            0x1F83D9ABFB41BD6BULL, 0x5BE0CD19137E2179ULL,
        };
        std::memcpy(h, H, sizeof(h));
    }

    void compress(const unsigned char* block) {
        static const std::uint64_t K[80] = {
            0x428A2F98D728AE22ULL, 0x7137449123EF65CDULL, 0xB5C0FBCFEC4D3B2FULL,
            0xE9B5DBA58189DBBCULL, 0x3956C25BF348B538ULL, 0x59F111F1B605D019ULL,
            0x923F82A4AF194F9BULL, 0xAB1C5ED5DA6D8118ULL, 0xD807AA98A3030242ULL,
            0x12835B0145706FBEULL, 0x243185BE4EE4B28CULL, 0x550C7DC3D5FFB4E2ULL,
            0x72BE5D74F27B896FULL, 0x80DEB1FE3B1696B1ULL, 0x9BDC06A725C71235ULL,
            0xC19BF174CF692694ULL, 0xE49B69C19EF14AD2ULL, 0xEFBE4786384F25E3ULL,
            0x0FC19DC68B8CD5B5ULL, 0x240CA1CC77AC9C65ULL, 0x2DE92C6F592B0275ULL,
            0x4A7484AA6EA6E483ULL, 0x5CB0A9DCBD41FBD4ULL, 0x76F988DA831153B5ULL,
            0x983E5152EE66DFABULL, 0xA831C66D2DB43210ULL, 0xB00327C898FB213FULL,
            0xBF597FC7BEEF0EE4ULL, 0xC6E00BF33DA88FC2ULL, 0xD5A79147930AA725ULL,
            0x06CA6351E003826FULL, 0x142929670A0E6E70ULL, 0x27B70A8546D22FFCULL,
            0x2E1B21385C26C926ULL, 0x4D2C6DFC5AC42AEDULL, 0x53380D139D95B3DFULL,
            0x650A73548BAF63DEULL, 0x766A0ABB3C77B2A8ULL, 0x81C2C92E47EDAEE6ULL,
            0x92722C851482353BULL, 0xA2BFE8A14CF10364ULL, 0xA81A664BBC423001ULL,
            0xC24B8B70D0F89791ULL, 0xC76C51A30654BE30ULL, 0xD192E819D6EF5218ULL,
            0xD69906245565A910ULL, 0xF40E35855771202AULL, 0x106AA07032BBD1B8ULL,
            0x19A4C116B8D2D0C8ULL, 0x1E376C085141AB53ULL, 0x2748774CDF8EEB99ULL,
            0x34B0BCB5E19B48A8ULL, 0x391C0CB3C5C95A63ULL, 0x4ED8AA4AE3418ACBULL,
            0x5B9CCA4F7763E373ULL, 0x682E6FF3D6B2B8A3ULL, 0x748F82EE5DEFB2FCULL,
            0x78A5636F43172F60ULL, 0x84C87814A1F0AB72ULL, 0x8CC702081A6439ECULL,
            0x90BEFFFA23631E28ULL, 0xA4506CEBDE82BDE9ULL, 0xBEF9A3F7B2C67915ULL,
            0xC67178F2E372532BULL, 0xCA273ECEEA26619CULL, 0xD186B8C721C0C207ULL,
            0xEADA7DD6CDE0EB1EULL, 0xF57D4F7FEE6ED178ULL, 0x06F067AA72176FBAULL,
            0x0A637DC5A2C898A6ULL, 0x113F9804BEF90DAEULL, 0x1B710B35131C471BULL,
            0x28DB77F523047D84ULL, 0x32CAAB7B40C72493ULL, 0x3C9EBE0A15C9BEBCULL,
            0x431D67C49C100D4CULL, 0x4CC5D4BECB3E42B6ULL, 0x597F299CFC657E2AULL,
            0x5FCB6FAB3AD6FAECULL, 0x6C44198C4A475817ULL,
        };
        std::uint64_t w[80];
        for (int t = 0; t < 16; ++t) w[t] = load_be64(block + 8 * t);
        for (int t = 16; t < 80; ++t) {
            const std::uint64_t s0 =
                rotr64(w[t - 15], 1) ^ rotr64(w[t - 15], 8) ^ (w[t - 15] >> 7);
            const std::uint64_t s1 =
                rotr64(w[t - 2], 19) ^ rotr64(w[t - 2], 61) ^ (w[t - 2] >> 6);
            w[t] = w[t - 16] + s0 + w[t - 7] + s1;
        }
        std::uint64_t a = h[0], b = h[1], c = h[2], d = h[3];
        std::uint64_t e = h[4], f = h[5], g = h[6], hh = h[7];
        for (int t = 0; t < 80; ++t) {
            const std::uint64_t S1 = rotr64(e, 14) ^ rotr64(e, 18) ^ rotr64(e, 41);
            const std::uint64_t ch = (e & f) ^ (~e & g);
            const std::uint64_t t1 = hh + S1 + ch + K[t] + w[t];
            const std::uint64_t S0 = rotr64(a, 28) ^ rotr64(a, 34) ^ rotr64(a, 39);
            const std::uint64_t maj = (a & b) ^ (a & c) ^ (b & c);
            const std::uint64_t t2 = S0 + maj;
            hh = g;
            g = f;
            f = e;
            e = d + t1;
            d = c;
            c = b;
            b = a;
            a = t1 + t2;
        }
        h[0] += a;
        h[1] += b;
        h[2] += c;
        h[3] += d;
        h[4] += e;
        h[5] += f;
        h[6] += g;
        h[7] += hh;
    }

    // A 128-bit big-endian bit count; messages here are far below 2^64 bits.
    static void encode_length(unsigned char* p, std::uint64_t bits) {
        store_be64(p, 0);
        store_be64(p + 8, bits);
    }

    void output(unsigned char* out) const {
        for (int i = 0; i < 8; ++i) store_be64(out + 8 * i, h[i]);
    }
};

typedef BlockHash<Md5Core> Md5;
typedef BlockHash<Sha1Core> Sha1;
typedef BlockHash<Sha256Core> Sha256;
typedef BlockHash<Sha512Core> Sha512;

// HMAC (RFC 2104). The key's inner and outer blocks are absorbed once; each
// message then starts from copies of those two states.
template <typename Hash>
class Hmac {
public:
    static const std::size_t digest_size = Hash::digest_size;

    Hmac(const unsigned char* key, std::size_t size) {
        unsigned char block[Hash::block_size];
        std::memset(block, 0, sizeof(block));
        if (size > Hash::block_size) {
            Hash digest;
            digest.update(key, size);
            digest.final(block);
        } else if (size != 0) {
            std::memcpy(block, key, size);
        }
        unsigned char pad[Hash::block_size];
        for (std::size_t i = 0; i < Hash::block_size; ++i)
            pad[i] = static_cast<unsigned char>(block[i] ^ 0x36);
        inner_.update(pad, sizeof(pad));
        for (std::size_t i = 0; i < Hash::block_size; ++i)
            pad[i] = static_cast<unsigned char>(block[i] ^ 0x5C);
        outer_.update(pad, sizeof(pad));
    }

    void mac(const unsigned char* data, std::size_t size,
             unsigned char* out) const {
        Hash inner = inner_;
        inner.update(data, size);
        unsigned char digest[Hash::digest_size];
        inner.final(digest);
        Hash outer = outer_;
        outer.update(digest, sizeof(digest));
        outer.final(out);
    }

private:
    Hash inner_;
    Hash outer_;
};

// ---------------------------------------------------------------------------
// XXH64
// ---------------------------------------------------------------------------

const std::uint64_t XXH_PRIME64_1 = 0x9E3779B185EBCA87ULL;
const std::uint64_t XXH_PRIME64_2 = 0xC2B2AE3D27D4EB4FULL;
const std::uint64_t XXH_PRIME64_3 = 0x165667B19E3779F9ULL;
const std::uint64_t XXH_PRIME64_4 = 0x85EBCA77C2B2AE63ULL;
const std::uint64_t XXH_PRIME64_5 = 0x27D4EB2F165667C5ULL;

inline std::uint64_t xxh64_round(std::uint64_t acc, std::uint64_t input) {
    acc += input * XXH_PRIME64_2;
    acc = rotl64(acc, 31);
    return acc * XXH_PRIME64_1;
}

inline std::uint64_t xxh64_merge(std::uint64_t acc, std::uint64_t value) {
    acc ^= xxh64_round(0, value);
    return acc * XXH_PRIME64_1 + XXH_PRIME64_4;
}

inline std::uint64_t xxh64(const unsigned char* p, std::size_t size,
                           std::uint64_t seed) {
    const unsigned char* const end = p + size;
    std::uint64_t h;
    if (size >= 32) {
        std::uint64_t v1 = seed + XXH_PRIME64_1 + XXH_PRIME64_2;
        std::uint64_t v2 = seed + XXH_PRIME64_2;
        std::uint64_t v3 = seed;
        std::uint64_t v4 = seed - XXH_PRIME64_1;
        const unsigned char* const limit = end - 32;
        do {
            v1 = xxh64_round(v1, load_le64(p));
            v2 = xxh64_round(v2, load_le64(p + 8));
            v3 = xxh64_round(v3, load_le64(p + 16));
            v4 = xxh64_round(v4, load_le64(p + 24));
            p += 32;
        } while (p <= limit);
        h = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + rotl64(v4, 18);
        h = xxh64_merge(h, v1);
        h = xxh64_merge(h, v2);
        h = xxh64_merge(h, v3);
        h = xxh64_merge(h, v4);
    } else {
        h = seed + XXH_PRIME64_5;
    }
    h += static_cast<std::uint64_t>(size);
    while (p + 8 <= end) {
        h ^= xxh64_round(0, load_le64(p));
        h = rotl64(h, 27) * XXH_PRIME64_1 + XXH_PRIME64_4;
        p += 8;
    }
    if (p + 4 <= end) {
        h ^= static_cast<std::uint64_t>(load_le32(p)) * XXH_PRIME64_1;
        h = rotl64(h, 23) * XXH_PRIME64_2 + XXH_PRIME64_3;
        p += 4;
    }
    while (p < end) {
        h ^= static_cast<std::uint64_t>(*p) * XXH_PRIME64_5;
        h = rotl64(h, 11) * XXH_PRIME64_1;
        ++p;
    }
    h ^= h >> 33;
    h *= XXH_PRIME64_2;
    h ^= h >> 29;
    h *= XXH_PRIME64_3;
    h ^= h >> 32;
    return h;
}

}  // namespace fasthash

#endif // FAST_STRING_HASH_CORE_H
