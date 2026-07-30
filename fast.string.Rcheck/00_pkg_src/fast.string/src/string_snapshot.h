#ifndef FAST_STRING_STRING_SNAPSHOT_H
#define FAST_STRING_STRING_SNAPSHOT_H

#include <Rcpp.h>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

// Immutable byte view consumed by worker threads. A null data pointer is
// reserved for NA; empty non-NA strings retain their ordinary CHAR() pointer.
struct StringView {
    const char* data;
    std::size_t size;

    bool is_na() const noexcept { return data == nullptr; }
};

// Materialise every R string and resolve CHAR()/LENGTH() on the main thread.
// ALTREP strings are copied into an ordinary STRSXP pointer array so generated
// CHARSXPs remain rooted for the complete synchronous parallel operation.
class StringSnapshot {
public:
    explicit StringSnapshot(const Rcpp::StringVector& input)
        : owner_(input),
          atoms_(nullptr),
          views_(static_cast<std::size_t>(input.size())),
          total_bytes_(0) {
        if (ALTREP(static_cast<SEXP>(input))) {
            Rcpp::CharacterVector materialized(input.size());
            for (R_xlen_t i = 0; i < input.size(); ++i)
                SET_STRING_ELT(materialized, i, STRING_ELT(input, i));
            owner_ = materialized;
        }

        // Resolve the ordinary STRSXP pointer array once on the main thread;
        // repeated STRING_ELT calls are measurable for short-string kernels.
        atoms_ = static_cast<const SEXP*>(DATAPTR_RO(owner_));
        const std::uint64_t max_bytes =
            (std::numeric_limits<std::uint64_t>::max)();
        for (R_xlen_t i = 0; i < owner_.size(); ++i) {
            SEXP elem = atoms_[i];
            StringView& view = views_[static_cast<std::size_t>(i)];
            if (elem == NA_STRING) {
                view = StringView{nullptr, 0};
                continue;
            }

            const std::size_t size = static_cast<std::size_t>(LENGTH(elem));
            view = StringView{CHAR(elem), size};
            if (max_bytes - total_bytes_ < size)
                total_bytes_ = max_bytes;
            else
                total_bytes_ += static_cast<std::uint64_t>(size);
        }
    }

    const StringView& operator[](std::size_t i) const noexcept {
        return views_[i];
    }

    // Main-thread-only access for result finalisation paths that can reuse an
    // unchanged CHARSXP. Never call this accessor from Worker::operator().
    SEXP charsxp(std::size_t i) const {
        return atoms_[i];
    }

    const StringView* data() const noexcept { return views_.data(); }
    std::size_t size() const noexcept { return views_.size(); }
    std::uint64_t total_bytes() const noexcept { return total_bytes_; }

private:
    Rcpp::CharacterVector owner_;
    const SEXP* atoms_;
    std::vector<StringView> views_;
    std::uint64_t total_bytes_;
};

#endif // FAST_STRING_STRING_SNAPSHOT_H
