#ifndef FAST_STRING_STRING_SNAPSHOT_H
#define FAST_STRING_STRING_SNAPSHOT_H

#include <Rcpp.h>
#include <algorithm>
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

inline std::size_t snapshot_bytes_as_size(const StringSnapshot& snapshot) {
    const std::uint64_t limit = static_cast<std::uint64_t>(
        (std::numeric_limits<std::size_t>::max)()
    );
    return static_cast<std::size_t>((std::min)(snapshot.total_bytes(), limit));
}

inline std::size_t saturating_string_work_add(std::size_t left,
                                              std::size_t right) {
    const std::size_t limit = (std::numeric_limits<std::size_t>::max)();
    return right > limit - left ? limit : left + right;
}

inline std::size_t saturating_string_work_multiply(std::size_t left,
                                                   std::size_t right) {
    const std::size_t limit = (std::numeric_limits<std::size_t>::max)();
    return left != 0 && right > limit / left ? limit : left * right;
}

inline std::size_t string_byte_units(std::size_t bytes,
                                     std::size_t bytes_per_unit = 64) {
    if (bytes == 0) return 0;
    return bytes / bytes_per_unit + (bytes % bytes_per_unit != 0);
}

// Keep the historical row/cell crossovers for ordinary short strings while
// allowing long-string calls to parallelise based on the byte work actually
// performed. Matrix byte work accounts for reuse across every opposing row.
inline std::size_t estimated_pairwise_string_work(
        const StringSnapshot& a,
        const StringSnapshot& b) {
    const std::size_t bytes = saturating_string_work_add(
        snapshot_bytes_as_size(a), snapshot_bytes_as_size(b)
    );
    return (std::max)(
        (std::max)(a.size(), b.size()),
        string_byte_units(bytes)
    );
}

inline std::size_t estimated_matrix_string_work(
        const StringSnapshot& a,
        const StringSnapshot& b,
        std::size_t cells) {
    const std::size_t reused_a = saturating_string_work_multiply(
        snapshot_bytes_as_size(a), b.size()
    );
    const std::size_t reused_b = saturating_string_work_multiply(
        snapshot_bytes_as_size(b), a.size()
    );
    const std::size_t reused_bytes =
        saturating_string_work_add(reused_a, reused_b);
    return (std::max)(cells, string_byte_units(reused_bytes));
}

#endif // FAST_STRING_STRING_SNAPSHOT_H
