// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "parallel_dispatch.h"
#include "rapidfuzz_core.h"
#include "string_snapshot.h"
#include "utf8_text.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <utility>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// fagrepl(): does x contain a substring within `k` Levenshtein edits of a
// literal pattern? This is base R's agrepl(fixed = TRUE) with unit costs.
// Myers' bit-parallel algorithm (1999) in its search form: the DP's top row
// is all zeros, so a match may start anywhere, and the score is the best
// distance of the pattern to a substring ending at the current position.

namespace {

typedef std::uint32_t cp_t;

inline cp_t fold_case(cp_t c, bool ignore_case) {
    if (!ignore_case) return c;
    if (c < 0x80) return c >= 'A' && c <= 'Z' ? c + ('a' - 'A') : c;
    if (c >= 0x110000u) return c;
    // Lower-case letters as Python's str.lower() does (rapidfuzz_unicode.h);
    // other characters stay as they are.
    const rf_char folded = rf_default_process_char(c);
    return folded == 0x20 ? c : folded;
}

// Reads the next character (code point, or byte) of a text.
struct TextCursor {
    const unsigned char* s;
    std::size_t n;
    std::size_t i;
    bool utf8;

    bool next(cp_t& c) {
        if (i >= n) return false;
        if (!utf8 || s[i] < 0x80) {
            c = s[i++];
            return true;
        }
        const std::size_t w = utf8_decode(s, n, i, c);
        if (w == 0) {
            c = 0x110000u + s[i];
            ++i;
        } else {
            i += w;
        }
        return true;
    }
};

class ApproxPattern {
public:
    ApproxPattern(const std::vector<cp_t>& pattern, bool ignore_case)
        : m_(pattern.size()), ignore_case_(ignore_case), pattern_(pattern) {
        for (cp_t& c : pattern_) c = fold_case(c, ignore_case_);
        std::fill(ascii_, ascii_ + 128, 0);
        if (m_ <= 64) {
            for (std::size_t i = 0; i < m_; ++i) {
                const cp_t c = pattern_[i];
                const std::uint64_t bit = std::uint64_t(1) << i;
                if (c < 128) {
                    ascii_[c] |= bit;
                } else {
                    auto it = std::lower_bound(
                        wide_.begin(), wide_.end(), c,
                        [](const std::pair<cp_t, std::uint64_t>& e, cp_t v) {
                            return e.first < v;
                        });
                    if (it != wide_.end() && it->first == c) it->second |= bit;
                    else wide_.insert(it, std::make_pair(c, bit));
                }
            }
        }
    }

    std::size_t length() const { return m_; }

    // Whether some substring of the text is within k edits of the pattern.
    bool matches(TextCursor text, std::size_t k, std::vector<std::size_t>& column) const {
        if (m_ <= k) return true;  // the empty substring is close enough
        return m_ <= 64 ? myers(text, k) : dynamic(text, k, column);
    }

private:
    std::size_t m_;
    bool ignore_case_;
    std::vector<cp_t> pattern_;
    std::uint64_t ascii_[128];
    std::vector<std::pair<cp_t, std::uint64_t>> wide_;

    std::uint64_t eq(cp_t c) const {
        if (c < 128) return ascii_[c];
        auto it = std::lower_bound(
            wide_.begin(), wide_.end(), c,
            [](const std::pair<cp_t, std::uint64_t>& e, cp_t v) { return e.first < v; });
        return it != wide_.end() && it->first == c ? it->second : 0;
    }

    bool myers(TextCursor text, std::size_t k) const {
        const std::uint64_t high = std::uint64_t(1) << (m_ - 1);
        std::uint64_t pv = m_ == 64 ? ~std::uint64_t(0) : (high << 1) - 1;
        std::uint64_t mv = 0;
        std::size_t score = m_;
        cp_t c;
        while (text.next(c)) {
            const std::uint64_t e = eq(fold_case(c, ignore_case_));
            const std::uint64_t xv = e | mv;
            const std::uint64_t xh = (((e & pv) + pv) ^ pv) | e;
            std::uint64_t ph = mv | ~(xh | pv);
            std::uint64_t mh = pv & xh;
            if (ph & high) ++score;
            else if (mh & high) --score;
            ph <<= 1;  // the top row is zeros: no carry into the first bit
            mh <<= 1;
            pv = mh | ~(xv | ph);
            mv = ph & xv;
            if (score <= k) return true;
        }
        return false;
    }

    // Plain semi-global DP for patterns longer than 64 characters.
    bool dynamic(TextCursor text, std::size_t k, std::vector<std::size_t>& column) const {
        column.resize(m_ + 1);
        for (std::size_t i = 0; i <= m_; ++i) column[i] = i;
        cp_t c;
        while (text.next(c)) {
            const cp_t t = fold_case(c, ignore_case_);
            std::size_t diagonal = column[0];  // top row stays 0
            for (std::size_t i = 1; i <= m_; ++i) {
                const std::size_t above = column[i];
                column[i] = (std::min)((std::min)(above, column[i - 1]) + 1,
                                       diagonal + (pattern_[i - 1] != t));
                diagonal = above;
            }
            if (column[m_] <= k) return true;
        }
        return false;
    }
};

struct ApproxWorker : public Worker {
    const StringView* strings;
    const ApproxPattern& pattern;
    std::size_t k;
    bool utf8;
    RVector<int> out;

    ApproxWorker(const StringView* strings_, const ApproxPattern& pattern_,
                 std::size_t k_, bool utf8_, LogicalVector& out_)
        : strings(strings_), pattern(pattern_), k(k_), utf8(utf8_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<std::size_t> column;
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) {
                out[i] = NA_LOGICAL;
                continue;
            }
            TextCursor text{reinterpret_cast<const unsigned char*>(value.data),
                            value.size, 0, utf8};
            out[i] = pattern.matches(text, k, column) ? 1 : 0;
        }
    }
};

}  // namespace

// [[Rcpp::export]]
LogicalVector fast_agrepl_impl(const std::string& pattern, const StringVector& x,
                               double max_distance, bool ignore_case,
                               bool use_bytes, int nthreads) {
    StringSnapshot snapshot(x, use_bytes ? SnapshotText::raw : SnapshotText::utf8);
    const bool utf8 = !use_bytes && !snapshot.any_bytes();
    std::vector<cp_t> chars;
    TextCursor cursor{reinterpret_cast<const unsigned char*>(pattern.data()),
                      pattern.size(), 0, utf8};
    cp_t c;
    while (cursor.next(c)) chars.push_back(c);
    if (chars.empty()) stop("'pattern' must be a non-empty character string");
    // As base R's agrep(): a bound below 1 is a fraction of the pattern
    // length, rounded up.
    const double bound = max_distance < 1
        ? max_distance * static_cast<double>(chars.size())
        : max_distance;
    const double limit = std::ceil(bound);
    const std::size_t k = limit >= 1e18 ? std::numeric_limits<std::size_t>::max() / 2
                                        : static_cast<std::size_t>(limit);
    const ApproxPattern compiled(chars, ignore_case);
    LogicalVector out = no_init(x.size());
    ApproxWorker worker(snapshot.data(), compiled, k, utf8, out);
    const std::uint64_t work = snapshot.total_bytes() *
        static_cast<std::uint64_t>((chars.size() + 63) / 64);
    dispatch_for(0, snapshot.size(), worker,
                 static_cast<std::size_t>((std::min)(
                     work, static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)()))),
                 100000, nthreads);
    return out;
}
