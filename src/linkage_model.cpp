#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <numeric>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace Rcpp;

// The Fellegi-Sunter side of record linkage: collapse scored pairs into
// agreement patterns, fit the two-class latent model by EM, turn the fitted
// probabilities into match weights, resolve the pairs into links, and score
// the result against a known truth.

namespace {

// A missing comparison is stored as the level just past the last real one, so
// that one integer code covers every case and the EM step can recognise it.
constexpr int PATTERN_NA = -1;

struct PatternKeyHash {
    const std::vector<int>* codes;
    std::size_t width;

    std::size_t operator()(std::size_t row) const {
        // FNV-1a over the row's codes.
        std::uint64_t h = 1469598103934665603ULL;
        const int* p = codes->data() + row * width;
        for (std::size_t k = 0; k < width; ++k) {
            std::uint64_t v = static_cast<std::uint64_t>(
                static_cast<std::int64_t>(p[k]));
            for (int byte = 0; byte < 4; ++byte) {
                h ^= (v >> (8 * byte)) & 0xFF;
                h *= 1099511628211ULL;
            }
        }
        return static_cast<std::size_t>(h);
    }
};

struct PatternKeyEqual {
    const std::vector<int>* codes;
    std::size_t width;

    bool operator()(std::size_t x, std::size_t y) const {
        const int* a = codes->data() + x * width;
        const int* b = codes->data() + y * width;
        for (std::size_t k = 0; k < width; ++k)
            if (a[k] != b[k]) return false;
        return true;
    }
};

inline std::uint64_t pair_key(int i, int j) {
    return (static_cast<std::uint64_t>(static_cast<std::uint32_t>(i)) << 32) |
           static_cast<std::uint32_t>(j);
}

}  // namespace

// Distinct agreement patterns, their counts, and which pattern each pair has.
//
// `levels` is one integer vector per comparison, holding a level code from 0
// up or NA. Patterns are numbered in the order they are first seen.
//
// [[Rcpp::export]]
List fast_agreement_patterns_impl(List levels) {
    const std::size_t width = static_cast<std::size_t>(levels.size());
    if (width == 0) stop("At least one comparison is needed.");
    std::vector<IntegerVector> columns;
    columns.reserve(width);
    R_xlen_t n = -1;
    for (std::size_t k = 0; k < width; ++k) {
        IntegerVector column = levels[k];
        if (n < 0) n = column.size();
        else if (column.size() != n)
            stop("Every comparison must have the same number of pairs.");
        columns.push_back(column);
    }
    const std::size_t rows = static_cast<std::size_t>(n);

    // Distinct patterns are appended here; the row just past the last one is
    // scratch, holding the pattern currently being looked up.
    std::vector<int> codes(width, 0);

    PatternKeyHash hash{&codes, width};
    PatternKeyEqual equal{&codes, width};
    std::unordered_map<std::size_t, std::size_t, PatternKeyHash, PatternKeyEqual>
        seen(16, hash, equal);

    IntegerVector index = no_init(n);
    std::vector<double> counts;
    std::size_t distinct = 0;

    for (std::size_t r = 0; r < rows; ++r) {
        // Append the candidate at the end, so the map's hash and equality can
        // reach it by row number like any other.
        if (codes.size() < (distinct + 1) * width)
            codes.resize((distinct + 1) * width);
        int* slot = codes.data() + distinct * width;
        for (std::size_t k = 0; k < width; ++k) {
            const int value = columns[k][r];
            slot[k] = value == NA_INTEGER ? PATTERN_NA : value;
        }
        auto found = seen.find(distinct);
        if (found == seen.end()) {
            seen.insert(std::make_pair(distinct, distinct));
            counts.push_back(1.0);
            index[r] = static_cast<int>(distinct) + 1;
            ++distinct;
        } else {
            counts[found->second] += 1.0;
            index[r] = static_cast<int>(found->second) + 1;
        }
    }

    IntegerMatrix patterns(static_cast<int>(distinct), static_cast<int>(width));
    for (std::size_t p = 0; p < distinct; ++p)
        for (std::size_t k = 0; k < width; ++k)
            patterns(static_cast<int>(p), static_cast<int>(k)) =
                codes[p * width + k] == PATTERN_NA ? NA_INTEGER
                                                   : codes[p * width + k];

    return List::create(_["patterns"] = patterns, _["n"] = wrap(counts),
                        _["index"] = index);
}

namespace {

// Log of the probability of one pattern under one class, skipping the
// comparisons the pair has no value for.
inline double pattern_loglik(const int* pattern, std::size_t width,
                             const std::vector<double>& probs,
                             const std::vector<std::size_t>& offsets) {
    double total = 0.0;
    for (std::size_t k = 0; k < width; ++k) {
        const int level = pattern[k];
        if (level == PATTERN_NA) continue;
        total += std::log(probs[offsets[k] + static_cast<std::size_t>(level)]);
    }
    return total;
}

}  // namespace

// EM for the Fellegi-Sunter model: two latent classes (match and non-match)
// whose comparisons are independent within each class.
//
// `patterns` has one row per distinct agreement pattern and one column per
// comparison; `counts` says how many pairs have each. `m` and `u` arrive and
// leave flattened, comparison by comparison, with `n_levels` giving the
// width of each comparison's block.
//
// [[Rcpp::export]]
List fast_em_linkage_impl(IntegerMatrix patterns, NumericVector counts,
                          IntegerVector n_levels, NumericVector m_init,
                          NumericVector u_init, double lambda, int max_iter,
                          double tol, double floor_prob) {
    const std::size_t rows = static_cast<std::size_t>(patterns.nrow());
    const std::size_t width = static_cast<std::size_t>(patterns.ncol());
    if (static_cast<std::size_t>(n_levels.size()) != width)
        stop("`n_levels` must have one entry per comparison.");
    if (static_cast<std::size_t>(counts.size()) != rows)
        stop("`counts` must have one entry per pattern.");

    std::vector<std::size_t> offsets(width);
    std::size_t total_levels = 0;
    for (std::size_t k = 0; k < width; ++k) {
        offsets[k] = total_levels;
        total_levels += static_cast<std::size_t>(n_levels[k]);
    }
    if (static_cast<std::size_t>(m_init.size()) != total_levels ||
        static_cast<std::size_t>(u_init.size()) != total_levels)
        stop("`m_init` and `u_init` must hold one probability per level.");

    std::vector<double> m(m_init.begin(), m_init.end());
    std::vector<double> u(u_init.begin(), u_init.end());
    std::vector<int> flat(rows * width);
    for (std::size_t r = 0; r < rows; ++r)
        for (std::size_t k = 0; k < width; ++k) {
            const int value = patterns(static_cast<int>(r), static_cast<int>(k));
            flat[r * width + k] = value == NA_INTEGER ? PATTERN_NA : value;
        }

    std::vector<double> posterior(rows, 0.0);
    std::vector<double> m_next(total_levels), u_next(total_levels);
    std::vector<double> m_total(width), u_total(width);

    double total_count = 0.0;
    for (std::size_t r = 0; r < rows; ++r) total_count += counts[r];
    if (!(total_count > 0)) stop("There are no pairs to fit.");

    if (lambda < floor_prob) lambda = floor_prob;
    if (lambda > 1.0 - floor_prob) lambda = 1.0 - floor_prob;

    double loglik = R_NegInf;
    int iteration = 0;
    bool converged = false;

    for (; iteration < max_iter; ++iteration) {
        // E step.
        double new_loglik = 0.0;
        for (std::size_t r = 0; r < rows; ++r) {
            const int* pattern = flat.data() + r * width;
            const double log_match =
                std::log(lambda) + pattern_loglik(pattern, width, m, offsets);
            const double log_other = std::log(1.0 - lambda) +
                                     pattern_loglik(pattern, width, u, offsets);
            const double top = (std::max)(log_match, log_other);
            const double mix =
                top + std::log(std::exp(log_match - top) + std::exp(log_other - top));
            posterior[r] = std::exp(log_match - mix);
            new_loglik += counts[r] * mix;
        }

        // M step.
        std::fill(m_next.begin(), m_next.end(), 0.0);
        std::fill(u_next.begin(), u_next.end(), 0.0);
        std::fill(m_total.begin(), m_total.end(), 0.0);
        std::fill(u_total.begin(), u_total.end(), 0.0);
        double matched = 0.0;
        for (std::size_t r = 0; r < rows; ++r) {
            const double weight_match = counts[r] * posterior[r];
            const double weight_other = counts[r] * (1.0 - posterior[r]);
            matched += weight_match;
            const int* pattern = flat.data() + r * width;
            for (std::size_t k = 0; k < width; ++k) {
                const int level = pattern[k];
                if (level == PATTERN_NA) continue;
                const std::size_t at = offsets[k] + static_cast<std::size_t>(level);
                m_next[at] += weight_match;
                u_next[at] += weight_other;
                m_total[k] += weight_match;
                u_total[k] += weight_other;
            }
        }

        double shift = std::fabs(matched / total_count - lambda);
        lambda = matched / total_count;
        if (lambda < floor_prob) lambda = floor_prob;
        if (lambda > 1.0 - floor_prob) lambda = 1.0 - floor_prob;

        for (std::size_t k = 0; k < width; ++k) {
            const std::size_t levels = static_cast<std::size_t>(n_levels[k]);
            for (std::size_t l = 0; l < levels; ++l) {
                const std::size_t at = offsets[k] + l;
                double next_m = m_total[k] > 0 ? m_next[at] / m_total[k]
                                               : m[at];
                double next_u = u_total[k] > 0 ? u_next[at] / u_total[k]
                                               : u[at];
                next_m = (std::min)((std::max)(next_m, floor_prob),
                                    1.0 - floor_prob);
                next_u = (std::min)((std::max)(next_u, floor_prob),
                                    1.0 - floor_prob);
                shift = (std::max)(shift, std::fabs(next_m - m[at]));
                shift = (std::max)(shift, std::fabs(next_u - u[at]));
                m[at] = next_m;
                u[at] = next_u;
            }
        }

        loglik = new_loglik;
        if (shift < tol) {
            converged = true;
            ++iteration;
            break;
        }
    }

    // One last E step, so the posteriors returned belong to the parameters
    // returned rather than to the iteration before them.
    NumericVector weight = no_init(static_cast<R_xlen_t>(rows));
    NumericVector probability = no_init(static_cast<R_xlen_t>(rows));
    for (std::size_t r = 0; r < rows; ++r) {
        const int* pattern = flat.data() + r * width;
        const double log_m = pattern_loglik(pattern, width, m, offsets);
        const double log_u = pattern_loglik(pattern, width, u, offsets);
        weight[r] = (log_m - log_u) / std::log(2.0);
        const double log_match = std::log(lambda) + log_m;
        const double log_other = std::log(1.0 - lambda) + log_u;
        const double top = (std::max)(log_match, log_other);
        const double mix =
            top + std::log(std::exp(log_match - top) + std::exp(log_other - top));
        probability[r] = std::exp(log_match - mix);
    }

    return List::create(_["m"] = wrap(m), _["u"] = wrap(u),
                        _["lambda"] = lambda, _["iterations"] = iteration,
                        _["converged"] = converged, _["loglik"] = loglik,
                        _["weight"] = weight, _["probability"] = probability);
}

// Match weight (log2 of the ratio of the two classes' likelihoods) and
// posterior probability of a match, for already-fitted probabilities.
//
// [[Rcpp::export]]
List fast_match_weight_impl(IntegerMatrix patterns, NumericVector m_flat,
                            NumericVector u_flat, IntegerVector n_levels,
                            double lambda) {
    const std::size_t rows = static_cast<std::size_t>(patterns.nrow());
    const std::size_t width = static_cast<std::size_t>(patterns.ncol());
    if (static_cast<std::size_t>(n_levels.size()) != width)
        stop("`n_levels` must have one entry per comparison.");
    std::vector<std::size_t> offsets(width);
    std::size_t total_levels = 0;
    for (std::size_t k = 0; k < width; ++k) {
        offsets[k] = total_levels;
        total_levels += static_cast<std::size_t>(n_levels[k]);
    }
    if (static_cast<std::size_t>(m_flat.size()) != total_levels ||
        static_cast<std::size_t>(u_flat.size()) != total_levels)
        stop("`m` and `u` must hold one probability per level.");

    const std::vector<double> m(m_flat.begin(), m_flat.end());
    const std::vector<double> u(u_flat.begin(), u_flat.end());

    NumericVector weight = no_init(static_cast<R_xlen_t>(rows));
    NumericVector probability = no_init(static_cast<R_xlen_t>(rows));
    std::vector<int> pattern(width);
    for (std::size_t r = 0; r < rows; ++r) {
        bool bad = false;
        for (std::size_t k = 0; k < width; ++k) {
            const int value = patterns(static_cast<int>(r), static_cast<int>(k));
            if (value != NA_INTEGER &&
                (value < 0 || value >= n_levels[static_cast<int>(k)]))
                bad = true;
            pattern[k] = value == NA_INTEGER ? PATTERN_NA : value;
        }
        if (bad) {
            weight[r] = NA_REAL;
            probability[r] = NA_REAL;
            continue;
        }
        const double log_m = pattern_loglik(pattern.data(), width, m, offsets);
        const double log_u = pattern_loglik(pattern.data(), width, u, offsets);
        weight[r] = (log_m - log_u) / std::log(2.0);
        const double log_match = std::log(lambda) + log_m;
        const double log_other = std::log(1.0 - lambda) + log_u;
        const double top = (std::max)(log_match, log_other);
        const double mix =
            top + std::log(std::exp(log_match - top) + std::exp(log_other - top));
        probability[r] = std::exp(log_match - mix);
    }
    return List::create(_["weight"] = weight, _["probability"] = probability);
}

// How many distinct pairs each of two pair sets holds, and how many distinct
// pairs they have in common: everything linkage_eval() and blocking_eval()
// need, in one pass over each set.
//
// [[Rcpp::export]]
List fast_pair_overlap_impl(IntegerVector i, IntegerVector j,
                            IntegerVector other_i, IntegerVector other_j) {
    if (i.size() != j.size()) stop("`i` and `j` must have the same length.");
    if (other_i.size() != other_j.size())
        stop("The second pair set's indices must have the same length.");
    std::unordered_set<std::uint64_t> known;
    known.reserve(static_cast<std::size_t>(other_i.size()) * 2 + 1);
    for (R_xlen_t k = 0; k < other_i.size(); ++k) {
        if (other_i[k] == NA_INTEGER || other_j[k] == NA_INTEGER) continue;
        known.insert(pair_key(other_i[k], other_j[k]));
    }
    std::unordered_set<std::uint64_t> seen;
    seen.reserve(static_cast<std::size_t>(i.size()) * 2 + 1);
    double common = 0.0;
    for (R_xlen_t k = 0; k < i.size(); ++k) {
        if (i[k] == NA_INTEGER || j[k] == NA_INTEGER) continue;
        const std::uint64_t key = pair_key(i[k], j[k]);
        if (!seen.insert(key).second) continue;  // already counted
        if (known.count(key) > 0) common += 1.0;
    }
    return List::create(_["n"] = static_cast<double>(seen.size()),
                        _["n_other"] = static_cast<double>(known.size()),
                        _["n_common"] = common);
}

// Greedy n-to-m selection: take the pairs in order of decreasing weight,
// keeping one whenever neither of its records has used up its allowance.
//
// [[Rcpp::export]]
LogicalVector fast_select_greedy_impl(IntegerVector i, IntegerVector j,
                                      NumericVector weight, int n, int m) {
    const R_xlen_t pairs = i.size();
    if (j.size() != pairs || weight.size() != pairs)
        stop("`i`, `j` and `weight` must have the same length.");
    std::vector<R_xlen_t> order;
    order.reserve(static_cast<std::size_t>(pairs));
    for (R_xlen_t k = 0; k < pairs; ++k) {
        if (i[k] == NA_INTEGER || j[k] == NA_INTEGER || ISNAN(weight[k]))
            continue;
        order.push_back(k);
    }
    // Stable, so that pairs of equal weight are taken in the order given.
    std::stable_sort(order.begin(), order.end(),
                     [&weight](R_xlen_t a, R_xlen_t b) {
                         return weight[a] > weight[b];
                     });

    std::unordered_map<int, int> used_i, used_j;
    used_i.reserve(order.size() * 2 + 1);
    used_j.reserve(order.size() * 2 + 1);
    LogicalVector out(pairs);
    for (R_xlen_t k = 0; k < pairs; ++k)
        if (i[k] == NA_INTEGER || j[k] == NA_INTEGER || ISNAN(weight[k]))
            out[k] = NA_LOGICAL;
    for (R_xlen_t at : order) {
        int& count_i = used_i.insert(std::make_pair(i[at], 0)).first->second;
        if (count_i >= n) continue;
        int& count_j = used_j.insert(std::make_pair(j[at], 0)).first->second;
        if (count_j >= m) continue;
        ++count_i;
        ++count_j;
        out[at] = TRUE;
    }
    return out;
}

// Relative frequency of each element's own value, counted over `x` (and over
// `y` as well when it is given, so that both sides of a linkage share one
// distribution).
//
// [[Rcpp::export]]
List fast_value_frequency_impl(IntegerVector codes_x, IntegerVector codes_y,
                               int n_codes) {
    std::vector<double> counts(static_cast<std::size_t>(n_codes) + 1, 0.0);
    double total = 0.0;
    for (R_xlen_t k = 0; k < codes_x.size(); ++k)
        if (codes_x[k] != NA_INTEGER) {
            counts[static_cast<std::size_t>(codes_x[k])] += 1.0;
            total += 1.0;
        }
    for (R_xlen_t k = 0; k < codes_y.size(); ++k)
        if (codes_y[k] != NA_INTEGER) {
            counts[static_cast<std::size_t>(codes_y[k])] += 1.0;
            total += 1.0;
        }
    NumericVector out_x = no_init(codes_x.size());
    for (R_xlen_t k = 0; k < codes_x.size(); ++k)
        out_x[k] = codes_x[k] == NA_INTEGER
            ? NA_REAL
            : counts[static_cast<std::size_t>(codes_x[k])] / total;
    NumericVector out_y = no_init(codes_y.size());
    for (R_xlen_t k = 0; k < codes_y.size(); ++k)
        out_y[k] = codes_y[k] == NA_INTEGER
            ? NA_REAL
            : counts[static_cast<std::size_t>(codes_y[k])] / total;
    return List::create(_["x"] = out_x, _["y"] = out_y, _["total"] = total);
}
