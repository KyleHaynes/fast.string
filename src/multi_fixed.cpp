// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "aho_corasick.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
#include "text_results.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>
#include <string>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// fgrepl_any(), fcount_any(), fwhich_any() and fextract_any(): many literal
// patterns searched in one pass with an Aho-Corasick automaton.

namespace {

std::unique_ptr<AhoCorasick> build_automaton(const StringVector& patterns,
                                             bool ignore_case) {
    std::vector<std::string> keys;
    keys.reserve(static_cast<std::size_t>(patterns.size()));
    for (R_xlen_t j = 0; j < patterns.size(); ++j) {
        const SEXP value = STRING_ELT(patterns, j);
        if (value == NA_STRING) stop("`patterns` must not contain NA.");
        if (LENGTH(value) == 0) stop("`patterns` must not contain empty strings.");
        keys.emplace_back(CHAR(value), static_cast<std::size_t>(LENGTH(value)));
    }
    if (keys.empty()) stop("`patterns` must contain at least one string.");
    if (keys.size() > static_cast<std::size_t>((std::numeric_limits<int>::max)()))
        stop("Too many patterns.");
    return std::unique_ptr<AhoCorasick>(new AhoCorasick(keys, ignore_case));
}

std::size_t scan_work(const StringSnapshot& snapshot) {
    const std::uint64_t work =
        snapshot.total_bytes() + 8u * static_cast<std::uint64_t>(snapshot.size());
    const std::uint64_t limit =
        static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)());
    return static_cast<std::size_t>((std::min)(work, limit));
}

enum class AnyMode { detect, count, which };

struct AnyWorker : public Worker {
    const StringView* strings;
    const AhoCorasick& automaton;
    AnyMode mode;
    RVector<int> out;

    AnyWorker(const StringView* strings_, const AhoCorasick& automaton_,
              AnyMode mode_, IntegerVector& out_)
        : strings(strings_), automaton(automaton_), mode(mode_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) {
                out[i] = NA_INTEGER;
                continue;
            }
            switch (mode) {
            case AnyMode::detect:
                out[i] = automaton.contains(value.data, value.size) ? 1 : 0;
                break;
            case AnyMode::which: {
                const int found = automaton.min_pattern(value.data, value.size);
                out[i] = found < 0 ? NA_INTEGER : found + 1;
                break;
            }
            case AnyMode::count: {
                int count = 0;
                std::size_t from = 0;
                AhoCorasick::Match match;
                while (automaton.find(value.data, value.size, from, match)) {
                    ++count;
                    from = match.start + match.length;
                }
                out[i] = count;
                break;
            }
            }
        }
    }
};

struct Span {
    std::size_t offset;
    std::size_t length;
};

struct ExtractAnyWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    const AhoCorasick& automaton;
    bool all;
    std::vector<std::vector<Span>>& spans;
    std::vector<std::size_t>& first;
    std::vector<std::size_t>& count;

    ExtractAnyWorker(const StringView* strings_,
                     const std::vector<TextWorkChunk>& chunks_,
                     const AhoCorasick& automaton_, bool all_,
                     std::vector<std::vector<Span>>& spans_,
                     std::vector<std::size_t>& first_,
                     std::vector<std::size_t>& count_)
        : strings(strings_), chunks(chunks_), automaton(automaton_), all(all_),
          spans(spans_), first(first_), count(count_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<Span>& out = spans[chunk];
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                first[i] = out.size();
                count[i] = 0;
                if (value.is_na()) continue;
                std::size_t from = 0;
                AhoCorasick::Match match;
                while (automaton.find(value.data, value.size, from, match)) {
                    out.push_back(Span{match.start, match.length});
                    ++count[i];
                    if (!all) break;
                    from = match.start + match.length;
                }
            }
        }
    }
};

}  // namespace

// mode: 0 detect (logical), 1 count, 2 which.
// [[Rcpp::export]]
SEXP fast_any_impl(const StringVector& patterns, const StringVector& x,
                   bool ignore_case, bool use_bytes, int mode, int nthreads) {
    const std::unique_ptr<AhoCorasick> automaton =
        build_automaton(patterns, ignore_case);
    StringSnapshot snapshot(x, use_bytes ? SnapshotText::raw : SnapshotText::utf8);
    const std::size_t n = snapshot.size();
    IntegerVector out = no_init(static_cast<R_xlen_t>(n));
    const AnyMode any_mode = mode == 0 ? AnyMode::detect
        : mode == 1 ? AnyMode::count : AnyMode::which;
    AnyWorker worker(snapshot.data(), *automaton, any_mode, out);
    dispatch_for(0, n, worker, scan_work(snapshot), 100000, nthreads);
    if (any_mode == AnyMode::detect) {
        LogicalVector result(out.size());
        std::copy(out.begin(), out.end(), result.begin());
        return result;
    }
    return out;
}

// [[Rcpp::export]]
SEXP fast_extract_any_impl(const StringVector& patterns, const StringVector& x,
                           bool ignore_case, bool use_bytes, bool all,
                           int nthreads) {
    const std::unique_ptr<AhoCorasick> automaton =
        build_automaton(patterns, ignore_case);
    StringSnapshot snapshot(x, use_bytes ? SnapshotText::raw : SnapshotText::utf8);
    const std::size_t n = snapshot.size();
    const std::size_t work = scan_work(snapshot);
    const std::size_t threshold = 100000;
    const TextWorkPlan plan =
        make_text_work_plan(snapshot, work, threshold, nthreads, 8);
    std::vector<std::vector<Span>> spans(plan.chunks.size());
    std::vector<std::size_t> first(n, 0), count(n, 0);
    ExtractAnyWorker worker(snapshot.data(), plan.chunks, *automaton, all,
                            spans, first, count);
    dispatch_for(0, plan.chunks.size(), worker, work, threshold,
                 plan.dispatch_threads, plan.grain_size);

    if (!all) {
        std::vector<TextResult> results = make_text_results(snapshot);
        for (std::size_t c = 0; c < plan.chunks.size(); ++c) {
            for (std::size_t i = plan.chunks[c].begin; i < plan.chunks[c].end;
                 ++i) {
                if (snapshot[i].is_na()) continue;
                if (count[i] == 0) {
                    results[i] = TextResult{TextResultKind::na, 0, 0, 0};
                    continue;
                }
                const Span& span = spans[c][first[i]];
                if (span.offset == 0 && span.length == snapshot[i].size)
                    results[i] = TextResult{};
                else
                    results[i] = TextResult{TextResultKind::source_slice, 0,
                                            span.offset, span.length};
            }
        }
        TextArenas arenas;
        return finalize_text_results(snapshot, results, arenas);
    }

    List output(static_cast<R_xlen_t>(n));
    for (std::size_t c = 0; c < plan.chunks.size(); ++c) {
        for (std::size_t i = plan.chunks[c].begin; i < plan.chunks[c].end; ++i) {
            if (snapshot[i].is_na()) {
                output[static_cast<R_xlen_t>(i)] = CharacterVector::create(NA_STRING);
                continue;
            }
            const cetype_t encoding = snapshot.translated(i)
                ? CE_UTF8
                : Rf_getCharCE(snapshot.charsxp(i));
            CharacterVector matches(static_cast<R_xlen_t>(count[i]));
            for (std::size_t k = 0; k < count[i]; ++k) {
                const Span& span = spans[c][first[i] + k];
                SET_STRING_ELT(matches, static_cast<R_xlen_t>(k),
                               make_text_charsxp(snapshot[i].data + span.offset,
                                                 span.length, encoding));
            }
            output[static_cast<R_xlen_t>(i)] = matches;
        }
    }
    return output;
}
