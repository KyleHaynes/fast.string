#ifndef FAST_STRING_TEXT_RESULTS_H
#define FAST_STRING_TEXT_RESULTS_H

#include <Rcpp.h>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <string>
#include <thread>
#include <vector>

#include "string_snapshot.h"

enum class TextResultKind : std::uint8_t {
    na,
    original,
    source_slice,
    changed
};

struct TextResult {
    TextResultKind kind = TextResultKind::original;
    std::size_t chunk = 0;
    std::size_t offset = 0;
    std::size_t length = 0;
};

struct TextWorkChunk {
    std::size_t begin;
    std::size_t end;
    std::size_t estimated_bytes;
};

struct TextWorkPlan {
    std::vector<TextWorkChunk> chunks;
    std::size_t grain_size = 1;
    int dispatch_threads = -1;
};

using TextArenas = std::vector<std::vector<char>>;

inline std::vector<TextResult>
make_text_results(const StringSnapshot& snapshot) {
    std::vector<TextResult> results(snapshot.size());
    for (std::size_t i = 0; i < snapshot.size(); ++i) {
        if (snapshot[i].is_na())
            results[i].kind = TextResultKind::na;
    }
    return results;
}

// Build byte-balanced ranges for allocation-producing string kernels. The
// nominal chunk is 256 KiB, reduced when necessary to expose 4-8 tasks per
// requested thread; no chunk contains more than 4,096 input rows.
inline TextWorkPlan
make_text_work_plan(const StringSnapshot& snapshot,
                    std::size_t estimated_work,
                    std::size_t parallel_threshold,
                    int nthreads,
                    std::size_t row_overhead) {
    constexpr std::size_t TARGET_BYTES = 256u * 1024u;
    constexpr std::size_t MAX_ROWS = 4096u;
    constexpr std::size_t CHUNKS_PER_THREAD = 6u;

    TextWorkPlan plan;
    plan.dispatch_threads = nthreads;
    const std::size_t n = snapshot.size();
    if (n == 0) return plan;
    const bool serial =
        nthreads == 1 || estimated_work < parallel_threshold;

    std::size_t thread_hint = nthreads > 0
        ? static_cast<std::size_t>(nthreads)
        : static_cast<std::size_t>((std::max)(
              1u, std::thread::hardware_concurrency()));
    thread_hint = (std::min)(thread_hint, n);
    const std::size_t natural_chunks =
        estimated_work / TARGET_BYTES +
        (estimated_work % TARGET_BYTES != 0);
    const std::size_t useful_threads = (std::max)(
        static_cast<std::size_t>(1),
        natural_chunks / CHUNKS_PER_THREAD +
            (natural_chunks % CHUNKS_PER_THREAD != 0)
    );
    thread_hint = (std::min)(thread_hint, useful_threads);
    if (nthreads > 0) {
        plan.dispatch_threads = static_cast<int>((std::min)(
            static_cast<std::size_t>(nthreads), thread_hint
        ));
    }
    const std::size_t max_size =
        (std::numeric_limits<std::size_t>::max)();
    const std::size_t desired_chunks =
        thread_hint > max_size / CHUNKS_PER_THREAD
            ? max_size
            : thread_hint * CHUNKS_PER_THREAD;
    std::size_t target = (std::numeric_limits<std::size_t>::max)();
    if (!serial) {
        target = desired_chunks == 0
            ? TARGET_BYTES
            : estimated_work / desired_chunks +
                  (estimated_work % desired_chunks != 0);
        target = (std::max)(static_cast<std::size_t>(1), target);
    }

    plan.chunks.reserve((std::min)(n, desired_chunks));
    std::size_t begin = 0;
    std::size_t bytes = 0;
    for (std::size_t i = 0; i < n; ++i) {
        const StringView& value = snapshot[i];
        const std::size_t value_bytes = value.is_na() ? 0 : value.size;
        const std::size_t row_bytes =
            value_bytes > max_size - row_overhead
                ? max_size
                : value_bytes + row_overhead;
        bytes = bytes > max_size - row_bytes ? max_size : bytes + row_bytes;
        const std::size_t rows = i - begin + 1;
        if (rows >= MAX_ROWS || bytes >= target) {
            plan.chunks.push_back(TextWorkChunk{begin, i + 1, bytes});
            begin = i + 1;
            bytes = 0;
        }
    }
    if (begin < n)
        plan.chunks.push_back(TextWorkChunk{begin, n, bytes});
    if (!serial && thread_hint != 0) {
        plan.grain_size =
            plan.chunks.size() / thread_hint +
            (plan.chunks.size() % thread_hint != 0);
    }
    return plan;
}

inline void store_changed_text(std::size_t row,
                               std::size_t chunk,
                               const char* data,
                               std::size_t length,
                               std::vector<TextResult>& results,
                               TextArenas& arenas) {
    std::vector<char>& arena = arenas[chunk];
    const std::size_t offset = arena.size();
    if (length != 0)
        arena.insert(arena.end(), data, data + length);
    results[row] = TextResult{
        TextResultKind::changed, chunk, offset, length
    };
}

inline void store_text_output(std::size_t row,
                              std::size_t chunk,
                              const StringView& source,
                              const char* data,
                              std::size_t length,
                              std::vector<TextResult>& results,
                              TextArenas& arenas) {
    if (length == source.size &&
        (length == 0 || std::memcmp(data, source.data, length) == 0)) {
        results[row] = TextResult{};
        return;
    }
    store_changed_text(
        row, chunk, data, length, results, arenas
    );
}

inline SEXP make_text_charsxp(const char* data,
                              std::size_t length,
                              cetype_t encoding) {
    if (length >
            static_cast<std::size_t>((std::numeric_limits<int>::max)())) {
        Rcpp::stop("Substitution result exceeds R's maximum string length.");
    }
    return Rf_mkCharLenCE(
        length == 0 ? "" : data,
        static_cast<int>(length),
        encoding
    );
}

inline Rcpp::CharacterVector finalize_text_results(
        const StringSnapshot& snapshot,
        const std::vector<TextResult>& records,
        const TextArenas& arenas) {
    const R_xlen_t n = static_cast<R_xlen_t>(snapshot.size());
    Rcpp::CharacterVector output(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        const std::size_t row = static_cast<std::size_t>(i);
        const TextResult& record = records[row];
        switch (record.kind) {
            case TextResultKind::na:
                SET_STRING_ELT(output, i, NA_STRING);
                break;
            case TextResultKind::original:
                SET_STRING_ELT(output, i, snapshot.charsxp(row));
                break;
            case TextResultKind::source_slice: {
                SEXP source = snapshot.charsxp(row);
                SET_STRING_ELT(
                    output, i,
                    make_text_charsxp(
                        snapshot[row].data + record.offset,
                        record.length,
                        Rf_getCharCE(source)
                    )
                );
                break;
            }
            case TextResultKind::changed: {
                const std::vector<char>& arena = arenas[record.chunk];
                const char* data = record.length == 0
                    ? ""
                    : arena.data() + record.offset;
                SET_STRING_ELT(
                    output, i,
                    make_text_charsxp(data, record.length, CE_UTF8)
                );
                break;
            }
        }
    }
    return output;
}

#endif // FAST_STRING_TEXT_RESULTS_H
