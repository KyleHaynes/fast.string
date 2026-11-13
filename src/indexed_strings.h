#ifndef FAST_STRING_INDEXED_STRINGS_H
#define FAST_STRING_INDEXED_STRINGS_H

#include <Rcpp.h>
#include <RcppParallel.h>
#include <atomic>
#include <cstddef>
#include <cstdint>
#include "parallel_dispatch.h"

// `source[index]` as a character vector that has not been built.
//
// rLink() scores candidate pairs, and a pair is two record numbers: the
// strings a comparator is handed are `surname[index_1]` and
// `surname[index_2]`. Built as ordinary vectors those are one pointer per
// pair, and every comparator then resolved CHAR() and LENGTH() for each of
// them on the calling thread before any worker could start -- for ten
// million pairs drawn from half a million records, the same half million
// strings resolved twenty times over. That serial pass, not the comparison,
// was most of the time a comparator took.
//
// An indexed vector is an ALTREP character vector holding only the source
// and the index. To R it is a character vector like any other: it has a
// length, its elements can be read, and anything that asks for its data
// pointer gets an ordinary expanded copy. StringSnapshot and
// CodepointSnapshot recognise it and resolve the *source* instead, once per
// record, then lay the views out per pair in parallel.
struct IndexedStrings {
    SEXP source;       // the strings, one per record
    const int* index;  // 1-based; NA or out of range reads as NA
    std::size_t size;  // number of pairs
    int nthreads;      // the caller's thread cap, -1 for the default
};

// Whether `x` is an indexed vector that has not been expanded, and if so
// what it holds. Defined in indexed_strings.cpp; main thread only.
bool indexed_strings(SEXP x, IndexedStrings& out);

// The ordinary character vector an indexed vector stands for, built on
// first request and kept, or R_NilValue when `x` is not one. For a reader
// that cannot use the index: one gather, shared with whoever reads the
// vector next. Main thread only.
SEXP expanded_indexed_strings(SEXP x);

// out[i] = from[index[i] - 1], or `missing` where the index is NA or out of
// range. Returns the sum of the views' sizes, which is what the snapshots
// report as their total for the work estimate.
template <typename View>
struct IndexedGatherWorker : public RcppParallel::Worker {
    const View* from;
    std::size_t n_from;
    const int* index;
    View* out;
    View missing;
    std::atomic<std::uint64_t>& total;

    IndexedGatherWorker(const View* from_, std::size_t n_from_,
                        const int* index_, View* out_, const View& missing_,
                        std::atomic<std::uint64_t>& total_)
        : from(from_), n_from(n_from_), index(index_), out(out_),
          missing(missing_), total(total_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::uint64_t sum = 0;
        for (std::size_t i = begin; i < end; ++i) {
            const int k = index[i];
            if (k == NA_INTEGER || k < 1 ||
                static_cast<std::size_t>(k) > n_from) {
                out[i] = missing;
                continue;
            }
            out[i] = from[static_cast<std::size_t>(k) - 1];
            sum += static_cast<std::uint64_t>(out[i].size);
        }
        total.fetch_add(sum, std::memory_order_relaxed);
    }
};

template <typename View>
inline std::uint64_t gather_indexed(const View* from, std::size_t n_from,
                                    const IndexedStrings& ix, View* out,
                                    const View& missing) {
    std::atomic<std::uint64_t> total(0);
    IndexedGatherWorker<View> worker(from, n_from, ix.index, out, missing,
                                     total);
    dispatch_for(0, ix.size, worker, ix.size, 200000, ix.nthreads, 16384);
    return total.load();
}

#endif  // FAST_STRING_INDEXED_STRINGS_H
