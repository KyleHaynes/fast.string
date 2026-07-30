#ifndef FAST_STRING_PARALLEL_DISPATCH_H
#define FAST_STRING_PARALLEL_DISPATCH_H

#include <RcppParallel.h>
#include <algorithm>
#include <cstddef>

#if defined(_WIN32) && !RCPP_PARALLEL_USE_TBB && \
    !defined(FAST_STRING_ALLOW_TINYTHREAD)
#error "fast.string Windows builds require RcppParallel TBB linkage"
#endif

inline std::size_t fast_string_ceil_div(std::size_t numerator,
                                        std::size_t denominator) {
    return numerator / denominator + (numerator % denominator != 0);
}

// Run small or explicitly single-threaded work in the caller. Otherwise pass
// the requested thread count directly to RcppParallel, avoiding mutation of
// the process-wide RCPP_PARALLEL_NUM_THREADS setting.
template <typename Worker>
inline void dispatch_for(std::size_t begin,
                         std::size_t end,
                         Worker& worker,
                         std::size_t estimated_work,
                         std::size_t parallel_threshold,
                         int num_threads = -1,
                         std::size_t grain_size = 1) {
    if (end <= begin) return;
    if (estimated_work < parallel_threshold || num_threads == 1) {
        worker(begin, end);
        return;
    }

    grain_size = (std::max)(grain_size, static_cast<std::size_t>(1));
    const std::size_t tasks =
        fast_string_ceil_div(end - begin, grain_size);
    if (num_threads > 0) {
        num_threads = static_cast<int>((std::min)(
            static_cast<std::size_t>(num_threads), tasks
        ));
        if (num_threads == 1) {
            worker(begin, end);
            return;
        }
    }

#if !RCPP_PARALLEL_USE_TBB
    // TinyThread ignores parallelFor's numThreads argument and creates one OS
    // thread per range. A minimum range size keeps explicit requests bounded.
    if (num_threads > 0) {
        const std::size_t range_size = end - begin;
        grain_size = (std::max)(
            grain_size,
            fast_string_ceil_div(range_size,
                                 static_cast<std::size_t>(num_threads))
        );
    }
#endif

    RcppParallel::parallelFor(
        begin, end, worker, grain_size, num_threads
    );
}

#endif // FAST_STRING_PARALLEL_DISPATCH_H
