// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "parallel_dispatch.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// Candidate pairs for record linkage: block_pairs() and
// sorted_neighbourhood(), plus cluster_pairs().
//
// Each pass gives every record an integer code: the rank of its key among
// the sorted distinct keys (NA for a missing key). A pass with half-window h
// pairs records whose codes differ by at most h, so h = 0 is exact blocking
// and h = (window - 1) / 2 is the sorted neighbourhood method as Python's
// recordlinkage implements it. Pairs found by several passes are reported
// once, with the first pass that found them.

namespace {

struct Pass {
    const int* x_codes;
    int half_window;
    int n_codes;
    std::vector<std::size_t> offsets;  // y records with code c: [offsets[c], offsets[c + 1])
    std::vector<int> members;          // y record indices, by code then index
    std::vector<unsigned char> skipped;  // codes whose block is too big
};

struct Candidate {
    int y;
    int pass;
};

struct PairChunk {
    std::vector<int> x;
    std::vector<int> y;
    std::vector<int> pass;
};

struct PairWorker : public Worker {
    const std::vector<Pass>& passes;
    bool dedupe;
    std::size_t rows_per_chunk;
    std::size_t n_x;
    std::vector<PairChunk>& chunks;

    PairWorker(const std::vector<Pass>& passes_, bool dedupe_,
               std::size_t rows_per_chunk_, std::size_t n_x_,
               std::vector<PairChunk>& chunks_)
        : passes(passes_), dedupe(dedupe_), rows_per_chunk(rows_per_chunk_),
          n_x(n_x_), chunks(chunks_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<Candidate> found;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            PairChunk& out = chunks[chunk];
            const std::size_t first = chunk * rows_per_chunk;
            const std::size_t last = (std::min)(n_x, first + rows_per_chunk);
            for (std::size_t i = first; i < last; ++i) {
                found.clear();
                for (std::size_t p = 0; p < passes.size(); ++p) {
                    const Pass& pass = passes[p];
                    const int code = pass.x_codes[i];
                    if (code == NA_INTEGER) continue;
                    if (!pass.skipped.empty() && pass.skipped[code]) continue;
                    const int lo = (std::max)(1, code - pass.half_window);
                    const int hi = (std::min)(pass.n_codes,
                                              code + pass.half_window);
                    const std::size_t from = pass.offsets[lo];
                    const std::size_t to = pass.offsets[hi + 1];
                    for (std::size_t k = from; k < to; ++k) {
                        const int j = pass.members[k];
                        if (dedupe && j <= static_cast<int>(i)) continue;
                        found.push_back(Candidate{j, static_cast<int>(p)});
                    }
                }
                if (found.empty()) continue;
                std::sort(found.begin(), found.end(),
                          [](const Candidate& a, const Candidate& b) {
                              return a.y != b.y ? a.y < b.y : a.pass < b.pass;
                          });
                int previous = -1;
                for (const Candidate& c : found) {
                    if (c.y == previous) continue;
                    previous = c.y;
                    out.x.push_back(static_cast<int>(i) + 1);
                    out.y.push_back(c.y + 1);
                    out.pass.push_back(c.pass + 1);
                }
            }
        }
    }
};

}  // namespace

// [[Rcpp::export]]
List fast_candidate_pairs_impl(const List& x_codes, const List& y_codes,
                               const IntegerVector& n_codes,
                               const IntegerVector& half_windows, bool dedupe,
                               double max_block, int nthreads) {
    const std::size_t n_passes = static_cast<std::size_t>(x_codes.size());
    if (n_passes == 0) stop("At least one key is needed.");
    std::vector<IntegerVector> xs, ys;
    for (std::size_t p = 0; p < n_passes; ++p) {
        xs.push_back(as<IntegerVector>(x_codes[p]));
        ys.push_back(as<IntegerVector>(y_codes[p]));
    }
    const std::size_t n_x = static_cast<std::size_t>(xs[0].size());

    std::vector<Pass> passes(n_passes);
    double bound = 0;
    std::size_t skipped_blocks = 0;
    for (std::size_t p = 0; p < n_passes; ++p) {
        Pass& pass = passes[p];
        const IntegerVector& xc = xs[p];
        const IntegerVector& yc = ys[p];
        pass.x_codes = xc.begin();
        pass.half_window = half_windows[p];
        pass.n_codes = n_codes[p];
        const std::size_t codes = static_cast<std::size_t>(pass.n_codes);
        // Counting sort of the y records by code.
        pass.offsets.assign(codes + 2, 0);
        for (R_xlen_t j = 0; j < yc.size(); ++j) {
            if (yc[j] != NA_INTEGER) ++pass.offsets[yc[j] + 1];
        }
        for (std::size_t c = 1; c < pass.offsets.size(); ++c)
            pass.offsets[c] += pass.offsets[c - 1];
        pass.members.resize(pass.offsets[codes + 1]);
        std::vector<std::size_t> fill(pass.offsets.begin(),
                                      pass.offsets.end() - 1);
        for (R_xlen_t j = 0; j < yc.size(); ++j) {
            if (yc[j] != NA_INTEGER)
                pass.members[fill[yc[j]]++] = static_cast<int>(j);
        }
        std::vector<double> x_count(codes + 1, 0);
        for (std::size_t i = 0; i < n_x; ++i) {
            if (pass.x_codes[i] != NA_INTEGER) ++x_count[pass.x_codes[i]];
        }
        if (pass.half_window == 0 && max_block < 1e18) {
            pass.skipped.assign(codes + 1, 0);
            for (std::size_t c = 1; c <= codes; ++c) {
                const double y_count = static_cast<double>(
                    pass.offsets[c + 1] - pass.offsets[c]);
                const double size = dedupe ? y_count : x_count[c] + y_count;
                if (size > max_block) {
                    pass.skipped[c] = 1;
                    ++skipped_blocks;
                }
            }
        }
        // An upper bound on the pairs this pass can produce.
        for (std::size_t c = 1; c <= codes; ++c) {
            if (x_count[c] == 0) continue;
            if (!pass.skipped.empty() && pass.skipped[c]) continue;
            const int lo = (std::max)(1, static_cast<int>(c) - pass.half_window);
            const int hi = (std::min)(pass.n_codes,
                                      static_cast<int>(c) + pass.half_window);
            const double partners = static_cast<double>(
                pass.offsets[hi + 1] - pass.offsets[lo]);
            bound += x_count[c] * partners / (dedupe ? 2.0 : 1.0);
        }
    }
    if (bound > static_cast<double>((std::numeric_limits<int>::max)()))
        stop("These keys give more than 2^31 candidate pairs; block on "
             "more specific keys or use a smaller window.");

    const std::size_t rows_per_chunk = 1024;
    const std::size_t n_chunks = (n_x + rows_per_chunk - 1) / rows_per_chunk;
    std::vector<PairChunk> chunks(n_chunks);
    PairWorker worker(passes, dedupe, rows_per_chunk, n_x, chunks);
    const std::size_t work = static_cast<std::size_t>(
        (std::min)(bound + static_cast<double>(n_x), 1e18));
    dispatch_for(0, n_chunks, worker, work, 200000, nthreads, 1);

    std::size_t total = 0;
    for (const PairChunk& c : chunks) total += c.x.size();
    IntegerVector out_x(Rcpp::no_init(total)), out_y(Rcpp::no_init(total)),
        out_pass(Rcpp::no_init(total));
    std::size_t at = 0;
    for (const PairChunk& c : chunks) {
        std::copy(c.x.begin(), c.x.end(), out_x.begin() + at);
        std::copy(c.y.begin(), c.y.end(), out_y.begin() + at);
        std::copy(c.pass.begin(), c.pass.end(), out_pass.begin() + at);
        at += c.x.size();
    }
    return List::create(_["x_index"] = out_x, _["y_index"] = out_y,
                        _["pass"] = out_pass,
                        _["skipped_blocks"] = static_cast<double>(skipped_blocks));
}

// [[Rcpp::export]]
IntegerVector fast_cluster_pairs_impl(const IntegerVector& i,
                                      const IntegerVector& j, int n) {
    std::vector<int> parent(static_cast<std::size_t>(n));
    std::vector<int> size(static_cast<std::size_t>(n), 1);
    for (int k = 0; k < n; ++k) parent[k] = k;
    auto find = [&parent](int v) {
        while (parent[v] != v) {
            parent[v] = parent[parent[v]];  // path halving
            v = parent[v];
        }
        return v;
    };
    const R_xlen_t m = i.size();
    for (R_xlen_t k = 0; k < m; ++k) {
        int a = find(i[k] - 1);
        int b = find(j[k] - 1);
        if (a == b) continue;
        if (size[a] < size[b]) std::swap(a, b);
        parent[b] = a;
        size[a] += size[b];
    }
    // Clusters are numbered in the order of their smallest member.
    IntegerVector out(Rcpp::no_init(n));
    std::vector<int> label(static_cast<std::size_t>(n), 0);
    int next = 0;
    for (int k = 0; k < n; ++k) {
        const int root = find(k);
        if (label[root] == 0) label[root] = ++next;
        out[k] = label[root];
    }
    return out;
}
