#ifndef FAST_STRING_AHO_CORASICK_H
#define FAST_STRING_AHO_CORASICK_H

// Aho-Corasick multi-pattern literal search over bytes (Aho & Corasick,
// 1975). Built once on the main thread and then only read, so workers share
// one automaton without synchronisation.
//
// Patterns are byte strings; with `ignore_case` only ASCII letters fold, as
// in the package's other fixed-string searches. Bytes are first mapped to
// equivalence classes (each byte that occurs in a pattern gets its own
// class, every other byte shares class 0), which keeps the transition table
// small. When states x classes fits in a few hundred megabytes the failure
// links are compiled into a full DFA, one table read per input byte;
// otherwise the scan follows failure links over sorted sparse edges.
//
// Matching semantics are those of gsub_all(fixed = TRUE,
// sequential = FALSE): leftmost-first -- the match that starts earliest
// wins, and among matches starting at the same position the pattern that
// comes first in the pattern vector wins.

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <utility>
#include <vector>

class AhoCorasick {
public:
    struct Match {
        std::size_t start;
        std::size_t length;
        int pattern;  // 0-based index into the pattern vector
    };

    AhoCorasick(const std::vector<std::string>& patterns, bool ignore_case) {
        build(patterns, ignore_case);
    }

    std::size_t pattern_count() const noexcept { return lengths_.size(); }
    std::size_t pattern_length(int pattern) const {
        return lengths_[static_cast<std::size_t>(pattern)];
    }

    // Whether any pattern occurs in s.
    bool contains(const char* s, std::size_t n) const {
        std::int32_t state = 0;
        for (std::size_t i = 0; i < n; ++i) {
            state = step(state, s[i]);
            if (chain_min_[static_cast<std::size_t>(state)] != NONE) return true;
        }
        return false;
    }

    // The lowest index of any pattern that occurs in s, or -1.
    int min_pattern(const char* s, std::size_t n) const {
        std::int32_t state = 0;
        std::int32_t best = NONE;
        for (std::size_t i = 0; i < n; ++i) {
            state = step(state, s[i]);
            const std::int32_t found = chain_min_[static_cast<std::size_t>(state)];
            if (found < best) {
                best = found;
                if (best == 0) break;
            }
        }
        return best == NONE ? -1 : static_cast<int>(best);
    }

    // The leftmost-first match starting at or after `from`.
    bool find(const char* s, std::size_t n, std::size_t from, Match& match) const {
        std::int32_t state = 0;
        std::size_t best_start = NPOS;
        std::int32_t best_pattern = NONE;
        std::size_t best_length = 0;
        for (std::size_t i = from; i < n; ++i) {
            // Matches ending here or later start after best_start.
            if (best_start != NPOS && i >= best_start + max_length_) break;
            state = step(state, s[i]);
            const std::size_t at = static_cast<std::size_t>(state);
            const std::uint32_t length = longest_length_[at];
            if (length == 0) continue;
            const std::size_t start = i + 1 - length;
            const std::int32_t pattern = longest_pattern_[at];
            if (start < best_start ||
                (start == best_start && pattern < best_pattern)) {
                best_start = start;
                best_pattern = pattern;
                best_length = length;
            }
        }
        if (best_start == NPOS) return false;
        match = Match{best_start, best_length, static_cast<int>(best_pattern)};
        return true;
    }

private:
    static constexpr std::int32_t NONE = (std::numeric_limits<std::int32_t>::max)();
    static constexpr std::size_t NPOS = (std::numeric_limits<std::size_t>::max)();
    // Dense DFA limit: states x classes transitions (4 bytes each).
    static constexpr std::size_t DENSE_LIMIT = std::size_t(64) << 20;

    std::uint16_t byte_class_[256] = {};
    std::size_t classes_ = 1;
    std::size_t max_length_ = 0;
    std::vector<std::size_t> lengths_;

    // Sparse trie: the edges of state s are edge_class_/edge_target_
    // [edge_begin_[s], edge_begin_[s + 1]), sorted by class.
    std::vector<std::uint32_t> edge_begin_;
    std::vector<std::uint16_t> edge_class_;
    std::vector<std::int32_t> edge_target_;
    std::vector<std::int32_t> fail_;
    std::vector<std::int32_t> root_next_;  // goto of the root, every class

    std::vector<std::int32_t> dense_;  // states x classes, or empty

    // Per state: the longest pattern ending here (via the failure chain),
    // and the lowest pattern index ending here.
    std::vector<std::uint32_t> longest_length_;
    std::vector<std::int32_t> longest_pattern_;
    std::vector<std::int32_t> chain_min_;

    std::int32_t goto_edge(std::int32_t state, std::uint16_t cls) const {
        if (state == 0) return root_next_[cls];
        const std::uint32_t lo = edge_begin_[static_cast<std::size_t>(state)];
        const std::uint32_t hi = edge_begin_[static_cast<std::size_t>(state) + 1];
        const std::uint16_t* first = edge_class_.data() + lo;
        const std::uint16_t* last = edge_class_.data() + hi;
        const std::uint16_t* it = std::lower_bound(first, last, cls);
        if (it == last || *it != cls) return -1;
        return edge_target_[lo + static_cast<std::uint32_t>(it - first)];
    }

    std::int32_t step(std::int32_t state, char byte) const {
        const std::uint16_t cls = byte_class_[static_cast<unsigned char>(byte)];
        if (!dense_.empty())
            return dense_[static_cast<std::size_t>(state) * classes_ + cls];
        while (true) {
            const std::int32_t next = goto_edge(state, cls);
            if (next >= 0) return next;
            if (state == 0) return 0;
            state = fail_[static_cast<std::size_t>(state)];
        }
    }

    static unsigned char fold(unsigned char c, bool ignore_case) {
        return ignore_case && c >= 'A' && c <= 'Z'
            ? static_cast<unsigned char>(c + ('a' - 'A'))
            : c;
    }

    void build(const std::vector<std::string>& patterns, bool ignore_case) {
        const std::size_t count = patterns.size();
        lengths_.resize(count);
        // Byte classes, numbered in byte order.
        bool used[256] = {};
        for (const std::string& p : patterns)
            for (char c : p)
                used[fold(static_cast<unsigned char>(c), ignore_case)] = true;
        std::uint16_t next_class = 1;
        for (int b = 0; b < 256; ++b)
            if (used[b]) byte_class_[b] = next_class++;
        if (ignore_case)
            for (int b = 'A'; b <= 'Z'; ++b)
                byte_class_[b] = byte_class_[b + ('a' - 'A')];
        classes_ = next_class;

        // Insert the patterns in sorted order, so that every state's
        // children are created consecutively and in class order.
        std::vector<std::vector<std::uint16_t>> keys(count);
        for (std::size_t p = 0; p < count; ++p) {
            lengths_[p] = patterns[p].size();
            max_length_ = (std::max)(max_length_, patterns[p].size());
            keys[p].reserve(patterns[p].size());
            for (char c : patterns[p])
                keys[p].push_back(byte_class_[fold(static_cast<unsigned char>(c),
                                                   ignore_case)]);
        }
        std::vector<std::size_t> order(count);
        for (std::size_t p = 0; p < count; ++p) order[p] = p;
        std::stable_sort(order.begin(), order.end(),
                         [&keys](std::size_t a, std::size_t b) {
                             return keys[a] < keys[b];
                         });

        // Build the trie level by level (breadth first): states of depth d
        // are the distinct length-d prefixes, in sorted order. Each
        // pattern's state at the current depth is tracked in `at`.
        std::vector<std::int32_t> own;      // lowest pattern ending exactly here
        std::vector<std::uint32_t> depth;
        std::vector<std::int32_t> parent;
        std::vector<std::uint16_t> via;     // class of the edge from the parent
        own.push_back(NONE);
        depth.push_back(0);
        parent.push_back(-1);
        via.push_back(0);
        std::vector<std::int32_t> at(count, 0);
        std::vector<std::size_t> alive;  // patterns longer than the depth
        for (std::size_t k = 0; k < count; ++k)
            if (!keys[order[k]].empty()) alive.push_back(order[k]);
        for (std::size_t d = 0; !alive.empty(); ++d) {
            std::vector<std::size_t> still;
            std::int32_t last_parent = -1;
            std::uint16_t last_class = 0;
            std::int32_t last_state = -1;
            for (std::size_t p : alive) {
                const std::uint16_t cls = keys[p][d];
                if (at[p] != last_parent || cls != last_class) {
                    last_state = static_cast<std::int32_t>(own.size());
                    own.push_back(NONE);
                    depth.push_back(static_cast<std::uint32_t>(d + 1));
                    parent.push_back(at[p]);
                    via.push_back(cls);
                    last_parent = at[p];
                    last_class = cls;
                }
                at[p] = last_state;
                if (keys[p].size() == d + 1) {
                    std::int32_t& o = own[static_cast<std::size_t>(last_state)];
                    o = (std::min)(o, static_cast<std::int32_t>(p));
                } else {
                    still.push_back(p);
                }
            }
            // Keep the order sorted by (parent state, class, rest): parents
            // were created in sorted order, so a stable pass suffices.
            alive.swap(still);
        }
        const std::size_t states = own.size();

        // Edges in CSR form. States were created breadth first with each
        // parent's children consecutive and in class order.
        edge_begin_.assign(states + 1, 0);
        for (std::size_t s = 1; s < states; ++s)
            ++edge_begin_[static_cast<std::size_t>(parent[s]) + 1];
        for (std::size_t s = 0; s < states; ++s)
            edge_begin_[s + 1] += edge_begin_[s];
        edge_class_.assign(states == 0 ? 0 : states - 1, 0);
        edge_target_.assign(states == 0 ? 0 : states - 1, 0);
        {
            std::vector<std::uint32_t> fill(edge_begin_.begin(),
                                            edge_begin_.end() - 1);
            for (std::size_t s = 1; s < states; ++s) {
                const std::size_t p = static_cast<std::size_t>(parent[s]);
                edge_class_[fill[p]] = via[s];
                edge_target_[fill[p]] = static_cast<std::int32_t>(s);
                ++fill[p];
            }
        }
        root_next_.assign(classes_, 0);
        for (std::uint32_t e = edge_begin_[0]; e < edge_begin_[1]; ++e)
            root_next_[edge_class_[e]] = edge_target_[e];

        // Failure links and outputs, breadth first (states are already in
        // breadth-first order, so a parent's links exist before its
        // children's).
        fail_.assign(states, 0);
        longest_length_.assign(states, 0);
        longest_pattern_.assign(states, NONE);
        chain_min_.assign(states, NONE);
        for (std::size_t s = 1; s < states; ++s) {
            const std::int32_t p = parent[s];
            std::int32_t f = 0;
            if (p != 0) {
                std::int32_t candidate = fail_[static_cast<std::size_t>(p)];
                while (true) {
                    const std::int32_t next = goto_edge(candidate, via[s]);
                    if (next >= 0) {
                        f = next;
                        break;
                    }
                    if (candidate == 0) break;
                    candidate = fail_[static_cast<std::size_t>(candidate)];
                }
            }
            fail_[s] = f;
            const std::size_t fs = static_cast<std::size_t>(f);
            if (own[s] != NONE) {
                longest_length_[s] = depth[s];
                longest_pattern_[s] = own[s];
            } else {
                longest_length_[s] = longest_length_[fs];
                longest_pattern_[s] = longest_pattern_[fs];
            }
            chain_min_[s] = (std::min)(own[s], chain_min_[fs]);
        }

        // Compile the DFA when it fits.
        if (states <= DENSE_LIMIT / classes_) {
            dense_.assign(states * classes_, 0);
            for (std::size_t c = 0; c < classes_; ++c) dense_[c] = root_next_[c];
            for (std::size_t s = 1; s < states; ++s) {
                const std::size_t f = static_cast<std::size_t>(fail_[s]);
                std::int32_t* row = dense_.data() + s * classes_;
                const std::int32_t* fail_row = dense_.data() + f * classes_;
                for (std::size_t c = 0; c < classes_; ++c) row[c] = fail_row[c];
                for (std::uint32_t e = edge_begin_[s]; e < edge_begin_[s + 1]; ++e)
                    row[edge_class_[e]] = edge_target_[e];
            }
        }
    }
};

#endif // FAST_STRING_AHO_CORASICK_H
