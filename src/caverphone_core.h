#ifndef FAST_STRING_CAVERPHONE_CORE_H
#define FAST_STRING_CAVERPHONE_CORE_H

#include <cctype>
#include <cstring>
#include <string>

// Caverphone 2.0 (Caversham Project, University of Otago, 2004 revision):
// a fixed chain of literal/anchored substring transforms (originally
// specified as a sequence of regex replacements) collapsing a name to a
// 10-character phonetic code. Widely used in NZ/AU electoral-roll record
// linkage. Ported step-for-step from the algorithm's reference Java
// implementation (Apache Commons Codec's Caverphone2), including its
// documented "enough"/"trough" duplicate-rule quirk inherited from the
// original published spec -- preserved here for output fidelity rather
// than "corrected", since the point is to match the standard.
//
// The chain runs around fifty transforms over every name, and most of them
// do not fire: a name holds a handful of the letters they look for. Each
// one therefore starts with a find() and returns before touching the
// string when there is nothing to do, rewrites in place when the
// replacement is the same width as what it replaces, and otherwise builds
// into a caller-owned scratch buffer that is reused for the whole vector.
// Together these turn roughly sixty heap allocations per name into none.

namespace caverphone_detail {

// Letters only, lower-cased, compacted in place.
inline void keep_lower_az(std::string& s) {
    std::size_t out = 0;
    for (std::size_t i = 0; i < s.size(); ++i) {
        char c = s[i];
        if (c >= 'A' && c <= 'Z') c = static_cast<char>(c - 'A' + 'a');
        if (c >= 'a' && c <= 'z') s[out++] = c;
    }
    s.resize(out);
}

// Every occurrence of `from` becomes `to`. `scratch` is only touched when
// the two differ in width and `from` actually occurs.
inline void replace_all(std::string& s, const char* from, const char* to,
                        std::string& scratch) {
    const std::size_t flen = std::strlen(from);
    if (flen == 0 || s.size() < flen) return;
    std::size_t hit = s.find(from, 0);
    if (hit == std::string::npos) return;

    const std::size_t tlen = std::strlen(to);
    if (tlen == flen) {
        do {
            std::memcpy(&s[hit], to, tlen);
            hit = s.find(from, hit + flen);
        } while (hit != std::string::npos);
        return;
    }
    if (tlen == 0) {  // a deletion: compact in place
        std::size_t out = hit;
        std::size_t i = hit + flen;
        while (i < s.size()) {
            if (s.compare(i, flen, from) == 0) {
                i += flen;
            } else {
                s[out++] = s[i++];
            }
        }
        s.resize(out);
        return;
    }
    scratch.clear();
    scratch.reserve(s.size() + 8);
    std::size_t pos = 0;
    while (true) {
        scratch.append(s, pos, hit - pos);
        scratch.append(to, tlen);
        pos = hit + flen;
        hit = s.find(from, pos);
        if (hit == std::string::npos) {
            scratch.append(s, pos, std::string::npos);
            break;
        }
    }
    s.swap(scratch);
}

inline void replace_prefix(std::string& s, const char* prefix,
                           const char* to) {
    const std::size_t plen = std::strlen(prefix);
    if (s.size() >= plen && s.compare(0, plen, prefix) == 0)
        s.replace(0, plen, to);
}

inline void replace_suffix(std::string& s, const char* suffix,
                           const char* to) {
    const std::size_t slen = std::strlen(suffix);
    if (s.size() >= slen && s.compare(s.size() - slen, slen, suffix) == 0)
        s.replace(s.size() - slen, slen, to);
}

inline void replace_leading_vowel(std::string& s) {
    if (s.empty()) return;
    const char c = s[0];
    if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') s[0] = 'A';
}

inline void replace_vowels(std::string& s) {
    for (char& c : s)
        if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') c = '3';
}

// A run of `c` becomes a single `upper`. Shrinking only, so in place: the
// run is stepped over before `upper` is written, which keeps the write
// index strictly behind the read index.
inline void collapse_runs(std::string& s, char c, char upper) {
    const std::size_t hit = s.find(c);
    if (hit == std::string::npos) return;
    std::size_t out = hit;
    std::size_t i = hit;
    const std::size_t n = s.size();
    while (i < n) {
        if (s[i] == c) {
            while (i < n && s[i] == c) ++i;
            s[out++] = upper;
        } else {
            s[out++] = s[i++];
        }
    }
    s.resize(out);
}

}  // namespace caverphone_detail

// Writes the 10-character code for `input` into `code`. `scratch` is a
// working buffer the caller keeps across elements; its contents on entry
// and exit are not meaningful.
inline void caverphone2_code(const char* input, std::size_t length,
                             std::string& code, std::string& scratch) {
    using namespace caverphone_detail;
    std::string& txt = code;
    txt.assign(input, length);
    keep_lower_az(txt);
    replace_suffix(txt, "e", "");

    replace_prefix(txt, "cough", "cou2f");
    replace_prefix(txt, "rough", "rou2f");
    replace_prefix(txt, "tough", "tou2f");
    replace_prefix(txt, "enough", "enou2f");
    replace_prefix(txt, "trough", "trou2f");
    replace_prefix(txt, "gn", "2n");

    replace_suffix(txt, "mb", "m2");

    replace_all(txt, "cq", "2q", scratch);
    replace_all(txt, "ci", "si", scratch);
    replace_all(txt, "ce", "se", scratch);
    replace_all(txt, "cy", "sy", scratch);
    replace_all(txt, "tch", "2ch", scratch);
    replace_all(txt, "c", "k", scratch);
    replace_all(txt, "q", "k", scratch);
    replace_all(txt, "x", "k", scratch);
    replace_all(txt, "v", "f", scratch);
    replace_all(txt, "dg", "2g", scratch);
    replace_all(txt, "tio", "sio", scratch);
    replace_all(txt, "tia", "sia", scratch);
    replace_all(txt, "d", "t", scratch);
    replace_all(txt, "ph", "fh", scratch);
    replace_all(txt, "b", "p", scratch);
    replace_all(txt, "sh", "s2", scratch);
    replace_all(txt, "z", "s", scratch);

    replace_leading_vowel(txt);
    replace_vowels(txt);

    replace_all(txt, "j", "y", scratch);
    replace_prefix(txt, "y3", "Y3");
    replace_prefix(txt, "y", "A");
    replace_all(txt, "y", "3", scratch);

    replace_all(txt, "3gh3", "3kh3", scratch);
    replace_all(txt, "gh", "22", scratch);
    replace_all(txt, "g", "k", scratch);

    collapse_runs(txt, 's', 'S');
    collapse_runs(txt, 't', 'T');
    collapse_runs(txt, 'p', 'P');
    collapse_runs(txt, 'k', 'K');
    collapse_runs(txt, 'f', 'F');
    collapse_runs(txt, 'm', 'M');
    collapse_runs(txt, 'n', 'N');

    replace_all(txt, "w3", "W3", scratch);
    replace_all(txt, "wh3", "Wh3", scratch);
    replace_suffix(txt, "w", "3");
    replace_all(txt, "w", "2", scratch);

    replace_prefix(txt, "h", "A");
    replace_all(txt, "h", "2", scratch);

    replace_all(txt, "r3", "R3", scratch);
    replace_suffix(txt, "r", "3");
    replace_all(txt, "r", "2", scratch);

    replace_all(txt, "l3", "L3", scratch);
    replace_suffix(txt, "l", "3");
    replace_all(txt, "l", "2", scratch);

    replace_all(txt, "2", "", scratch);
    replace_suffix(txt, "3", "A");
    replace_all(txt, "3", "", scratch);

    txt.append("1111111111");
    txt.resize(10);
}

inline std::string caverphone2_code(const std::string& word_in) {
    std::string code, scratch;
    caverphone2_code(word_in.data(), word_in.size(), code, scratch);
    return code;
}

#endif  // FAST_STRING_CAVERPHONE_CORE_H
