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

namespace caverphone_detail {

inline void remove_non_az(std::string& s) {
    std::string out;
    out.reserve(s.size());
    for (char c : s) if (c >= 'a' && c <= 'z') out.push_back(c);
    s.swap(out);
}

inline void replace_all(std::string& s, const char* from, const char* to) {
    std::size_t flen = std::strlen(from);
    if (flen == 0 || s.size() < flen) return;
    std::string out;
    out.reserve(s.size());
    std::size_t pos = 0;
    while (true) {
        std::size_t hit = s.find(from, pos);
        if (hit == std::string::npos) { out.append(s, pos, std::string::npos); break; }
        out.append(s, pos, hit - pos);
        out += to;
        pos = hit + flen;
    }
    s.swap(out);
}

inline void replace_prefix(std::string& s, const char* prefix, const char* to) {
    std::size_t plen = std::strlen(prefix);
    if (s.size() >= plen && s.compare(0, plen, prefix) == 0)
        s = std::string(to) + s.substr(plen);
}

inline void replace_suffix(std::string& s, const char* suffix, const char* to) {
    std::size_t slen = std::strlen(suffix);
    if (s.size() >= slen && s.compare(s.size() - slen, slen, suffix) == 0)
        s = s.substr(0, s.size() - slen) + to;
}

inline void replace_leading_vowel(std::string& s) {
    if (!s.empty()) {
        char c = s[0];
        if (c=='a'||c=='e'||c=='i'||c=='o'||c=='u') s = "A" + s.substr(1);
    }
}

inline void replace_vowels(std::string& s) {
    for (char& c : s) if (c=='a'||c=='e'||c=='i'||c=='o'||c=='u') c = '3';
}

inline void collapse_runs(std::string& s, char c, char upper) {
    std::string out;
    out.reserve(s.size());
    for (std::size_t i = 0; i < s.size(); ++i) {
        if (s[i] == c) {
            out.push_back(upper);
            while (i + 1 < s.size() && s[i + 1] == c) ++i;
        } else {
            out.push_back(s[i]);
        }
    }
    s.swap(out);
}

} // namespace caverphone_detail

inline std::string caverphone2_code(const std::string& word_in) {
    using namespace caverphone_detail;
    std::string txt;
    txt.reserve(word_in.size());
    for (char c : word_in) txt.push_back((char)std::tolower((unsigned char)c));
    remove_non_az(txt);
    replace_suffix(txt, "e", "");

    replace_prefix(txt, "cough", "cou2f");
    replace_prefix(txt, "rough", "rou2f");
    replace_prefix(txt, "tough", "tou2f");
    replace_prefix(txt, "enough", "enou2f");
    replace_prefix(txt, "trough", "trou2f");
    replace_prefix(txt, "gn", "2n");

    replace_suffix(txt, "mb", "m2");

    replace_all(txt, "cq", "2q");
    replace_all(txt, "ci", "si");
    replace_all(txt, "ce", "se");
    replace_all(txt, "cy", "sy");
    replace_all(txt, "tch", "2ch");
    replace_all(txt, "c", "k");
    replace_all(txt, "q", "k");
    replace_all(txt, "x", "k");
    replace_all(txt, "v", "f");
    replace_all(txt, "dg", "2g");
    replace_all(txt, "tio", "sio");
    replace_all(txt, "tia", "sia");
    replace_all(txt, "d", "t");
    replace_all(txt, "ph", "fh");
    replace_all(txt, "b", "p");
    replace_all(txt, "sh", "s2");
    replace_all(txt, "z", "s");

    replace_leading_vowel(txt);
    replace_vowels(txt);

    replace_all(txt, "j", "y");
    replace_prefix(txt, "y3", "Y3");
    replace_prefix(txt, "y", "A");
    replace_all(txt, "y", "3");

    replace_all(txt, "3gh3", "3kh3");
    replace_all(txt, "gh", "22");
    replace_all(txt, "g", "k");

    collapse_runs(txt, 's', 'S');
    collapse_runs(txt, 't', 'T');
    collapse_runs(txt, 'p', 'P');
    collapse_runs(txt, 'k', 'K');
    collapse_runs(txt, 'f', 'F');
    collapse_runs(txt, 'm', 'M');
    collapse_runs(txt, 'n', 'N');

    replace_all(txt, "w3", "W3");
    replace_all(txt, "wh3", "Wh3");
    replace_suffix(txt, "w", "3");
    replace_all(txt, "w", "2");

    replace_prefix(txt, "h", "A");
    replace_all(txt, "h", "2");

    replace_all(txt, "r3", "R3");
    replace_suffix(txt, "r", "3");
    replace_all(txt, "r", "2");

    replace_all(txt, "l3", "L3");
    replace_suffix(txt, "l", "3");
    replace_all(txt, "l", "2");

    replace_all(txt, "2", "");
    replace_suffix(txt, "3", "A");
    replace_all(txt, "3", "");

    txt += "1111111111";
    txt.resize(10);
    return txt;
}

#endif // FAST_STRING_CAVERPHONE_CORE_H
