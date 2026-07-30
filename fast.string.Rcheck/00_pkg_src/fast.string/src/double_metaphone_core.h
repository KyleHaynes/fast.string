#ifndef FAST_STRING_DOUBLE_METAPHONE_CORE_H
#define FAST_STRING_DOUBLE_METAPHONE_CORE_H

#include <cctype>
#include <cstring>
#include <initializer_list>
#include <string>

// Double Metaphone (Lawrence Philips, 2000): an improvement on classic
// Metaphone that copes with names of non-English origin by emitting two
// codes per word -- primary and an alternate -- whenever the pronunciation
// is ambiguous. Ported from the structure of the well-known Apache Commons
// Codec Java implementation (the algorithm itself is the public Philips
// ruleset reimplemented by many projects; this is an independent C++
// reimplementation, cross-checked against Commons Codec's own published
// test vectors -- see test-double_metaphone.R).
namespace dm_detail {

constexpr std::size_t MAXLEN = 4;

inline bool is_vowel(char c) {
    return c=='A'||c=='E'||c=='I'||c=='O'||c=='U'||c=='Y';
}

// Every index touched by the algorithm goes through char_at()/contains(),
// both of which treat out-of-range (including the size_t wraparound from
// e.g. `index - 2` when index < 2) as simply "no match" -- mirroring the
// original's explicit start<0 checks without needing signed indices.
inline char char_at(const std::string& v, std::size_t i) {
    return i >= v.size() ? '\0' : v[i];
}

inline bool contains(const std::string& v, std::size_t start, std::size_t length,
                      std::initializer_list<const char*> opts) {
    if (start >= v.size() || start + length > v.size()) return false;
    for (const char* o : opts)
        if (v.compare(start, length, o) == 0) return true;
    return false;
}

struct Result {
    std::string primary, alternate;
    void appendPrimary(char c) { if (primary.size() < MAXLEN) primary.push_back(c); }
    void appendPrimary(const char* s) {
        std::size_t room = primary.size() < MAXLEN ? MAXLEN - primary.size() : 0;
        primary.append(s, std::min(room, std::strlen(s)));
    }
    void appendAlternate(char c) { if (alternate.size() < MAXLEN) alternate.push_back(c); }
    void appendAlternate(const char* s) {
        std::size_t room = alternate.size() < MAXLEN ? MAXLEN - alternate.size() : 0;
        alternate.append(s, std::min(room, std::strlen(s)));
    }
    void append(char c) { appendPrimary(c); appendAlternate(c); }
    void append(char p, char a) { appendPrimary(p); appendAlternate(a); }
    void append(const char* s) { appendPrimary(s); appendAlternate(s); }
    void append(const char* p, const char* a) { appendPrimary(p); appendAlternate(a); }
    bool isComplete() const { return primary.size() >= MAXLEN && alternate.size() >= MAXLEN; }
};

inline bool is_slavo_germanic(const std::string& v) {
    return v.find('W') != std::string::npos || v.find('K') != std::string::npos ||
           v.find("CZ") != std::string::npos || v.find("WITZ") != std::string::npos;
}

inline bool is_silent_start(const std::string& v) {
    return contains(v, 0, 2, {"GN", "KN", "PN", "WR", "PS"});
}

inline bool condition_c0(const std::string& v, std::size_t i) {
    if (contains(v, i, 4, {"CHIA"})) return true;
    if (i <= 1) return false;
    if (is_vowel(char_at(v, i - 2))) return false;
    if (!contains(v, i - 1, 3, {"ACH"})) return false;
    char c = char_at(v, i + 2);
    return (c != 'I' && c != 'E') || contains(v, i - 2, 6, {"BACHER", "MACHER"});
}

inline bool condition_ch0(const std::string& v, std::size_t i) {
    if (i != 0) return false;
    if (!contains(v, i + 1, 5, {"HARAC", "HARIS"}) &&
        !contains(v, i + 1, 3, {"HOR", "HYM", "HIA", "HEM"}))
        return false;
    return !contains(v, 0, 5, {"CHORE"});
}

inline bool condition_ch1(const std::string& v, std::size_t i) {
    return contains(v, 0, 4, {"VAN ", "VON "}) || contains(v, 0, 3, {"SCH"}) ||
           contains(v, i - 2, 6, {"ORCHES", "ARCHIT", "ORCHID"}) ||
           contains(v, i + 2, 1, {"T", "S"}) ||
           ((contains(v, i - 1, 1, {"A", "O", "U", "E"}) || i == 0) &&
            (contains(v, i + 2, 1, {"L", "R", "N", "M", "B", "H", "F", "V", "W", " "}) ||
             i + 1 == v.size() - 1));
}

inline bool condition_l0(const std::string& v, std::size_t i) {
    if (i == v.size() - 3 && contains(v, i - 1, 4, {"ILLO", "ILLA", "ALLE"})) return true;
    return (contains(v, v.size() - 2, 2, {"AS", "OS"}) || contains(v, v.size() - 1, 1, {"A", "O"})) &&
           contains(v, i - 1, 4, {"ALLE"});
}

inline bool condition_m0(const std::string& v, std::size_t i) {
    if (char_at(v, i + 1) == 'M') return true;
    return contains(v, i - 1, 3, {"UMB"}) &&
           (i + 1 == v.size() - 1 || contains(v, i + 2, 2, {"ER"}));
}

inline std::size_t handle_cc(const std::string& v, Result& r, std::size_t i) {
    if (contains(v, i + 2, 1, {"I", "E", "H"}) && !contains(v, i + 2, 2, {"HU"})) {
        if ((i == 1 && char_at(v, i - 1) == 'A') || contains(v, i - 1, 5, {"UCCEE", "UCCES"}))
            r.append("KS");
        else
            r.appendPrimary('X'), r.appendAlternate('X'); // == append('X')
        return i + 3;
    }
    r.append('K');
    return i + 2;
}

inline std::size_t handle_ch(const std::string& v, Result& r, std::size_t i) {
    if (i > 0 && contains(v, i, 4, {"CHAE"})) { r.append('K', 'X'); return i + 2; }
    if (condition_ch0(v, i)) { r.append('K'); return i + 2; }
    if (condition_ch1(v, i)) { r.append('K'); return i + 2; }
    if (i > 0) {
        if (contains(v, 0, 2, {"MC"})) r.append('K');
        else r.append('X', 'K');
    } else {
        r.append('X');
    }
    return i + 2;
}

inline std::size_t handle_c(const std::string& v, Result& r, std::size_t i) {
    if (condition_c0(v, i)) { r.append('K'); return i + 2; }
    if (i == 0 && contains(v, i, 6, {"CAESAR"})) { r.append('S'); return i + 2; }
    if (contains(v, i, 2, {"CH"})) return handle_ch(v, r, i);
    if (contains(v, i, 2, {"CZ"}) && !contains(v, i - 2, 4, {"WICZ"})) {
        r.append('S', 'X'); return i + 2;
    }
    if (contains(v, i + 1, 3, {"CIA"})) { r.append('X'); return i + 3; }
    if (contains(v, i, 2, {"CC"}) && !(i == 1 && char_at(v, 0) == 'M'))
        return handle_cc(v, r, i);
    if (contains(v, i, 2, {"CK", "CG", "CQ"})) { r.append('K'); return i + 2; }
    if (contains(v, i, 2, {"CI", "CE", "CY"})) {
        if (contains(v, i, 3, {"CIO", "CIE", "CIA"})) r.append('S', 'X');
        else r.append('S');
        return i + 2;
    }
    r.append('K');
    if (contains(v, i + 1, 2, {" C", " Q", " G"})) return i + 3;
    if (contains(v, i + 1, 1, {"C", "K", "Q"}) && !contains(v, i + 1, 2, {"CE", "CI"}))
        return i + 2;
    return i + 1;
}

inline std::size_t handle_d(const std::string& v, Result& r, std::size_t i) {
    if (contains(v, i, 2, {"DG"})) {
        if (contains(v, i + 2, 1, {"I", "E", "Y"})) { r.append('J'); return i + 3; }
        r.append("TK"); return i + 2;
    }
    if (contains(v, i, 2, {"DT", "DD"})) { r.append('T'); return i + 2; }
    r.append('T'); return i + 1;
}

inline std::size_t handle_gh(const std::string& v, Result& r, std::size_t i) {
    if (i > 0 && !is_vowel(char_at(v, i - 1))) { r.append('K'); return i + 2; }
    if (i == 0) {
        if (char_at(v, i + 2) == 'I') r.append('J');
        else r.append('K');
        return i + 2;
    }
    if ((i > 1 && contains(v, i - 2, 1, {"B", "H", "D"})) ||
        (i > 2 && contains(v, i - 3, 1, {"B", "H", "D"})) ||
        (i > 3 && contains(v, i - 4, 1, {"B", "H"})))
        return i + 2;
    if (i > 2 && char_at(v, i - 1) == 'U' && contains(v, i - 3, 1, {"C", "G", "L", "R", "T"}))
        r.append('F');
    else if (i > 0 && char_at(v, i - 1) != 'I')
        r.append('K');
    return i + 2;
}

inline std::size_t handle_g(const std::string& v, Result& r, std::size_t i, bool slavo) {
    if (char_at(v, i + 1) == 'H') return handle_gh(v, r, i);
    if (char_at(v, i + 1) == 'N') {
        if (i == 1 && is_vowel(char_at(v, 0)) && !slavo) r.append("KN", "N");
        else if (!contains(v, i + 2, 2, {"EY"}) && char_at(v, i + 1) != 'Y' && !slavo) r.append("N", "KN");
        else r.append("KN");
        return i + 2;
    }
    if (contains(v, i + 1, 2, {"LI"}) && !slavo) { r.append("KL", "L"); return i + 2; }
    if (i == 0 && (char_at(v, i + 1) == 'Y' ||
                   contains(v, i + 1, 2, {"ES","EP","EB","EL","EY","IB","IL","IN","IE","EI","ER"}))) {
        r.append('K', 'J'); return i + 2;
    }
    if ((contains(v, i + 1, 2, {"ER"}) || char_at(v, i + 1) == 'Y') &&
        !contains(v, 0, 6, {"DANGER", "RANGER", "MANGER"}) &&
        !contains(v, i - 1, 1, {"E", "I"}) && !contains(v, i - 1, 3, {"RGY", "OGY"})) {
        r.append('K', 'J'); return i + 2;
    }
    if (contains(v, i + 1, 1, {"E", "I", "Y"}) || contains(v, i - 1, 4, {"AGGI", "OGGI"})) {
        if (contains(v, 0, 4, {"VAN ", "VON "}) || contains(v, 0, 3, {"SCH"}) || contains(v, i + 1, 2, {"ET"}))
            r.append('K');
        else if (contains(v, i + 1, 3, {"IER"}))
            r.append('J');
        else
            r.append('J', 'K');
        return i + 2;
    }
    r.append('K');
    return (char_at(v, i + 1) == 'G') ? i + 2 : i + 1;
}

inline std::size_t handle_h(const std::string& v, Result& r, std::size_t i) {
    if ((i == 0 || is_vowel(char_at(v, i - 1))) && is_vowel(char_at(v, i + 1))) {
        r.append('H'); return i + 2;
    }
    return i + 1;
}

inline std::size_t handle_j(const std::string& v, Result& r, std::size_t i, bool slavo) {
    bool jose_like = contains(v, i, 4, {"JOSE"}) || contains(v, 0, 4, {"SAN "});
    if (jose_like) {
        if ((i == 0 && char_at(v, i + 4) == ' ') || v.size() == 4 || contains(v, 0, 4, {"SAN "}))
            r.append('H');
        else
            r.append('J', 'H');
    } else {
        if (i == 0) r.append('J', 'A');
        else if (is_vowel(char_at(v, i - 1)) && !slavo &&
                 (char_at(v, i + 1) == 'A' || char_at(v, i + 1) == 'O'))
            r.append('J', 'H');
        else if (i == v.size() - 1)
            r.append('J', ' ');
        else if (!contains(v, i + 1, 1, {"L","T","K","S","N","M","B","Z"}) &&
                 !contains(v, i - 1, 1, {"S","K","L"}))
            r.append('J');
    }
    return (char_at(v, i + 1) == 'J') ? i + 2 : i + 1;
}

inline std::size_t handle_l(const std::string& v, Result& r, std::size_t i) {
    if (char_at(v, i + 1) == 'L') {
        if (condition_l0(v, i)) r.appendPrimary('L');
        else r.append('L');
        return i + 2;
    }
    r.append('L');
    return i + 1;
}

inline std::size_t handle_p(const std::string& v, Result& r, std::size_t i) {
    if (char_at(v, i + 1) == 'H') { r.append('F'); return i + 2; }
    r.append('P');
    return contains(v, i + 1, 1, {"P", "B"}) ? i + 2 : i + 1;
}

inline std::size_t handle_r(const std::string& v, Result& r, std::size_t i, bool slavo) {
    if (i == v.size() - 1 && !slavo && contains(v, i - 2, 2, {"IE"}) && !contains(v, i - 4, 2, {"ME", "MA"}))
        r.appendAlternate('R');
    else
        r.append('R');
    return (char_at(v, i + 1) == 'R') ? i + 2 : i + 1;
}

inline std::size_t handle_sc(const std::string& v, Result& r, std::size_t i) {
    if (char_at(v, i + 2) == 'H') {
        if (contains(v, i + 3, 2, {"OO","ER","EN","UY","ED","EM"})) {
            if (contains(v, i + 3, 2, {"ER", "EN"})) r.append("X", "SK");
            else r.append("SK");
        } else if (i == 0 && !is_vowel(char_at(v, 3)) && char_at(v, 3) != 'W') {
            r.append('X', 'S');
        } else {
            r.append('X');
        }
    } else if (contains(v, i + 2, 1, {"I", "E", "Y"})) {
        r.append('S');
    } else {
        r.append("SK");
    }
    return i + 3;
}

inline std::size_t handle_s(const std::string& v, Result& r, std::size_t i, bool slavo) {
    if (contains(v, i - 1, 3, {"ISL", "YSL"})) return i + 1;
    if (i == 0 && contains(v, i, 5, {"SUGAR"})) { r.append('X', 'S'); return i + 1; }
    if (contains(v, i, 2, {"SH"})) {
        if (contains(v, i + 1, 4, {"HEIM", "HOEK", "HOLM", "HOLZ"})) r.append('S');
        else r.append('X');
        return i + 2;
    }
    if (contains(v, i, 3, {"SIO", "SIA"}) || contains(v, i, 4, {"SIAN"})) {
        if (slavo) r.append('S'); else r.append('S', 'X');
        return i + 3;
    }
    if ((i == 0 && contains(v, i + 1, 1, {"M","N","L","W"})) || contains(v, i + 1, 1, {"Z"})) {
        r.append('S', 'X');
        return contains(v, i + 1, 1, {"Z"}) ? i + 2 : i + 1;
    }
    if (contains(v, i, 2, {"SC"})) return handle_sc(v, r, i);
    if (i == v.size() - 1 && contains(v, i - 2, 2, {"AI", "OI"})) r.appendAlternate('S');
    else r.append('S');
    return contains(v, i + 1, 1, {"S", "Z"}) ? i + 2 : i + 1;
}

inline std::size_t handle_t(const std::string& v, Result& r, std::size_t i) {
    if (contains(v, i, 4, {"TION"}) || contains(v, i, 3, {"TIA", "TCH"})) { r.append('X'); return i + 3; }
    if (contains(v, i, 2, {"TH"}) || contains(v, i, 3, {"TTH"})) {
        if (contains(v, i + 2, 2, {"OM", "AM"}) || contains(v, 0, 4, {"VAN ", "VON "}) || contains(v, 0, 3, {"SCH"}))
            r.append('T');
        else
            r.append('0', 'T');
        return i + 2;
    }
    r.append('T');
    return contains(v, i + 1, 1, {"T", "D"}) ? i + 2 : i + 1;
}

inline std::size_t handle_w(const std::string& v, Result& r, std::size_t i) {
    if (contains(v, i, 2, {"WR"})) { r.append('R'); return i + 2; }
    if (i == 0 && (is_vowel(char_at(v, i + 1)) || contains(v, i, 2, {"WH"}))) {
        if (is_vowel(char_at(v, i + 1))) r.append('A', 'F');
        else r.append('A');
        return i + 1;
    }
    if ((i == v.size() - 1 && is_vowel(char_at(v, i - 1))) ||
        contains(v, i - 1, 5, {"EWSKI", "EWSKY", "OWSKI", "OWSKY"}) ||
        contains(v, 0, 3, {"SCH"})) {
        r.appendAlternate('F'); return i + 1;
    }
    if (contains(v, i, 4, {"WICZ", "WITZ"})) { r.append("TS", "FX"); return i + 4; }
    return i + 1;
}

inline std::size_t handle_x(const std::string& v, Result& r, std::size_t i) {
    if (i == 0) { r.append('S'); return i + 1; }
    if (!(i == v.size() - 1 && (contains(v, i - 3, 3, {"IAU", "EAU"}) || contains(v, i - 2, 2, {"AU", "OU"}))))
        r.append("KS");
    return contains(v, i + 1, 1, {"C", "X"}) ? i + 2 : i + 1;
}

inline std::size_t handle_z(const std::string& v, Result& r, std::size_t i, bool slavo) {
    if (char_at(v, i + 1) == 'H') { r.append('J'); return i + 2; }
    if (contains(v, i + 1, 2, {"ZO", "ZI", "ZA"}) || (slavo && i > 0 && char_at(v, i - 1) != 'T'))
        r.append("S", "TS");
    else
        r.append('S');
    return (char_at(v, i + 1) == 'Z') ? i + 2 : i + 1;
}

} // namespace dm_detail

// Cleans (trims outer whitespace, uppercases) and codes `word_in`, writing
// the primary code to `primary_out` and the alternate to `secondary_out`.
// Mirrors Commons Codec's cleanInput(): internal whitespace/punctuation is
// NOT stripped (the algorithm itself keys off literal "VAN ", "SCH", etc.,
// including the space), unlike this package's soundex()/nysiis() which
// strip down to letters only.
inline void double_metaphone_code(const std::string& word_in,
                                   std::string& primary_out, std::string& secondary_out) {
    using namespace dm_detail;
    std::size_t b = 0, e = word_in.size();
    while (b < e && std::isspace((unsigned char)word_in[b])) ++b;
    while (e > b && std::isspace((unsigned char)word_in[e - 1])) --e;
    if (b == e) { primary_out.clear(); secondary_out.clear(); return; }

    std::string v;
    v.reserve(e - b);
    for (std::size_t k = b; k < e; ++k) {
        unsigned char c = (unsigned char)word_in[k];
        v.push_back((c >= 'a' && c <= 'z') ? (char)(c - 'a' + 'A') : (char)c);
    }

    bool slavo = is_slavo_germanic(v);
    std::size_t index = is_silent_start(v) ? 1 : 0;
    Result result;

    while (!result.isComplete() && index < v.size()) {
        char c = v[index];
        switch (c) {
            case 'A': case 'E': case 'I': case 'O': case 'U': case 'Y':
                if (index == 0) result.append('A');
                index += 1;
                break;
            case 'B':
                result.append('P');
                index = (char_at(v, index + 1) == 'B') ? index + 2 : index + 1;
                break;
            case 'C':
                index = handle_c(v, result, index);
                break;
            case 'D':
                index = handle_d(v, result, index);
                break;
            case 'F':
                result.append('F');
                index = (char_at(v, index + 1) == 'F') ? index + 2 : index + 1;
                break;
            case 'G':
                index = handle_g(v, result, index, slavo);
                break;
            case 'H':
                index = handle_h(v, result, index);
                break;
            case 'J':
                index = handle_j(v, result, index, slavo);
                break;
            case 'K':
                result.append('K');
                index = (char_at(v, index + 1) == 'K') ? index + 2 : index + 1;
                break;
            case 'L':
                index = handle_l(v, result, index);
                break;
            case 'M':
                result.append('M');
                index = condition_m0(v, index) ? index + 2 : index + 1;
                break;
            case 'N':
                result.append('N');
                index = (char_at(v, index + 1) == 'N') ? index + 2 : index + 1;
                break;
            case 'P':
                index = handle_p(v, result, index);
                break;
            case 'Q':
                result.append('K');
                index = (char_at(v, index + 1) == 'Q') ? index + 2 : index + 1;
                break;
            case 'R':
                index = handle_r(v, result, index, slavo);
                break;
            case 'S':
                index = handle_s(v, result, index, slavo);
                break;
            case 'T':
                index = handle_t(v, result, index);
                break;
            case 'V':
                result.append('F');
                index = (char_at(v, index + 1) == 'V') ? index + 2 : index + 1;
                break;
            case 'W':
                index = handle_w(v, result, index);
                break;
            case 'X':
                index = handle_x(v, result, index);
                break;
            case 'Z':
                index = handle_z(v, result, index, slavo);
                break;
            default:
                index += 1;
                break;
        }
    }
    primary_out = result.primary;
    secondary_out = result.alternate;
}

#endif // FAST_STRING_DOUBLE_METAPHONE_CORE_H
