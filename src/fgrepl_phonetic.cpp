// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <string>
#include <vector>
#include "double_metaphone_core.h"
#include "caverphone_core.h"
using namespace Rcpp;
using namespace RcppParallel;

// ASCII-only helpers — names/addresses in record-linkage data are
// overwhelmingly ASCII letters; non-letters are simply skipped, matching
// how ChartrWorker treats input as a flat byte table rather than going
// through locale-dependent <cctype>.
static inline bool is_alpha_ascii(unsigned char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}
static inline char to_upper_ascii(unsigned char c) {
    return (c >= 'a' && c <= 'z') ? (char)(c - 'a' + 'A') : (char)c;
}
static inline bool is_vowel(char c) {
    return c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U';
}

// ---------------------------------------------------------------------------
// Soundex (Odell-Russell / American Soundex, US Census Bureau rules).
// First letter kept verbatim; remaining consonants mapped to digits 1-6;
// adjacent letters sharing a digit collapse to one (H/W are transparent —
// they don't break adjacency, e.g. "Ashcraft" -> A261, not A2261); padded
// or truncated to exactly 4 characters (1 letter + 3 digits).
// ---------------------------------------------------------------------------

static inline int soundex_digit(char c) {
    switch (c) {
        case 'B': case 'F': case 'P': case 'V': return 1;
        case 'C': case 'G': case 'J': case 'K': case 'Q':
        case 'S': case 'X': case 'Z': return 2;
        case 'D': case 'T': return 3;
        case 'L': return 4;
        case 'M': case 'N': return 5;
        case 'R': return 6;
        default: return 0; // vowels (A E I O U Y) reset adjacency
    }
}

static bool soundex_code(const char* s, int len, std::string& out) {
    int i = 0;
    while (i < len && !is_alpha_ascii((unsigned char)s[i])) ++i;
    if (i >= len) return false;

    char first = to_upper_ascii((unsigned char)s[i]);
    char buf[4] = { first, '0', '0', '0' };
    int out_len = 1;
    int last_digit = soundex_digit(first);

    for (++i; i < len && out_len < 4; ++i) {
        unsigned char uc = (unsigned char)s[i];
        if (!is_alpha_ascii(uc)) continue;
        char c = to_upper_ascii(uc);
        if (c == 'H' || c == 'W') continue; // transparent: skip, keep last_digit
        int d = soundex_digit(c);
        if (d != 0 && d != last_digit) buf[out_len++] = (char)('0' + d);
        last_digit = d;
    }
    out.assign(buf, 4);
    return true;
}

struct SoundexWorker : public Worker {
    SEXP x_sexp;
    std::vector<std::string>& results;
    std::vector<uint8_t>& is_na;

    SoundexWorker(SEXP x, std::vector<std::string>& r, std::vector<uint8_t>& na)
        : x_sexp(x), results(r), is_na(na) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { is_na[i] = 1; continue; }
            if (!soundex_code(CHAR(elem), LENGTH(elem), results[i])) is_na[i] = 1;
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_soundex_impl(const StringVector& x) {
    R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);
    std::vector<uint8_t> is_na((std::size_t)n, 0);
    SoundexWorker worker(x, results, is_na);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (is_na[(std::size_t)i])
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i, Rf_mkChar(results[(std::size_t)i].c_str()));
    }
    return result;
}

// ---------------------------------------------------------------------------
// NYSIIS (1970 New York State Identification and Intelligence System).
// Letters-only working copy; leading/trailing transforms; per-letter
// transform table building a key with adjacent-duplicate collapsing;
// trailing key cleanup (drop S, AY->Y, drop A); truncated to 6 chars.
// This implements the commonly-cited core ruleset, not a byte-for-byte
// port of any single reference library.
// ---------------------------------------------------------------------------

static inline void nysiis_push(char t, std::string& key, char& last_added) {
    if (t != last_added) { key.push_back(t); last_added = t; }
}

static bool nysiis_code(const char* s, int len, std::string& out) {
    std::string w;
    w.reserve((std::size_t)len);
    for (int i = 0; i < len; ++i) {
        unsigned char c = (unsigned char)s[i];
        if (is_alpha_ascii(c)) w.push_back(to_upper_ascii(c));
    }
    if (w.empty()) return false;

    // Leading transforms.
    if (w.size() >= 3 && w.compare(0, 3, "MAC") == 0) w.replace(0, 3, "MCC");
    else if (w.size() >= 2 && w.compare(0, 2, "KN") == 0) w.replace(0, 2, "NN");
    else if (w[0] == 'K') w[0] = 'C';
    if (w.size() >= 2 && (w.compare(0, 2, "PH") == 0 || w.compare(0, 2, "PF") == 0))
        w.replace(0, 2, "FF");
    else if (w.size() >= 3 && w.compare(0, 3, "SCH") == 0) w.replace(0, 3, "SSS");

    // Trailing transforms.
    std::size_t n = w.size();
    if (n >= 2) {
        std::string tail2 = w.substr(n - 2);
        if (tail2 == "EE" || tail2 == "IE") w.replace(n - 2, 2, "Y");
    }
    n = w.size();
    if (n >= 2) {
        std::string tail2 = w.substr(n - 2);
        if (tail2 == "DT" || tail2 == "RT" || tail2 == "RD" ||
            tail2 == "NT" || tail2 == "ND")
            w.replace(n - 2, 2, "D");
    }

    std::string key;
    key.push_back(w[0]);
    char last_added = w[0];

    n = w.size();
    for (std::size_t i = 1; i < n; ) {
        char c = w[i];
        char prev = w[i - 1];
        char next = (i + 1 < n) ? w[i + 1] : '\0';

        if (c == 'E' && next == 'V') {
            nysiis_push('A', key, last_added);
            nysiis_push('F', key, last_added);
            i += 2;
            continue;
        }

        char t;
        if (is_vowel(c))            t = 'A';
        else if (c == 'Q')          t = 'G';
        else if (c == 'Z')          t = 'S';
        else if (c == 'M')          t = 'N';
        else if (c == 'K')          t = (next == 'N') ? 'N' : 'C';
        else if (c == 'H')          t = (!is_vowel(prev) || (next != '\0' && !is_vowel(next))) ? prev : c;
        else if (c == 'W' && is_vowel(prev)) t = 'A';
        else                        t = c;

        nysiis_push(t, key, last_added);
        ++i;
    }

    // Trailing key cleanup — guard sizes so we never strip down to nothing.
    if (key.size() > 1 && key.back() == 'S') key.pop_back();
    if (key.size() >= 2 && key[key.size() - 2] == 'A' && key.back() == 'Y')
        key.erase(key.size() - 2, 1);
    if (key.size() > 1 && key.back() == 'A') key.pop_back();

    if (key.size() > 6) key.resize(6);
    out = key;
    return true;
}

struct NysiisWorker : public Worker {
    SEXP x_sexp;
    std::vector<std::string>& results;
    std::vector<uint8_t>& is_na;

    NysiisWorker(SEXP x, std::vector<std::string>& r, std::vector<uint8_t>& na)
        : x_sexp(x), results(r), is_na(na) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { is_na[i] = 1; continue; }
            if (!nysiis_code(CHAR(elem), LENGTH(elem), results[i])) is_na[i] = 1;
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_nysiis_impl(const StringVector& x) {
    R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);
    std::vector<uint8_t> is_na((std::size_t)n, 0);
    NysiisWorker worker(x, results, is_na);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (is_na[(std::size_t)i])
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i, Rf_mkChar(results[(std::size_t)i].c_str()));
    }
    return result;
}

// ---------------------------------------------------------------------------
// Double Metaphone (Lawrence Philips). Computed once per element, yielding
// both the primary and alternate code in a single pass (see
// double_metaphone_core.h for the port and its validation against Apache
// Commons Codec's published test vectors).
// ---------------------------------------------------------------------------

struct DoubleMetaphoneWorker : public Worker {
    SEXP x_sexp;
    std::vector<std::string>& primary;
    std::vector<std::string>& secondary;
    std::vector<uint8_t>& is_na;

    DoubleMetaphoneWorker(SEXP x, std::vector<std::string>& p, std::vector<std::string>& s,
                           std::vector<uint8_t>& na)
        : x_sexp(x), primary(p), secondary(s), is_na(na) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { is_na[i] = 1; continue; }
            std::string word(CHAR(elem), (std::size_t)LENGTH(elem));
            double_metaphone_code(word, primary[i], secondary[i]);
            if (primary[i].empty()) is_na[i] = 1;
        }
    }
};

// [[Rcpp::export]]
List fast_double_metaphone_impl(const StringVector& x) {
    R_xlen_t n = x.size();
    std::vector<std::string> primary((std::size_t)n), secondary((std::size_t)n);
    std::vector<uint8_t> is_na((std::size_t)n, 0);
    DoubleMetaphoneWorker worker(x, primary, secondary, is_na);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    CharacterVector primary_out(n), secondary_out(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (is_na[(std::size_t)i]) {
            SET_STRING_ELT(primary_out, i, NA_STRING);
            SET_STRING_ELT(secondary_out, i, NA_STRING);
        } else {
            SET_STRING_ELT(primary_out, i, Rf_mkChar(primary[(std::size_t)i].c_str()));
            SET_STRING_ELT(secondary_out, i, Rf_mkChar(secondary[(std::size_t)i].c_str()));
        }
    }
    return List::create(Named("primary") = primary_out, Named("secondary") = secondary_out);
}

// ---------------------------------------------------------------------------
// Caverphone 2.0 (Caversham Project, University of Otago). Always produces
// a 10-character code; see caverphone_core.h.
// ---------------------------------------------------------------------------

struct CaverphoneWorker : public Worker {
    SEXP x_sexp;
    std::vector<std::string>& results;
    std::vector<uint8_t>& is_na;

    CaverphoneWorker(SEXP x, std::vector<std::string>& r, std::vector<uint8_t>& na)
        : x_sexp(x), results(r), is_na(na) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { is_na[i] = 1; continue; }
            results[i] = caverphone2_code(std::string(CHAR(elem), (std::size_t)LENGTH(elem)));
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_caverphone_impl(const StringVector& x) {
    R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);
    std::vector<uint8_t> is_na((std::size_t)n, 0);
    CaverphoneWorker worker(x, results, is_na);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (is_na[(std::size_t)i])
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i, Rf_mkChar(results[(std::size_t)i].c_str()));
    }
    return result;
}
