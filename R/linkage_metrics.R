#' Ratcliff/Obershelp gestalt similarity
#'
#' Measures how much of two strings can be matched up by repeatedly taking
#' their longest common substring and then doing the same again on what is
#' left to the left and to the right of it. The similarity is twice the total
#' matched length divided by the sum of the two lengths, so it is `1` for
#' identical strings and `0` when they share no character.
#'
#' Unlike an edit distance, gestalt matching rewards long runs of agreement
#' and is insensitive to how far apart they are, which suits comparing names
#' and addresses whose parts have moved. It is the measure behind Python's
#' `difflib.SequenceMatcher.ratio()` (with junk heuristics off), and is also
#' known as the Ratcliff/Obershelp pattern-recognition similarity.
#' `ratcliff_obershelp_distance()` is `1 - ratcliff_obershelp()`.
#'
#' Two empty strings score `1`. Scoring is pair by pair in parallel across CPU
#' cores via Intel TBB.
#'
#' @param a,b Equal-length character vectors.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @param use_bytes Logical. Compare UTF-8 code points (`FALSE`, the default)
#'   or bytes (`TRUE`).
#' @return Numeric vector the same length as `a`; `NA` where either input is
#'   `NA`.
#' @references Ratcliff, J. W. and Metzener, D. E. (1988). Pattern matching:
#'   the gestalt approach. Dr. Dobb's Journal, 13(7), 46-51.
#' @seealso [lcs_str_similarity()] for the longest common substring alone,
#'   [bag_similarity()] for a cheaper upper bound on agreement, and
#'   [jaro_winkler()] for the usual name comparator.
#' @family record linkage comparators
#' @examples
#' ratcliff_obershelp("GEORGE SMITH", "SMITH GEORGE")
#' ratcliff_obershelp("INDOOROOPILLY", "INDOOROPILLY")
#' ratcliff_obershelp_distance(c("MACDONALD", "CHOWDHURY"),
#'                             c("MCDONALD", "CHOUDHURY"))
#' @name ratcliff_obershelp
NULL

#' @rdname ratcliff_obershelp
#' @export
ratcliff_obershelp <- function(a, b, nthreads = NULL, use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    fast_seq_metric_impl(a, b, 0L, use_bytes, .as_nthreads(nthreads))
}

#' @rdname ratcliff_obershelp
#' @export
ratcliff_obershelp_distance <- function(a, b, nthreads = NULL,
                                        use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    fast_seq_metric_impl(a, b, 1L, use_bytes, .as_nthreads(nthreads))
}

#' Bag distance
#'
#' Treats each string as a bag (multiset) of characters and returns
#' `max(|A - B|, |B - A|)`: how many characters one string has too many of,
#' or too few of, whichever is worse. Order is ignored entirely, so
#' `"SMITH"` and `"HTIMS"` are at distance `0`.
#'
#' The bag distance is a lower bound on the Levenshtein distance that costs a
#' sort rather than a dynamic-programming matrix, which is why record-linkage
#' systems use it to throw out hopeless pairs before running a real edit
#' distance (`levenshtein_within()` does the same job exactly, at more cost
#' per pair). `bag_similarity()` is `1 - bag_distance / max(nchar(a),
#' nchar(b))`; two empty strings score `1`.
#'
#' @inheritParams ratcliff_obershelp
#' @return Numeric vector the same length as `a`: a count of characters for
#'   `bag_distance()`, a similarity in `[0, 1]` for `bag_similarity()`. `NA`
#'   where either input is `NA`.
#' @references Bartolini, I., Ciaccia, P. and Patella, M. (2002). String
#'   matching with metric trees using an approximate distance. SPIRE 2002,
#'   271-283.
#' @seealso [levenshtein()], which the bag distance bounds from below, and
#'   [levenshtein_within()] for an exact early-exit test.
#' @family record linkage comparators
#' @examples
#' # The multiset of characters, so any reordering is free: useful as a
#' # cheap pre-filter, useless as a verdict -- "SMITH" and "HTIMS" are not
#' # the same surname.
#' bag_distance("SMITH", "HTIMS")
#'
#' # It is a lower bound on the Levenshtein distance, which is what makes
#' # it a filter: a pair bag distance rules out, Levenshtein would too.
#' bag_distance("MACDONALD", "MCDONALD")
#' levenshtein("MACDONALD", "MCDONALD")   # never smaller
#' bag_similarity(c("JOHNSON", "ROBERT"), c("JONSON", "ROBERTA"))
#' @name bag_distance
NULL

#' @rdname bag_distance
#' @export
bag_distance <- function(a, b, nthreads = NULL, use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    fast_seq_metric_impl(a, b, 2L, use_bytes, .as_nthreads(nthreads))
}

#' @rdname bag_distance
#' @export
bag_similarity <- function(a, b, nthreads = NULL, use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    fast_seq_metric_impl(a, b, 3L, use_bytes, .as_nthreads(nthreads))
}

#' Needleman-Wunsch global alignment
#'
#' Scores the best end-to-end alignment of two strings, awarding `match` for
#' every aligned pair of equal characters, `mismatch` for every aligned pair
#' that differs, and `gap` for every character left unaligned. Unlike
#' [smith_waterman()], which finds the best-scoring *part* of each string,
#' the Needleman-Wunsch alignment must consume both strings in full, which is
#' what makes it a whole-field comparator rather than a substring search.
#'
#' `needleman_wunsch()` returns the alignment score, which depends on the
#' scores chosen and on how long the strings are.
#' `needleman_wunsch_similarity()` puts it on a `[0, 1]` scale as
#' `(score - worst) / (best - worst)`, where `best` is what the longer string
#' scores against a copy of itself -- so only identical strings reach `1` --
#' and `worst` is the cheaper of aligning the two as mismatches and of gapping
#' both away entirely. Two empty strings score `1`.
#'
#' @inheritParams ratcliff_obershelp
#' @param match Score for an aligned pair of equal characters (default `1`).
#' @param mismatch Score for an aligned pair that differs (default `-1`).
#' @param gap Score for a character aligned against a gap (default `-1`).
#'   Gaps are scored one character at a time; [smith_waterman()] has the
#'   affine (open plus extend) form.
#' @return Numeric vector the same length as `a`; `NA` where either input is
#'   `NA`.
#' @references Needleman, S. B. and Wunsch, C. D. (1970). A general method
#'   applicable to the search for similarities in the amino acid sequence of
#'   two proteins. Journal of Molecular Biology, 48(3), 443-453.
#' @seealso [smith_waterman()] for local alignment with affine gaps, and
#'   [levenshtein()], which is the same dynamic program with `match = 0`,
#'   `mismatch = gap = -1`, minimised instead of maximised.
#' @family record linkage comparators
#' @examples
#' needleman_wunsch("MACDONALD", "MCDONALD")
#' needleman_wunsch_similarity("MACDONALD", "MCDONALD")
#'
#' # A gap costs less than a mismatch, so abbreviations align through
#' needleman_wunsch_similarity("ROBERT", "RBT", gap = -0.5)
#' @name needleman_wunsch
NULL

.one_finite_number <- function(value, name) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value))
        stop(sprintf("`%s` must be a single finite number.", name))
    as.double(value)
}

.needleman_scores <- function(match, mismatch, gap)
    c(.one_finite_number(match, "match"),
      .one_finite_number(mismatch, "mismatch"),
      .one_finite_number(gap, "gap"))

#' @rdname needleman_wunsch
#' @export
needleman_wunsch <- function(a, b, match = 1, mismatch = -1, gap = -1,
                             nthreads = NULL, use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    s <- .needleman_scores(match, mismatch, gap)
    fast_needleman_wunsch_impl(a, b, s[1L], s[2L], s[3L], FALSE, use_bytes,
                               .as_nthreads(nthreads))
}

#' @rdname needleman_wunsch
#' @export
needleman_wunsch_similarity <- function(a, b, match = 1, mismatch = -1,
                                        gap = -1, nthreads = NULL,
                                        use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    s <- .needleman_scores(match, mismatch, gap)
    fast_needleman_wunsch_impl(a, b, s[1L], s[2L], s[3L], TRUE, use_bytes,
                               .as_nthreads(nthreads))
}

#' Eudex phonetic hash
#'
#' A locality-sensitive phonetic hash: instead of a code that either matches
#' or does not, as [soundex()] and [nysiis()] give, `eudex()` returns a
#' 64-bit value whose *bits* encode how each sound is articulated -- nasal,
#' fricative, plosive, dental, liquid, labial -- so that names which sound
#' alike differ in few bits. `eudex_distance()` measures that difference, and
#' is a phonetic comparator rather than a blocking key.
#'
#' The hash keeps the first letter's code in its own top byte, because the
#' start of a name is both the most reliable part of it and the part a
#' phonetic key most needs to agree on. `eudex_distance()` weights each byte
#' of the difference accordingly: the Hamming weight of the top byte counts
#' 34 times, the next 21, then 13, 8, 5, 3, 2 and 1. The Rust crate this
#' ports calls two words similar when that distance is below 15.
#'
#' Only ASCII letters are read; digits, spaces, punctuation and accented
#' letters are skipped, so pass names through [ascii_fold()] first if they
#' may carry accents. Case does not matter.
#'
#' @param x Character vector of words -- one name or one token each, not a
#'   whole field: the hash has room for the first letter and up to eight
#'   sounds after it, so `"JOHN SMITH"` is hashed as the single word
#'   `"JOHNSMITH"` and the surname barely reaches the hash.
#' @param a,b Equal-length character vectors of words.
#' @param method `"graduated"` (the default) weights each byte of the
#'   difference as described above; `"hamming"` counts every differing bit
#'   once.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return `eudex()`: character vector of 16 lowercase hex digits, `NA` where
#'   `x` is `NA`. `eudex_distance()`: numeric vector the same length as `a`,
#'   `0` when the hashes agree and at most `696` (`"graduated"`) or `64`
#'   (`"hamming"`), `NA` where either input is `NA`.
#' @source Ports the Rust crate `eudex`
#'   (<https://github.com/ticki/eudex>), whose phone tables and
#'   byte weights are reproduced in `src/linkage_metrics.cpp`.
#' @seealso [soundex()] and the other [phonetic codes][soundex] for blocking
#'   keys that match exactly, and [editex()] for a phonetic edit distance.
#' @family record linkage comparators
#' @examples
#' # A blocking key that puts sound-alikes in the same block without the
#' # information loss of a four-character Soundex code.
#' eudex(c("JOHN", "JON", "SMITH", "SMYTH"))
#'
#' # Sound-alikes are a short distance apart, unrelated names a long one
#' eudex_distance(c("JOHN", "SMITH", "JOHN"), c("JON", "SMYTH", "MARGARET"))
#'
#' # Under 15 is the crate's own threshold for "similar". Note where it
#' # breaks down: the first letter gets its own byte and the smallest
#' # positional weight is spent on the last sound, so a name that varies at
#' # the *front* -- CATHERINE against KATHRYN -- is scored as far apart
#' # however alike the rest of it sounds. Block on eudex by all means, but
#' # pair it with a comparator that does not privilege the first letter.
#' eudex_distance(c("CATHERINE", "SIOBHAN"), c("KATHRYN", "SHIVAUN"))
#' eudex_distance(c("CATHERINE", "SIOBHAN"), c("KATHRYN", "SHIVAUN")) < 15
#' @name eudex
NULL

#' @rdname eudex
#' @export
eudex <- function(x, nthreads = NULL) {
    if (!is.character(x)) stop("`x` must be a character vector.")
    fast_eudex_impl(x, .as_nthreads(nthreads))
}

#' @rdname eudex
#' @export
eudex_distance <- function(a, b, method = c("graduated", "hamming"),
                           nthreads = NULL) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
    method <- match.arg(method)
    fast_eudex_distance_impl(a, b, method == "graduated",
                             .as_nthreads(nthreads))
}

#' Great-circle distance between coordinates
#'
#' The haversine distance between two WGS84 latitude/longitude positions:
#' how far apart two geocoded addresses are over the surface of the Earth,
#' taken as a sphere. In record linkage it turns a pair of geocodes into one
#' number that [numeric_compare()] can score, which discriminates between
#' households far better than comparing address text does.
#'
#' Latitudes and longitudes are in degrees. The spherical approximation is
#' within about 0.5% of the ellipsoidal (Vincenty) distance, which is well
#' inside the error of address geocoding itself.
#'
#' @param lat_1,lon_1 Numeric vectors: the first position, in degrees.
#' @param lat_2,lon_2 Numeric vectors: the second position, in degrees.
#'   Recycled against the first if length 1.
#' @param unit `"km"` (the default), `"m"`, `"mi"` (statute miles) or
#'   `"nm"` (nautical miles).
#' @param radius Earth radius to use, in `unit`s. Defaults to the mean radius
#'   6371.0088 km, converted.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Numeric vector of distances, `NA` where any coordinate is `NA`.
#' @seealso [numeric_compare()] to turn a distance into a `[0, 1]` agreement
#'   score, and [block_pairs()] for blocking on a rounded coordinate or a
#'   geohash prefix.
#' @family record linkage comparators
#' @examples
#' # Brisbane to Sydney
#' geo_distance(-27.4698, 153.0251, -33.8688, 151.2093)
#'
#' # Two geocodes of the same street address
#' geo_distance(c(-27.4698, -37.8136), c(153.0251, 144.9631),
#'              c(-27.4701, -37.8140), c(153.0249, 144.9635), unit = "m")
#' @export
geo_distance <- function(lat_1, lon_1, lat_2, lon_2,
                         unit = c("km", "m", "mi", "nm"), radius = NULL,
                         nthreads = NULL) {
    unit <- match.arg(unit)
    args <- list(lat_1 = lat_1, lon_1 = lon_1, lat_2 = lat_2, lon_2 = lon_2)
    for (nm in names(args))
        if (!is.numeric(args[[nm]]))
            stop(sprintf("`%s` must be a numeric vector.", nm))
    n <- max(lengths(args))
    for (nm in names(args)) {
        len <- length(args[[nm]])
        if (len != n) {
            if (len != 1L)
                stop("Coordinate vectors must have the same length, or length 1.")
            args[[nm]] <- rep(args[[nm]], n)
        }
    }
    if (is.null(radius)) {
        radius <- switch(unit, km = 6371.0088, m = 6371008.8,
                         mi = 3958.7613, nm = 3440.0695)
    } else if (!is.numeric(radius) || length(radius) != 1L ||
               !is.finite(radius) || radius <= 0) {
        stop("`radius` must be a single positive number.")
    }
    fast_geo_distance_impl(as.double(args$lat_1), as.double(args$lon_1),
                           as.double(args$lat_2), as.double(args$lon_2),
                           as.double(radius), .as_nthreads(nthreads))
}

#' Q-gram overlap coefficient
#'
#' The Szymkiewicz-Simpson overlap coefficient: the number of q-grams the two
#' strings share, divided by the number in whichever has fewer. A short string
#' wholly contained in a long one scores `1`, where [jaccard_index()] would
#' penalise it for the extra q-grams -- which is what makes the overlap
#' coefficient the measure to reach for when one field is an abbreviation,
#' an initial or a truncation of the other.
#'
#' Identical strings score `1`, and the score is `0` when either string is
#' shorter than `q`.
#'
#' @inheritParams qgram_metrics
#' @return Numeric vector of similarities in `[0, 1]`, `length(a)` long;
#'   `NA` where either input is `NA`.
#' @seealso [jaccard_index()], [dice_coefficient()] and [tversky_index()] for
#'   the other set overlaps, and [cosine_similarity()] for the
#'   frequency-weighted one.
#' @family q-gram similarity functions
#' @examples
#' overlap_coefficient("BROWN", "BROWNLEE")
#' jaccard_index("BROWN", "BROWNLEE")   # the extra q-grams count against it
#' overlap_coefficient("night", "nacht")
#' @export
overlap_coefficient <- function(a, b, q = 2, nthreads = NULL,
                                use_bytes = TRUE) {
    .qgram_validate(a, b, q, use_bytes)
    fast_overlap_coefficient_impl(a, b, as.integer(q), .as_nthreads(nthreads),
                                  use_bytes)
}

#' Q-gram overlap coefficient all-pairs similarity matrix
#'
#' @inheritParams jaccard_matrix
#' @return Numeric matrix with `length(a)` rows and `length(b)` columns,
#'   with `names(a)` and `names(b)` as row and column names when present.
#' @family q-gram similarity functions
#' @examples
#' overlap_matrix(c("BROWN", "SMITH"), c("BROWNLEE", "SMITHERS", "JONES"))
#' @export
overlap_matrix <- function(a, b, q = 2, nthreads = NULL, use_bytes = TRUE) {
    .qgram_validate_matrix(a, b, q, use_bytes)
    .matrix_dimnames(fast_overlap_coefficient_matrix_impl(
        a, b, as.integer(q), .as_nthreads(nthreads), use_bytes
    ), a, b)
}
