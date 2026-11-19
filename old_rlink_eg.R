# The legacy rLink() call, converted to fast.string::rLink()
# ===========================================================================
#
#   1. A straight conversion: the same 14 blocking passes and the same 9
#      Jaro-Winkler comparisons, pass for pass and score for score.
#   2. An improved version: the same linkage with the fields prepared once up
#      front, phonetic keys that beat Soundex, a sorted-neighbourhood pass,
#      more informative comparators, and a `filter` so the costly ones only
#      ever see pairs still in the running.
#
# How the old arguments map onto the new ones:
#
#   data1, data2           data_1, data_2
#   blockStrategy1/2       block_1 / block_2. Terms are now *expressions*
#                          evaluated against the data, not column names, so a
#                          key is written where it is used.
#   soundexPos             soundex() around the term itself.
#   missingPos             match_missing, one flag for the whole call. A
#                          single term asks for that behaviour by giving its
#                          missing values a value of their own -- na_key()
#                          below.
#   winkler1 / winkler2    compare = list(...), with `left | right` where the
#                          two files name the same information differently.
#   winklerSplitVec1/2     jaro_winkler_tokens(), which scores token by token.
#                          It splits on whitespace, so the old "-| " becomes a
#                          hyphen-to-space fgsub() around the column.
#   pair_id = c("frame_id", "frame_id")      pair_id = "frame_id"
#
# `d` is the first file and `q` the second, as before. The original built its
# contact passes from `email_combinations` and `phone_combinations`: one email
# column each side (1 pass) and three phone columns each side crossed with
# each other (9 passes). The names used here stand in for whatever those
# held -- change them to the real ones:
#
#   d: email         phone_home    phone_work   phone_mobile
#   q: email_address telephone     mobile       other_phone
#
# and the address fields are named as the old call had them: flat_number /
# number_first / postcode / locality in `d`, unit_no / street_number /
# postcode / postal_postcode / locality / postal_locality in `q`.

library(fast.string)
library(data.table)

# `missingPos` said, term by term, whether two records that are both missing a
# term could still pair on it. `rLink()` has one `match_missing` flag for the
# whole call, so where a single term needs that, give its missing values a
# value of their own. Keys are built from the text, so a value no real record
# holds is as good as a flag.
na_key <- function(x) {
    x <- as.character(x)
    x[is.na(x) | x == ""] <- "(MISSING)"
    x
}


# ===========================================================================
# 1. A straight conversion
# ===========================================================================
#
# Passes, in the order the old call listed them: 1 email, 9 phone, then the
# four name/address passes. `soundexPos` has become soundex() around the term
# it named, and `missingPos` na_key() around the terms it left out --- for the
# name/dob pass that was every term, which is worth a second look: records
# missing all three then block together.

straight <- rLink(
    data_1 = d,
    data_2 = q,

    block_1 = list(
        email       = email,
        # Every phone column in `d` against every phone column in `q`.
        ph_home_tel = phone_home,
        ph_home_mob = phone_home,
        ph_home_oth = phone_home,
        ph_work_tel = phone_work,
        ph_work_mob = phone_work,
        ph_work_oth = phone_work,
        ph_mob_tel  = phone_mobile,
        ph_mob_mob  = phone_mobile,
        ph_mob_oth  = phone_mobile,
        # soundexPos = c(1, 2), missingPos = c()
        name_dob    = list(na_key(soundex(first_name)),
                           na_key(soundex(last_name)),
                           na_key(dob)),
        # soundexPos = c(1), missingPos = c(1, 4)
        addr_pc     = list(soundex(last_name), na_key(flat_number),
                           na_key(number_first), postcode),
        # soundexPos = c(1, 4), missingPos = c(1, 4)
        addr_loc    = list(soundex(last_name), na_key(flat_number),
                           na_key(number_first), soundex(locality)),
        # soundexPos = c(1), missingPos = c(1, 2, 3)
        po_box      = list(soundex(last_name), post_box_no, postcode)
    ),

    # One pass for every pass of `block_1`, with the same number of terms.
    # NULL reuses `block_1`'s expressions, for a pass whose columns are named
    # the same in both files.
    block_2 = list(
        email_address,
        telephone, mobile, other_phone,
        telephone, mobile, other_phone,
        telephone, mobile, other_phone,
        NULL,
        list(soundex(last_name), na_key(unit_no),
             na_key(street_number), postcode),
        list(soundex(last_name), na_key(unit_no),
             na_key(street_number), soundex(locality)),
        list(soundex(last_name), post_box_no, postal_postcode)
    ),

    # The nine `winkler1`/`winkler2` pairs. fgsub() puts the old split
    # characters back where jaro_winkler_tokens() looks for them: it strips
    # hyphens by default, which joins "SMITH-JONES" into one token instead of
    # two. Where the old call had NA for the split, the plain comparator.
    compare = list(
        first_name  = jaro_winkler_tokens(
            fgsub("-", " ", first_name, fixed = TRUE)),
        last_name   = jaro_winkler_tokens(
            fgsub("-", " ", last_name, fixed = TRUE)),
        given_names = jaro_winkler_tokens(
            fgsub("-", " ", given_names, fixed = TRUE)),
        dob         = jaro_winkler(dob),
        street_name = jaro_winkler_tokens(
            fgsub("-", " ", street_name, fixed = TRUE)),
        locality    = jaro_winkler_tokens(
            fgsub("-", " ", locality, fixed = TRUE)),
        locality_postal = jaro_winkler_tokens(
            fgsub("-", " ", locality, fixed = TRUE) |
            fgsub("-", " ", postal_locality, fixed = TRUE)),
        postcode    = jaro_winkler_tokens(
            fgsub("-", " ", postcode, fixed = TRUE)),
        postcode_postal = jaro_winkler(postcode | postal_postcode)
    ),

    pair_id = "frame_id"
)

# What each pass cost and found, including the keys that make the most pairs.
# Add `dry_run = TRUE` to the call above to get this report without
# generating a single pair --- worth doing before the first full run.
attr(straight, "blocking")
summary(straight)


# ===========================================================================
# 2. An improved version
# ===========================================================================
#
# Everything the two files disagree about in form rather than content is
# settled once, up front, in data.table: the renaming, the case and accent
# folding, the separators, the phone and postcode formats, and the address
# numbers as numbers. The linkage call is then about the linkage.
#
# `rLink()` never modifies its inputs, and neither does this: it works on
# copies and leaves `d` and `q` as they are.

d2 <- copy(as.data.table(d))
q2 <- copy(as.data.table(q))

# One name for one thing. `q`'s postal address keeps its own names, since it
# is a second address rather than another spelling of the first.
setnames(q2,
         old = c("email_address", "telephone", "mobile", "other_phone",
                 "unit_no", "street_number"),
         new = c("email", "phone_home", "phone_work", "phone_mobile",
                 "flat_number", "number_first"),
         skip_absent = TRUE)

prep <- function(dt) {
    # Names and places: fold accents, upper-case, turn the characters the old
    # `winklerSplitVec` split on into spaces, and squash the runs of blanks
    # that leaves. Once per record here, rather than once per pair inside
    # every comparator.
    text <- c("first_name", "last_name", "given_names", "street_name",
              "locality")
    dt[, (paste0(text, "_c")) := lapply(.SD, function(v)
        fsquish(fgsub("[-/,.]", " ", fupper(ascii_fold(v))))),
       .SDcols = text]

    # Four digits, so "2 600" and "02600" meet.
    dt[, postcode_c := fpad(fgsub("[^0-9]", "", as.character(postcode)), 4,
                            pad = "0")]

    # Case and surrounding blanks are not information.
    dt[, email_c := flower(ftrimws(as.character(email)))]

    # Phone numbers: digits only, then the last eight, so a leading 0, a +61,
    # brackets and spaces stop mattering. Fewer than eight digits is not a
    # number worth blocking on.
    for (p in c("phone_home", "phone_work", "phone_mobile")) {
        v <- fgsub("[^0-9]", "", as.character(dt[[p]]))
        n <- fnchar(v)
        dt[, (paste0(p, "_c")) :=
               fifelse(n >= 8L, fsubstr(v, n - 7L, n), NA_character_)]
    }

    # Unit and street numbers as numbers, for numeric_compare(): "12A" and
    # "12" are then one apart rather than two different strings.
    dt[, `:=`(flat_no_n   = fparse_number(as.character(flat_number)),
              street_no_n = fparse_number(as.character(number_first)))]
    invisible(dt)
}
prep(d2)
prep(q2)

# `q`'s postal locality and postcode, cleaned the same way so they can be
# compared against `d`'s residential ones.
q2[, `:=`(
    locality_postal_c = fsquish(fgsub("[-/,.]", " ",
                                      fupper(ascii_fold(postal_locality)))),
    postcode_postal_c = fpad(fgsub("[^0-9]", "",
                                    as.character(postal_postcode)), 4,
                             pad = "0")
)]

# --- the passes ------------------------------------------------------------
#
# The ten contact passes are the cross product the original built from
# `phone_combinations`, so build them the same way rather than writing them
# out. alist() holds the terms unevaluated; `!!` splices the finished
# specification into the call.

phones <- c(h = "phone_home_c", w = "phone_work_c", m = "phone_mobile_c")
grid   <- CJ(a = names(phones), b = names(phones), sorted = FALSE)
labels <- paste0("ph_", grid$a, grid$b)

contact_1 <- c(alist(email = email_c),
               setNames(lapply(phones[grid$a], as.name), labels))
contact_2 <- c(alist(email = email_c),
               setNames(lapply(phones[grid$b], as.name), labels))

# Double Metaphone and NYSIIS both keep more of a name than Soundex's one
# letter and three digits, so their blocks are smaller without losing the
# spelling variants blocking phonetically is for. The year-of-birth pass
# catches the records whose date of birth slipped; the last pass is a sorted
# neighbourhood, which pairs surnames that are merely *near* each other in
# sorted order, so a typo no phonetic code survives still gets a chance.
address_1 <- alist(
    name_dob  = list(double_metaphone(first_name_c)$primary,
                     double_metaphone(last_name_c)$primary, dob),
    name_yob  = list(double_metaphone(first_name_c)$primary,
                     double_metaphone(last_name_c)$primary,
                     format_date(dob, "y")),
    addr_pc   = list(nysiis(last_name_c), na_key(flat_no_n),
                     na_key(street_no_n), postcode_c),
    addr_loc  = list(nysiis(last_name_c), na_key(flat_no_n),
                     na_key(street_no_n),
                     double_metaphone(locality_c)$primary),
    po_box    = list(nysiis(last_name_c), post_box_no, postcode_c),
    sn_window = list(last_name_c, dob)
)

block_spec <- c(contact_1, address_1)
n_pass     <- length(block_spec)

improved <- rLink(
    data_1 = d2,
    data_2 = q2,

    block_1 = !!as.call(c(list(quote(list)), block_spec)),
    block_2 = !!as.call(c(list(quote(list)), contact_2,
                          setNames(vector("list", length(address_1)),
                                   names(address_1)))),

    # Only the last pass slides a window; the rest require their keys to be
    # equal, as every pass in the original did.
    block_method = c(rep("exact", n_pass - 1L), "sorted_neighbourhood"),
    window       = 5,

    # A key half the file shares is not a key. Blocks bigger than this are
    # skipped, and the blocking report says how many and in which pass.
    max_block_size = 5000,

    compare = list(
        # Token-aware, and `contractions = TRUE` also tries pairs of tokens
        # concatenated, so "DE SOUZA" matches "DESOUZA" and "ANN MARIE"
        # matches "ANNMARIE".
        sn     = jaro_winkler_tokens(last_name_c, contractions = TRUE),
        fn     = jaro_winkler_tokens(first_name_c, contractions = TRUE),
        gn     = jaro_winkler_tokens(given_names_c, contractions = TRUE),
        # A second opinion on the surname from 2-grams: it does not care
        # where the letters moved, so it forgives what an ordered comparator
        # cannot.
        sn_q   = jaccard_index(last_name_c, q = 2),
        # Given names in a different order, or all of them in one file and
        # only the first in the other.
        gn_set = fuzz_token_set_ratio(given_names_c),
        # date_compare() compares the dates, not their spelling, so a
        # transposed day and month and a one-digit year typo score as the
        # near-misses they are rather than as two different strings.
        dob    = date_compare(dob),
        # Addresses. The numbers compare as numbers: one apart scores 0.5
        # with `scale = 2`, further apart less. A missing number scores NA
        # and is simply left out of that pair's weight below.
        street = jaro_winkler_tokens(street_name_c),
        st_no  = numeric_compare(street_no_n, scale = 2),
        flat   = numeric_compare(flat_no_n, scale = 2),
        pc     = levenshtein(postcode_c),
        loc    = jaro_winkler_tokens(locality_c),
        # The postal address of `q` against the residential address of `d`.
        pc_post  = levenshtein(postcode_c | postcode_postal_c),
        loc_post = jaro_winkler_tokens(locality_c | locality_postal_c),
        # Normalised, so 1 is an exact match: an email that agrees is the
        # strongest evidence in the file.
        email  = levenshtein(email_c)
    ),

    # Blocking is deliberately generous and most candidates are not matches.
    # Each condition is applied as soon as the scores it reads exist, and the
    # comparators run in the order the rule asks for them --- so the cheap,
    # decisive ones come first and the costly ones only ever see what is
    # still in the running. Keep the cut-offs loose: a pair dropped here is
    # not in the result to review.
    filter = sn >= 0.75 & (fn >= 0.7 | gn >= 0.7),

    # Only what a review needs to read, under both suffixes.
    keep = c("frame_id", "first_name", "last_name", "given_names", "dob",
             "street_name", "locality", "postcode", "email"),

    pair_id = "frame_id",
    verbose = TRUE
)

# --- what the run did ------------------------------------------------------

summary(improved)
attr(improved, "blocking")   # per pass: missing keys, biggest block, pairs
attr(improved, "compare")    # what each score is, and how many pairs it ran on
attr(improved, "filter")     # candidates found, scored, kept
attr(improved, "timings")    # seconds per stage

# --- from scores to links --------------------------------------------------
#
# One weight per pair instead of fourteen scores to eyeball, fitted on the
# pairs themselves (Fellegi-Sunter via em_linkage()). `tf` makes agreeing on
# a rare surname worth more than agreeing on the common one.
scored <- rLink_weights(improved, tf = "sn")
attr(scored, "model")$fit

# What a cut-off would do, with no truth set to check it against.
threshold_sweep(scored, "probability", probability = "probability",
                thresholds = c(0.5, 0.9, 0.99))[
    , c("threshold", "n_pairs", "est_precision", "est_recall")]

# One link per record each side: the best-weighted pair is taken first and
# kept whenever neither of its records is already linked.
accepted <- scored[probability >= 0.9]
links    <- accepted[select_greedy(accepted, weight = accepted$weight)]

# The pairs near the cut-off are the ones worth a human: this opens them in a
# reviewer keyed on `pair_id`, so the decisions survive a re-run.
# rLink_inspect(scored, threshold = 0.9)

# And the records no accepted link reached, which are what a second, looser
# run should be given.
# rLink(d2[rLink_unmatched(links), ], q2[rLink_unmatched(links, side = 2), ],
#       block_1 = list(soundex(last_name_c)), ...)
