# A basic rLink() example: two files of 100,000 people, four blocking passes
# each built with a different kind of hashed key, four comparison fields, and
# about 9 million candidate pairs.
#
#   Rscript rlink_example.R
#
# The data are synthetic but shaped like real administrative files: surnames
# and given names follow a Zipf distribution (a few very common, most rare),
# and 70% of the second file are people from the first, some with a typing
# error, a nickname, a slipped date of birth or a new postcode.

library(fast.string)

set.seed(2026)
n <- 100000L

# --- synthetic data ---------------------------------------------------------

syllables <- c("an", "ber", "cha", "dor", "el", "fin", "gar", "ham", "is",
               "jo", "kel", "lin", "mac", "nor", "ow", "per", "qui", "ros",
               "son", "tho", "ul", "van", "wil", "xu", "yor", "zam", "smi",
               "th", "ng", "uy", "en", "bro", "wn", "es", "ley", "ton")

# `size` distinct made-up names of two to four syllables.
make_names <- function(size, parts) {
    out <- character()
    while (length(out) < size) {
        k <- sample(parts, size, replace = TRUE)
        built <- vapply(k, function(m)
            paste(sample(syllables, m, replace = TRUE), collapse = ""), "")
        out <- unique(c(out, toupper(built)))
    }
    out[seq_len(size)]
}

# Draw from a pool with Zipf weights: the first name is the commonest.
zipf <- function(pool, size)
    pool[sample.int(length(pool), size, replace = TRUE,
                   prob = 1 / seq_along(pool))]

surnames <- make_names(8000L, 2:4)
givens <- make_names(800L, 1:3)

enrolments <- data.frame(
    frame_id = sprintf("E%06d", seq_len(n)),
    surname  = zipf(surnames, n),
    given    = zipf(givens, n),
    sex      = sample(c("F", "M"), n, replace = TRUE),
    dob      = as.Date("1940-01-01") + sample.int(25000L, n, replace = TRUE),
    postcode = sprintf("%04d", 800L + sample.int(400L, n, replace = TRUE)),
    stringsAsFactors = FALSE
)

claims <- data.frame(
    frame_id = sprintf("C%06d", seq_len(n)),
    surname  = zipf(surnames, n),
    given    = zipf(givens, n),
    sex      = sample(c("F", "M"), n, replace = TRUE),
    dob      = as.Date("1940-01-01") + sample.int(25000L, n, replace = TRUE),
    postcode = sprintf("%04d", 800L + sample.int(400L, n, replace = TRUE)),
    stringsAsFactors = FALSE
)

# 70% of the claims are enrolled people, recorded with the usual errors.
same <- sample.int(n, 0.7 * n)
from <- sample.int(n, length(same))
for (col in c("surname", "given", "sex", "dob", "postcode"))
    claims[[col]][same] <- enrolments[[col]][from]

typo <- sample(same, 0.10 * length(same))          # swap two letters
s <- claims$surname[typo]
claims$surname[typo] <- paste0(substr(s, 2, 2), substr(s, 1, 1),
                               substr(s, 3, nchar(s)))
nick <- sample(same, 0.08 * length(same))          # another given name
claims$given[nick] <- zipf(givens, length(nick))
slip <- sample(same, 0.05 * length(same))          # date of birth a day out
claims$dob[slip] <- claims$dob[slip] + 1L
moved <- sample(same, 0.15 * length(same))         # moved house
claims$postcode[moved] <- sprintf("%04d", 800L + sample.int(400L, length(moved),
                                                            replace = TRUE))

# --- link -------------------------------------------------------------------
#
# Four passes; a pair is a candidate if it agrees on any one of them. Each
# pass uses a different kind of hashed key, so a record that escapes one
# (a typo in the surname, a slipped date) is still caught by another:
#
#   1. Soundex of surname          + year of birth     (phonetic code)
#   2. Double Metaphone of given   + month of birth    (phonetic code)
#   3. NYSIIS of surname           + postcode area     (phonetic code)
#   4. xxh64 hash of sex + day and month of birth, cut to 3 hex digits
#      (4,096 buckets) + start of the Cologne code of given   (hash bucket)
#
# The fourth is the one that survives a typo in the surname *and* a wrong
# year. A truncated hash is a cheap way to make a key with a chosen number of
# buckets out of anything.

pairs <- rLink(
    enrolments, claims,
    block_1 = list(
        soundex_yob = list(soundex(surname), format_date(dob, "y")),
        metaphone   = list(double_metaphone(given)$primary,
                           format_date(dob, "y-m")),
        nysiis_area = list(nysiis(surname), fsubstr(postcode, 1, 3)),
        hash_bucket = list(fsubstr(fhash(paste(sex, format_date(dob, "m-d")),
                                         algo = "xxh64"), 1, 3),
                           fsubstr(cologne(given), 1, 2))
    ),
    compare = list(
        sn  = jaro_winkler(surname),
        gn  = jaro_winkler(given),
        dob = date_compare(dob),
        pc  = levenshtein(postcode)
    ),
    keep    = c("frame_id", "surname", "given", "dob", "postcode"),
    verbose = TRUE
)

# --- look at the result -----------------------------------------------------

summary(pairs)
attr(pairs, "timings")

# Which pass found the pairs
table(pass = pairs$pass)

# The best-scoring pairs
head(pairs[order(-sn, -gn, -dob, -pc)], 10)

# A simple rule on the four scores
linked <- pairs[sn >= 0.9 & gn >= 0.85 & dob >= 0.5 & pc >= 0.5]
cat(sprintf("%s pairs pass the rule.\n", format(nrow(linked), big.mark = ",")))
