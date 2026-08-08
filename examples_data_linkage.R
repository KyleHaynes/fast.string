# Examples: Data Linkage Functions in fast.string
# ============================================
# Use options(fast.string.verbose = FALSE) before library() to suppress the
# package startup banner.

options(fast.string.verbose = FALSE)
library(fast.string)

# ============================================================================
# 1. FORMAT_DATE — Fast date formatting for blocking keys
# ============================================================================

# Create sample dates
dates <- as.Date(c("1970-01-01", "2024-06-18", "1999-12-31", NA))

# Four supported formats
format_date(dates, "iso")        # YYYY-MM-DD (standard)
format_date(dates, "compact")    # YYYYMMDD (common blocking key)
format_date(dates, "dmy")        # DD/MM/YYYY (AU/EU display)
format_date(dates, "ymd_slash")  # YYYY/MM/DD

# Use case: Create date-based blocking keys from mixed-format input
record_dates <- as.Date(c("2020-03-15", "2020-03-15", "2021-07-22", "2021-07-22"))
blocking_keys <- format_date(record_dates, "compact")
# blocking_keys = c("20200315", "20200315", "20210722", "20210722")

# ============================================================================
# 2. DATE_PARTS — Extract components for feature engineering
# ============================================================================

parts <- date_parts(dates)
parts$year
parts$month
parts$day

# Use case: Extract birth month for fuzzy matching
dobs <- as.Date(c("1985-03-12", "1985-03-14", "1990-08-22"))
dob_parts <- date_parts(dobs)
# Can now use dob_parts$month in agreement scoring for month-level matching

# ============================================================================
# 3. JARO_WINKLER — Vectorised pairwise string similarity
# ============================================================================

# Simple pairwise comparison (equal-length vectors)
names_a <- c("JOHN", "MARY", "SMITH")
names_b <- c("JON",  "MARIE", "SMYTH")
scores <- jaro_winkler(names_a, names_b)
# scores ~ [0.933, 0.944, 0.889] — good matches

# Standard prefix weight (p=0.1 is the standard in data linkage)
jaro_winkler("SMITH", "SMYTH", p = 0.1)  # Higher due to common prefix

# Comparison: no prefix weight (pure Jaro)
jaro_winkler("SMITH", "SMYTH", p = 0.0)

# NA handling
jaro_winkler(c("JOHN", NA, "MARY"), c("JON", "MARIE", "MARIE"))

# ============================================================================
# 4. JARO_WINKLER_MATRIX — All-pairs blocking comparison
# ============================================================================

# Typical data-linkage scenario: compare records in dataset A against candidates in B
candidates_a <- c("JOHN SMITH", "MARY JONES", "ROBERT BROWN")
candidates_b <- c("JON SMYTH", "MARIE JONES", "ROB BROWN", "JOHN SMITH")

# Compute all 3×4 pairwise scores
similarity_matrix <- jaro_winkler_matrix(candidates_a, candidates_b)
# similarity_matrix[1,1] is jaro_winkler("JOHN SMITH", "JON SMYTH")
# similarity_matrix[1,4] is jaro_winkler("JOHN SMITH", "JOHN SMITH") = 1.0

# Apply threshold to find likely matches
matches <- similarity_matrix > 0.85
# matches[i,j] = TRUE if candidates_a[i] likely matches candidates_b[j]

# Use case: Find best match for each record in A
best_matches <- apply(similarity_matrix, 1, which.max)
best_scores  <- apply(similarity_matrix, 1, max)
# best_matches[1] = 4 (candidate A[1] best matches candidate B[4])

# ============================================================================
# 5. FTRIMWS — Clean whitespace from names/addresses
# ============================================================================

messy_names <- c("  JOHN SMITH  ", "\tMARY JONES\n", "ROBERT  BROWN", NA)

ftrimws(messy_names)             # Remove both sides (default)
ftrimws(messy_names, "left")     # Remove leading only
ftrimws(messy_names, "right")    # Remove trailing only

# Use case: Normalize addresses before comparison
addresses <- c("  123 Main St  ", "\t456 Oak Ave\n", "789 Pine Rd")
clean_addresses <- ftrimws(addresses)

# ============================================================================
# 6. FSUBSTR — Extract fixed-width fields from government records
# ============================================================================

# Common scenario: fixed-width government data (e.g., tax records, benefits)
# Field layout: Positions 1-3=State, 4-6=Category, 7-14=RecordID
records <- c("NSWRECORD123456789", "VICPOL987654321AB", "QLDHEALTH00112233")

state    <- fsubstr(records, 1, 3)   # NSW, VIC, QLD
category <- fsubstr(records, 4, 6)   # REC, POL, HEA
record_id <- fsubstr(records, 7, 14) # RECORD12, 987654, HEALTH00

# Vectorised start/stop (same length as x, or scalar)
starts <- c(1, 4, 7)
stops  <- c(3, 6, 14)
fsubstr(records[1], starts, stops)  # Recycle to extract 3 fields at once

# ============================================================================
# 7. FNCHAR — String length for validation/feature engineering
# ============================================================================

names_to_check <- c("JOHN", "MARY-JANE", "", NA)

fnchar(names_to_check)              # Bytes (same as "bytes" type)
fnchar(names_to_check, "bytes")     # Explicit: byte count
fnchar(names_to_check, "chars")     # UTF-8 codepoints (for accented names)

# Use case: Name length as agreement feature
postcode_field <- c("2000", "3141", "4000", NA)
length_score <- (fnchar(postcode_field) == 4)  # All valid Australian postcodes are 4 digits

# ============================================================================
# 8. FCHARTR — Normalize characters for blocking/matching
# ============================================================================

# Strip punctuation: useful for fuzzy name matching. fchartr() maps
# characters one-for-one, so `old` and `new` must be the same length -- it
# can replace a character but not delete one. Use fgsub() to remove.
names_with_punct <- c("O'BRIEN", "SMITH-JONES", "MARY-ANN", NA)
without_punct <- fgsub("['-]", "", names_with_punct)
# Result: c("OBRIEN", "SMITHJONES", "MARYANN", NA)

# Replacing punctuation with spaces (rather than deleting it) keeps token
# boundaries intact for jaro_winkler_tokens() -- that IS a job for fchartr().
fchartr("'-", "  ", names_with_punct)

# Map accented characters (e.g., for international records)
accented_names <- c("CAFÉ", "NAÏVE", "JOSÉ", "FRANÇOIS")
ascii_names <- fchartr("ÀÄÉÏÖÜàäéïöüÑñ", "AAEIOUaaeiouNn", accented_names)
# Maps accented → ASCII for comparison

# Use case: Normalize both sides before jaro_winkler
record_name <- "O'BRIEN"
candidate_name <- "OBRIEN"
normalized_record <- fgsub("['\\- ]", "", record_name)
normalized_candidate <- fgsub("['\\- ]", "", candidate_name)
jaro_winkler(normalized_record, normalized_candidate)

# ============================================================================
# 9. FCOUNT — Column-level data-quality profiling before matching
# ============================================================================

# Counting matches per element is the cheapest way to find records that
# should never reach the matching step at all.
raw_names <- c("JOHN O'BRIEN", "UNKNOWN 00000000", "Kyle John Haynes", NA)

fcount("[0-9]", raw_names)        # digits in a name field -> placeholder rows
fcount(" +", ftrimws(raw_names)) + 1L   # token count
fcount("'", raw_names, fixed = TRUE)    # apostrophes needing normalisation

# Use case: quarantine unmatched-able records up front
is_placeholder <- fcount("[0-9]", raw_names) > 0
# raw_names[!is_placeholder & !is.na(raw_names)] goes on to matching

# Matches are counted non-overlapping:
fcount("aa", "aaaa", fixed = TRUE)   # 2, not 3

# ============================================================================
# 10. FAS.POSIXCT / FORMAT_DATETIME — Event timestamps without a tz database
# ============================================================================

received <- c("2024-06-18 09:15:00", "2024-06-18 14:20:00", "not a time")
loaded <- fas.POSIXct(received)          # UTC POSIXct; malformed -> NA

# Four fixed shapes, on input and output
fas.POSIXct("2024-06-18T09:15:00Z", "rfc3339")
fas.POSIXct("20240618091500", "compact")
fas.POSIXct("2024-06-18T09:15:00+10:00", "iso_offset")  # normalised to UTC

# Unlike fas.Date(), the full calendar is validated:
fas.Date("29/02/2023", "dmy")         # rolls over -- 2023 is not a leap year
fas.POSIXct("2023-02-29 08:00:00")    # NA

# Use case: keep the most recent copy of a record received twice in a batch
# batch <- batch[order(batch$key, -as.numeric(batch$loaded)), ]
# batch <- batch[!duplicated(batch$key), ]

# Use case: stamp the output batch. "iso_offset" is a fixed offset, not a
# timezone -- no daylight-saving rules.
format_datetime(max(loaded, na.rm = TRUE), "rfc3339")
format_datetime(max(loaded, na.rm = TRUE), "iso_offset", offset = "+10:00")
format_datetime(max(loaded, na.rm = TRUE), "compact")   # sortable partition key

# ============================================================================
# 11. REFINED_SOUNDEX / COLOGNE — Blocking keys at two precision settings
# ============================================================================

surnames <- c("Müller", "Mueller", "Miller", "Smith", "Smyth", "Schmidt")

soundex(surnames)          # 4 chars, coarse
refined_soundex(surnames)  # encodes vowels, no truncation -> smaller blocks
cologne(surnames)          # German rules; folds umlauts to base vowels

# refined_soundex() splits "Müller"/"Mueller" (the umlaut is not ASCII);
# cologne() folds it and groups them. Neither key is right on its own.
data.frame(
    name    = surnames,
    soundex = soundex(surnames),
    refined = refined_soundex(surnames),
    cologne = cologne(surnames)
)

# Use case: multi-key blocking. Encode every token in one vectorised call,
# then take the union of the candidate sets each scheme produces.
tokens <- strsplit(c("Hans Müller", "Hans Mueller"), " +")
data.frame(
    record = rep(1:2, lengths(tokens)),
    token  = unlist(tokens),
    key    = cologne(unlist(tokens))
)

# ============================================================================
# 12. COSINE_SIMILARITY — A second, independent scoring signal
# ============================================================================

# jaccard/dice/tversky compare q-gram SETS (a repeated q-gram counts once).
# cosine compares q-gram frequency PROFILES, so repetition carries weight.
jaccard_index("aaaa", "aaab")       # 0.5  -- one of two distinct bigrams
cosine_similarity("aaaa", "aaab")   # 0.894 -- three "aa" against two

# Repetition is the norm in the identifier/code fields that sit next to
# names in a linkage file.
cosine_similarity(c("AAA-111", "0000"), c("AAA-112", "0001"))

# Use case: score candidates on two features that disagree informatively.
# Token comparison is order-insensitive; cosine is not.
jaro_winkler_tokens("Kyle John Haynes", "Haynes John Kyle")  # 1
cosine_similarity("KYLE JOHN HAYNES", "HAYNES JOHN KYLE")    # 0.867
# A high token score with a lower cosine says "same tokens, different order"
# -- different evidence from "same order, one typo".

# ============================================================================
# PRACTICAL PIPELINE: End-to-end record linkage
# ============================================================================

# Sample dataset: incoming records to match
incoming <- data.frame(
    id = c(1, 2, 3),
    name = c("  JOHN O'BRIEN  ", "MARY-JANE SMITH", "ROBERT BROWN"),
    dob = as.Date(c("1985-03-15", "1990-07-22", "1975-12-01")),
    stringsAsFactors = FALSE
)

# Reference database (candidates to match against)
reference <- data.frame(
    ref_id = c(101, 102, 103, 104),
    name = c("JON OBRIEN", "MARYANN SMITH", "ROB BROWN", "JOHN BRIEN"),
    dob = as.Date(c("1985-03-14", "1990-07-20", "1975-12-02", "1985-03-15")),
    stringsAsFactors = FALSE
)

# Step 1: Normalize incoming names
incoming$name_clean <- fgsub("['\\- ]", "", ftrimws(incoming$name))

# Step 2: Normalize reference names
reference$name_clean <- fgsub("['\\- ]", "", ftrimws(reference$name))

# Step 3: Create date-based blocking key (month-year)
incoming$dob_block <- fsubstr(format_date(incoming$dob, "compact"), 1, 6)  # YYYYMM
reference$dob_block <- fsubstr(format_date(reference$dob, "compact"), 1, 6)

# Step 4: Within same month-year block, compute all name similarities
block_1 <- which(incoming$dob_block == "198503")  # JOHN (block 198503)
candidates <- which(reference$dob_block == "198503")
if (length(block_1) > 0 && length(candidates) > 0) {
    jw_scores <- jaro_winkler_matrix(
        incoming$name_clean[block_1],
        reference$name_clean[candidates]
    )
    # jw_scores[1,] gives similarities for JOHN against candidates
    best_match <- which.max(jw_scores[1,])
    best_candidate <- candidates[best_match]
}

# Step 5: Apply threshold (e.g., 0.85) to determine links
threshold <- 0.85
# can_link <- jw_scores > threshold

# ============================================================================
# PRACTICAL PIPELINE, EXTENDED: profile -> block -> score on two features
# ============================================================================

# The pipeline above blocks on a date key alone and scores on one metric.
# A fuller version profiles the input first, blocks on phonetic keys from
# more than one scheme, and carries two independent similarity features.

# Step A: profile and quarantine
incoming$n_digits <- fcount("[0-9]", incoming$name)
usable <- incoming$n_digits == 0

# Step B: block on the phonetic code of EVERY token (names arrive in
# inconsistent token order), within the same birth year, across two schemes
block_keys <- function(ids, names_vec, years, scheme) {
    toks  <- strsplit(names_vec, " +")
    codes <- scheme(unlist(toks))
    out <- data.frame(
        id  = rep(ids, lengths(toks)),
        key = paste0(codes, ":", rep(years, lengths(toks))),
        stringsAsFactors = FALSE
    )
    out[!is.na(codes) & nzchar(codes), ]
}

inc_names <- fgsub(" +", " ", fchartr("'-", "  ", ftrimws(incoming$name)))
ref_names <- fchartr("'-", "  ", ftrimws(reference$name))

inc_keys <- rbind(
    block_keys(incoming$id[usable], inc_names[usable],
               date_parts(incoming$dob[usable])$year, refined_soundex),
    block_keys(incoming$id[usable], inc_names[usable],
               date_parts(incoming$dob[usable])$year, cologne)
)
ref_keys <- rbind(
    block_keys(reference$ref_id, ref_names, date_parts(reference$dob)$year, refined_soundex),
    block_keys(reference$ref_id, ref_names, date_parts(reference$dob)$year, cologne)
)
names(ref_keys)[1] <- "ref_id"

pairs <- unique(merge(inc_keys, ref_keys, by = "key")[, c("id", "ref_id")])
nrow(pairs)                            # candidate pairs...
sum(usable) * nrow(reference)          # ...instead of the full cross product

# Step C: score survivors on two independent features
pairs$name_a <- inc_names[match(pairs$id, incoming$id)]
pairs$name_b <- ref_names[match(pairs$ref_id, reference$ref_id)]
pairs$jw  <- jaro_winkler_tokens(pairs$name_a, pairs$name_b, ignore_case = TRUE)
pairs$cos <- cosine_similarity(toupper(pairs$name_a), toupper(pairs$name_b))
pairs$dob_gap <- abs(as.integer(
    incoming$dob[match(pairs$id, incoming$id)] -
    reference$dob[match(pairs$ref_id, reference$ref_id)]
))

# Step D: accept on the threshold, but flag records whose top two candidates
# are too close to separate -- those belong in a review queue, not a link.
accepted <- pairs[pairs$jw >= threshold & pairs$dob_gap <= 2, ]
accepted <- accepted[order(accepted$id, -accepted$jw), ]
accepted$ambiguous <- table(accepted$id)[as.character(accepted$id)] > 1L
accepted[, c("id", "name_a", "ref_id", "name_b", "jw", "cos", "ambiguous")]

# See the worked chapter for the full narrative version of this pipeline:
# https://kylehaynes.github.io/fast.string/04-record-linkage-workflow.html
