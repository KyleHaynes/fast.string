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

# Strip punctuation: useful for fuzzy name matching
names_with_punct <- c("O'BRIEN", "SMITH-JONES", "MARY-ANN", NA)
without_punct <- fchartr("'-", "", names_with_punct)
# Result: c("OBRIEN", "SMITHJONES", "MARYANN", NA)

# Map accented characters (e.g., for international records)
accented_names <- c("CAFÉ", "NAÏVE", "JOSÉ", "FRANÇOIS")
ascii_names <- fchartr("ÀÄÉÏÖÜàäéïöüÑñ", "AAEIOUaaeiouNn", accented_names)
# Maps accented → ASCII for comparison

# Use case: Normalize both sides before jaro_winkler
record_name <- "O'BRIEN"
candidate_name <- "OBRIEN"
normalized_record <- fchartr("'- ", "", record_name)
normalized_candidate <- fchartr("'- ", "", candidate_name)
jaro_winkler(normalized_record, normalized_candidate)

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
incoming$name_clean <- fchartr("'- ", "", ftrimws(incoming$name))

# Step 2: Normalize reference names
reference$name_clean <- fchartr("'- ", "", ftrimws(reference$name))

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
