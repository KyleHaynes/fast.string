pkgname <- "fast.string"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('fast.string')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("caverphone")
### * caverphone

flush(stderr()); flush(stdout())

### Name: caverphone
### Title: Caverphone 2.0 phonetic code (Caversham Project, University of
###   Otago)
### Aliases: caverphone

### ** Examples

caverphone(c("Peter", "Tedder", "Stevenson"))



cleanEx()
nameEx("double_metaphone")
### * double_metaphone

flush(stderr()); flush(stdout())

### Name: double_metaphone
### Title: Double Metaphone phonetic code (Lawrence Philips, 2000)
### Aliases: double_metaphone

### ** Examples

double_metaphone(c("Smith", "Schmidt", "Catherine", "Kathryn"))



cleanEx()
nameEx("fuzz")
### * fuzz

flush(stderr()); flush(stdout())

### Name: fuzz
### Title: fuzzywuzzy-style fuzzy string ratios
### Aliases: fuzz fuzz_ratio fuzz_partial_ratio fuzz_token_sort_ratio
###   fuzz_token_set_ratio

### ** Examples

fuzz_ratio("this is a test", "this is a test!")
fuzz_partial_ratio("fuzzy wuzzy was a bear", "wuzzy fuzzy was a bear")
fuzz_token_sort_ratio("fuzzy was a bear", "bear was a fuzzy")
fuzz_token_set_ratio("fuzzy was a bear", "fuzzy fuzzy bear was a bear")



cleanEx()
nameEx("jaro_winkler_tokens")
### * jaro_winkler_tokens

flush(stderr()); flush(stdout())

### Name: jaro_winkler_tokens
### Title: Token-aware Jaro-Winkler similarity for multi-token strings
### Aliases: jaro_winkler_tokens

### ** Examples

jaro_winkler_tokens("Kyle John Haynes", "John Kylie Haynes")
jaro_winkler_tokens(c("OBrien", "O'Brien"), c("O Brien", "O Brien"))

# Penalised, not ignored: an extra token still costs something.
jaro_winkler_tokens("John Smith", "John Smith Jones")

# Extra/junk token ("ZZ") dropped instead of diluting the score.
jaro_winkler_tokens("Kylie John ZZ Haynes", "Haynes John Kyle",
                    extra_penalty = 0)

# Contractions: "KYLEJOHN" recognised as "KYLE" + "JOHN" (adjacent), and
# "KYLEHAYNES" recognised as "KYLE" + "HAYNES" (non-adjacent, "JOHN" sits
# between them in the original) -- both score perfectly with contractions on.
jaro_winkler_tokens("KYLEJOHN HAYNES", "KYLE JOHN HAYNES", contractions = TRUE)
jaro_winkler_tokens("KYLEHAYNES JOHN", "KYLE JOHN HAYNES", contractions = TRUE)

# Both sides already fused two words into one token, just in opposite
# order ("JOHNKYLE" vs "KYLEJOHN") -- rescued by the rotation check.
jaro_winkler_tokens("HAYNES JOHNKYLE", "KYLEJOHN HAYNES", contractions = TRUE)



cleanEx()
nameEx("levenshtein")
### * levenshtein

flush(stderr()); flush(stdout())

### Name: levenshtein
### Title: Vectorised Levenshtein, Damerau-Levenshtein, and Hamming
###   distance
### Aliases: levenshtein damerau_levenshtein hamming

### ** Examples

levenshtein("kitten", "sitting")
damerau_levenshtein("ab", "ba")
hamming("karolin", "kathrin")



cleanEx()
nameEx("qgram_metrics")
### * qgram_metrics

flush(stderr()); flush(stdout())

### Name: qgram_metrics
### Title: Vectorised q-gram set-overlap similarity: Jaccard, Dice, Tversky
### Aliases: qgram_metrics jaccard_index dice_coefficient tversky_index

### ** Examples

jaccard_index("night", "nacht")
dice_coefficient("night", "nacht")
tversky_index("night", "nacht", alpha = 1, beta = 1) # == jaccard_index()
jaccard_index(c("Kyle Haynes", "John Smith"), c("Kyle Haynes", "Jon Smith"))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
