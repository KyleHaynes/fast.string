#' Fast vectorised hashing of strings
#'
#' Returns a hexadecimal digest of each string: SHA-256 by default, or
#' SHA-1, MD5, SHA-512 or the non-cryptographic XXH64. Given a `key`, the
#' MD5 and SHA digests become HMACs (RFC 2104), the standard way to
#' pseudonymise identifiers before sharing or linking data: without the key
#' nobody can rebuild the mapping by hashing every possible identifier, yet
#' the same identifier always gets the same pseudonym, so datasets hashed
#' with one key can still be joined. Strings are hashed in parallel across
#' CPU cores via Intel TBB, which makes this much faster than hashing one
#' string at a time with `digest::digest()` or serially with the `openssl`
#' package.
#'
#' Each string is hashed as UTF-8 (latin1 and native strings are translated
#' first), so the digest of a string does not depend on how it is encoded in
#' R; strings marked `"bytes"` are hashed as they are. Hashing is exact, so
#' normalise first (case, whitespace, punctuation: see [fsquish()],
#' [ascii_fold()] and [freplace_tokens()]) when spelling variants should get
#' the same pseudonym.
#'
#' XXH64 is not cryptographic: it is meant for fast fingerprints, change
#' detection and partitioning, and cannot take a key (use `seed` to vary
#' it). MD5 and SHA-1 are broken for collision resistance and are provided
#' for compatibility with existing hashed data.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param algo One of `"sha256"`, `"sha1"`, `"md5"`, `"sha512"` and
#'   `"xxh64"`.
#' @param key Optional secret key for an HMAC: a character string (used as
#'   its UTF-8 bytes) or a raw vector. Not allowed with `"xxh64"`.
#' @param seed Non-negative integer seed for `"xxh64"` (default `0`).
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x` of lower-case hex
#'   digests (64 characters for SHA-256, 40 for SHA-1, 32 for MD5, 128 for
#'   SHA-512 and 16 for XXH64), with `names(x)` kept. The values agree with
#'   `openssl::sha256(x, key)` and friends, and with
#'   `digest::digest(x, algo = "xxhash64", serialize = FALSE)`.
#' @family parsing, validation and hashing functions
#' @examples
#' fhash(c("abc", "", NA))
#' fhash("abc", "md5")
#'
#' # Keyed pseudonyms: the same ID always maps to the same value, but only
#' # holders of the key can produce it
#' ids <- c("QLD123456", "NSW987654", "QLD123456")
#' fhash(ids, key = "keep-this-secret")
#'
#' fhash("abc", "xxh64")
#' @export
fhash <- function(x, algo = c("sha256", "sha1", "md5", "sha512", "xxh64"),
                  key = NULL, seed = 0, nthreads = NULL) {
    x <- .character_x(x)
    algo <- match.arg(algo)
    keyed <- !is.null(key)
    if (keyed) {
        if (algo == "xxh64")
            stop("`key` is not supported for \"xxh64\"; use a SHA digest for ",
                 "keyed hashing.")
        if (is.character(key) && length(key) == 1L && !is.na(key)) {
            key <- charToRaw(enc2utf8(key))
        } else if (!is.raw(key)) {
            stop("`key` must be a single string or a raw vector.")
        }
    } else {
        key <- raw(0)
    }
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed < 0 ||
        seed != floor(seed) || seed > 2^53)
        stop("`seed` must be a single non-negative integer.")
    id <- match(algo, c("md5", "sha1", "sha256", "sha512", "xxh64")) - 1L
    .copy_names(fast_hash_impl(
        x, id, keyed, key, as.double(seed), .as_nthreads(nthreads)
    ), x)
}
