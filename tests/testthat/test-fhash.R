test_that("fhash() reproduces the standard test vectors", {
    abc <- "abc"
    long <- "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    expect_identical(fhash(c(abc, "", long)), c(
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    ))
    expect_identical(fhash(c(abc, ""), "sha1"), c(
        "a9993e364706816aba3e25717850c26c9cd0d89d",
        "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    ))
    expect_identical(fhash(c(abc, ""), "md5"), c(
        "900150983cd24fb0d6963f7d28e17f72",
        "d41d8cd98f00b204e9800998ecf8427e"
    ))
    expect_identical(fhash(abc, "sha512"), paste0(
        "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a",
        "2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
    ))
    expect_identical(fhash(c("", abc), "xxh64"),
                     c("ef46db3751d8e999", "44bc2cf5ad770999"))
    expect_identical(fhash(c(NA, k = abc), "md5"),
                     c(NA, k = "900150983cd24fb0d6963f7d28e17f72"))
})

test_that("fhash() computes HMACs (RFC 2202 and RFC 4231)", {
    msg <- "what do ya want for nothing?"
    expect_identical(fhash(msg, "sha256", key = "Jefe"),
                     "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
    expect_identical(fhash(msg, "sha1", key = "Jefe"),
                     "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79")
    expect_identical(fhash(msg, "md5", key = "Jefe"),
                     "750c783e6ab0b503eaa86e310a5db738")
    expect_identical(fhash(msg, key = "Jefe"), fhash(msg, key = charToRaw("Jefe")))
    expect_error(fhash("a", "xxh64", key = "k"), "not supported")
    expect_error(fhash("a", key = 1), "string or a raw")
})

test_that("fhash() hashes the UTF-8 encoding", {
    l1 <- "caf\xe9"
    Encoding(l1) <- "latin1"
    expect_identical(fhash(l1), fhash(enc2utf8(l1)))
})

test_that("fhash() agrees with openssl and digest", {
    skip_if_not_installed("openssl")
    set.seed(4)
    pool <- c(letters, 0:9, " ", intToUtf8(0xe9), intToUtf8(0x4e2d))
    lens <- c(0:3, 54:57, 62:66, 110:113, 126:130, 1000, sample(0:300, 60, TRUE))
    x <- vapply(lens, function(n) paste(sample(pool, n, TRUE), collapse = ""), "")
    key <- as.raw(sample(0:255, 100, TRUE))
    expect_identical(fhash(x), as.character(openssl::sha256(x)))
    expect_identical(fhash(x, "sha1"), as.character(openssl::sha1(x)))
    expect_identical(fhash(x, "md5"), as.character(openssl::md5(x)))
    expect_identical(fhash(x, "sha512"), as.character(openssl::sha512(x)))
    expect_identical(fhash(x, key = key), as.character(openssl::sha256(x, key = key)))
    expect_identical(fhash(x, "sha512", key = key),
                     as.character(openssl::sha512(x, key = key)))
    skip_if_not_installed("digest")
    expect_identical(
        fhash(x, "xxh64", seed = 7),
        vapply(x, digest::digest, "", algo = "xxhash64", serialize = FALSE,
               seed = 7, USE.NAMES = FALSE)
    )
})
