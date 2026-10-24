test_that("fparse_number() finds the first number in messy text", {
    x <- c("$1,234.50", "12.5%", "approx. 3,000 kg", "-$45", "$ -45", "SKU-123",
           "n/a", NA, "", ".5", "-.5", "1e3", "2.5E-2x", "1e", "1,", "1,2,3",
           "1,234.5,6", "(1,234)", "Total: -1,000", "x-y 3",
           paste0(intToUtf8(0x2212), "7"), c(a = "9"))
    expect_identical(
        fparse_number(x),
        c(1234.5, 12.5, 3000, -45, -45, 123, NA, NA, NA, 0.5, -0.5, 1000,
          0.025, 1, 1, 123, 1234.5, 1234, -1000, 3, -7, a = 9)
    )
})

test_that("fparse_number() reads European formats", {
    expect_identical(
        fparse_number(c("1.234,50 EUR", "12,5 %", "1 234 567,8"),
                      decimal_mark = ",", grouping_mark = c(".", " ")),
        c(1234.5, 12.5, 1234567.8)
    )
    expect_identical(fparse_number("1,234", grouping_mark = ""), 1)
    expect_identical(fparse_number("1,234", grouping_mark = character(0)), 1)
})

test_that("fparse_number() rounds exactly as as.numeric()", {
    set.seed(9)
    lit <- c(sprintf("%.17g", runif(2000) * 10^sample(-300:300, 2000, TRUE)),
             sprintf("%.2f", runif(2000, 0, 1e7)),
             "4.9e-324", "2.2250738585072011e-308", "1.7976931348623157e308",
             "0.1", "123456789012345678901234567890", "0.000000000000000000000001")
    expect_identical(fparse_number(lit), as.numeric(lit))
    expect_identical(fparse_number("1e400"), Inf)
    expect_identical(fparse_number("-1e400"), -Inf)
    expect_identical(fparse_number(lit, nthreads = 1), as.numeric(lit))
})

test_that("fparse_number() validates its marks", {
    expect_error(fparse_number("1", decimal_mark = ".."), "single character")
    expect_error(fparse_number("1", decimal_mark = ",", grouping_mark = ","),
                 "cannot also")
    expect_error(fparse_number("1", grouping_mark = "-"), "Marks")
    expect_error(fparse_number(1), "character")
})

test_that("valid_id() accepts published examples", {
    expect_identical(valid_id(c("51 824 753 556", "51 824 753 557", NA), "abn"),
                     c(TRUE, FALSE, NA))
    expect_true(all(valid_id(c("000 000 019", "000 250 000", "000 500 005",
                               "001 000 004", "004 085 616"), "acn")))
    expect_identical(valid_id(c("123 456 782", "123 456 783", "12345678"), "tfn"),
                     c(TRUE, FALSE, NA))
    expect_identical(valid_id(c("2123 45670 1", "2123 45671 1", "7123 45670 1",
                                "2123 45670 1 2"), "medicare"),
                     c(TRUE, FALSE, FALSE, TRUE))
    expect_identical(valid_id(c("4111 1111 1111 1111", "4111-1111-1111-1112",
                                "79927398713", "", "12a"), "luhn"),
                     c(TRUE, FALSE, TRUE, FALSE, FALSE))
    expect_identical(valid_id(c("978-0-306-40615-7", "0-306-40615-2",
                                "0-306-40615-3", "080442957X", "080442957x",
                                "977-0-306-40615-8"), "isbn"),
                     c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
    expect_identical(valid_id(c("4006381333931", "96385074", "4006381333932"), "ean"),
                     c(TRUE, TRUE, FALSE))
    expect_identical(valid_id(c("GB82 WEST 1234 5698 7654 32",
                                "gb82west12345698765432",
                                "DE89 3704 0044 0532 0130 00",
                                "GB82 WEST 1234 5698 7654 33",
                                "XX82 WEST 1234 5698 7654 32",
                                "GB82 WEST 1234 5698 7654 3"), "iban"),
                     c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
    expect_identical(valid_id(c(k = "51 824 753 556"), "abn"), c(k = TRUE))
    expect_error(valid_id("1", "passport"))
})

test_that("valid_id() agrees with python-stdnum", {
    ref <- utils::read.csv(test_path("fixtures", "valid_id.csv"),
                           colClasses = "character")
    for (type in unique(ref$type)) {
        rows <- ref[ref$type == type, ]
        expect_identical(valid_id(rows$x, type), rows$valid == "1", label = type)
    }
})
