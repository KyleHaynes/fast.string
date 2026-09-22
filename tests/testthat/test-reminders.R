test_that("every reminder target names a real exported fast.string function", {
    for (info in fast.string:::.reminder_map) {
        expect_true(
            exists(info$fast, where = asNamespace("fast.string"), inherits = FALSE),
            info = paste("missing fast.string function:", info$fast)
        )
    }
})

test_that("reminder wrappers return exactly what the base function returns", {
    x <- c("apple pie", "banana split", NA, "cherry tart")

    expect_identical(fast.string:::.reminder_grepl("an", x), base::grepl("an", x))
    expect_identical(fast.string:::.reminder_grep("an", x), base::grep("an", x))
    expect_identical(fast.string:::.reminder_sub("a", "A", x), base::sub("a", "A", x))
    expect_identical(fast.string:::.reminder_gsub("a", "A", x), base::gsub("a", "A", x))
    expect_identical(fast.string:::.reminder_trimws("  hi  "), base::trimws("  hi  "))
    expect_identical(fast.string:::.reminder_substr(x, 1, 3), base::substr(x, 1, 3))
    expect_identical(fast.string:::.reminder_nchar(x), base::nchar(x))
    expect_identical(fast.string:::.reminder_chartr("a", "A", x), base::chartr("a", "A", x))
})

test_that(".remind() is silent when not interactive, regardless of the option", {
    old <- options(fast.string.reminders = TRUE)
    on.exit(options(old), add = TRUE)

    expect_silent(fast.string:::.remind("grepl", is_interactive = FALSE))
})

test_that(".remind() is silent when the option is off, even if interactive", {
    old <- options(fast.string.reminders = FALSE)
    on.exit(options(old), add = TRUE)

    expect_silent(fast.string:::.remind("grepl", is_interactive = TRUE))
})

test_that(".remind() names the base function and its fast.string equivalent", {
    old <- options(fast.string.reminders = TRUE)
    on.exit(options(old), add = TRUE)

    expect_message(
        fast.string:::.remind("nchar", is_interactive = TRUE),
        "nchar.*fnchar"
    )
})

test_that(".remind() mentions how to silence future reminders", {
    old <- options(fast.string.reminders = TRUE)
    on.exit(options(old), add = TRUE)

    expect_message(
        fast.string:::.remind("substr", is_interactive = TRUE),
        "fast.string.reminders = FALSE", fixed = TRUE
    )
})

test_that("attach_reminders()/detach_reminders() mask and unmask base functions", {
    skip_if(fast.string:::.reminders_search_name %in% search())
    on.exit(fast.string:::.detach_reminders(), add = TRUE)

    fast.string:::.attach_reminders()
    expect_true(fast.string:::.reminders_search_name %in% search())
    expect_identical(find("grepl")[[1L]], "fast.string:reminders")

    fast.string:::.detach_reminders()
    expect_false(fast.string:::.reminders_search_name %in% search())
})

test_that("attach_reminders() is idempotent", {
    skip_if(fast.string:::.reminders_search_name %in% search())
    on.exit(fast.string:::.detach_reminders(), add = TRUE)

    fast.string:::.attach_reminders()
    fast.string:::.attach_reminders()
    expect_identical(sum(search() == fast.string:::.reminders_search_name), 1L)
})
