# The failing rLink() call, fixed.
# ===========================================================================
#
# What went wrong: the error
#
#     Error: Element 1 of `compare` does not name a function.
#
# means `jaro_winkler_tokens` could not be found -- or was not a function --
# where the call was written. The expressions in `compare` (and in `block_1`,
# `block_2` and `filter`) are your code and are evaluated in your
# environment, so until now `fast.string::rLink()` on its own was not enough:
# the comparators had to be visible as well, which is what
# `library(fast.string)` does. Blocking got further only because the names it
# used -- `soundex()` above all -- can come from somewhere else (phonics and
# RecordLinkage both export a `soundex()`), and there is no second
# `jaro_winkler_tokens()` anywhere to stand in.
#
# Two things changed in rLink() because of this:
#
#   1. A function the call names, which your environment cannot see and this
#      package exports, is now taken from the package itself, so
#      `fast.string::rLink(compare = list(jaro_winkler_tokens(x)))` works
#      with nothing attached. A function of your own, or another attached
#      package's, of the same name still wins -- which is exactly why the
#      check below is worth keeping.
#   2. The message now says what it actually found: "there is no function
#      `jaro_winklr`", or "`surname` is a character, not a function", rather
#      than one sentence for every cause.

library(fast.string)

# Which `soundex()` the keys were built with. If this says anything but
# `namespace:fast.string`, the passes above were blocked on another package's
# phonetic code -- still a valid key, but not the one you meant, and not the
# one `rLink_inspect()` will show you.
for (f in c("soundex", "jaro_winkler", "jaro_winkler_tokens", "fgsub")) {
    where <- environment(get(f))
    cat(sprintf("%-20s %s\n", f, environmentName(where)))
}


# --- the call --------------------------------------------------------------
#
# Unchanged from yours apart from the comparators: the commented-out ones are
# now inside their own commented block, so the live list ends in a comma-free
# element and nothing has to be un-commented in two places at once.

straight <- fast.string::rLink(
    data_1 = d,
    data_2 = q,
    block_1 = list(
        email       = email_1,
        # Every phone column in `d` against every phone column in `q`.
        ph_home_tel = phone_1,
        ph_home_mob = phone_1,
        ph_home_oth = phone_1,
        ph_work_tel = phone_2,
        ph_work_mob = phone_2,
        ph_work_oth = phone_2,
        ph_mob_tel  = phone_3,
        ph_mob_mob  = phone_3,
        ph_mob_oth  = phone_3,
        # soundexPos = c(1, 2), missingPos = c()
        name_dob    = list(soundex(first_name),
                           soundex(last_name),
                           dob),
        addr_pc     = list(soundex(last_name), flat_number, number_first,
                           postcode),
        # soundexPos = c(1, 4), missingPos = c(1, 4)
        addr_loc    = list(soundex(last_name), flat_number, number_first,
                           soundex(locality)),
        # soundexPos = c(1), missingPos = c(1, 2, 3)
        po_box      = list(soundex(last_name), post_box_no, postcode)
    ),

    # One pass for every pass of `block_1`, with the same number of terms.
    # NULL reuses `block_1`'s expressions, for a pass whose columns are named
    # the same in both files.
    block_2 = list(
        email_1,
        phone_1, phone_2, phone_3,
        phone_1, phone_2, phone_3,
        phone_1, phone_2, phone_3,
        NULL,
        list(soundex(last_name), unit_no, street_number, postcode),
        list(soundex(last_name), unit_no, street_number, soundex(locality)),
        list(soundex(last_name), post_box_no, postal_postcode)
    ),

    # Comparisons (winkler etc.). fgsub() puts the old split character back
    # where jaro_winkler_tokens() looks for it: it strips hyphens by default,
    # which joins "SMITH-JONES" into one token instead of two.
    compare = list(
        first_name  = jaro_winkler_tokens(
            fgsub("-", " ", first_name, fixed = TRUE)),
        last_name   = jaro_winkler_tokens(
            fgsub("-", " ", last_name, fixed = TRUE)),
        given_names = jaro_winkler_tokens(
            fgsub("-", " ", given_names, fixed = TRUE)),
        dob         = jaro_winkler(dob)

        # The rest of the old winkler1/winkler2 pairs, to add back one at a
        # time. Put a comma after `jaro_winkler(dob)` above when you do.
        #
        # street_name     = jaro_winkler_tokens(
        #     fgsub("-", " ", street_name, fixed = TRUE)),
        # locality        = jaro_winkler_tokens(
        #     fgsub("-", " ", locality, fixed = TRUE)),
        # locality_postal = jaro_winkler_tokens(
        #     fgsub("-", " ", locality, fixed = TRUE) |
        #     fgsub("-", " ", postal_locality, fixed = TRUE)),
        # postcode        = jaro_winkler_tokens(
        #     fgsub("-", " ", postcode, fixed = TRUE)),
        # postcode_postal = jaro_winkler(postcode | postal_postcode)
    ),
    verbose = TRUE,
    pair_id = "frame_id"
)

# What each pass cost and found, and what each score is on its own scale.
attr(straight, "blocking")
attr(straight, "compare")
summary(straight)


# --- the same call, with nothing attached ----------------------------------
#
# This is what the fix buys: no library(fast.string), every function of the
# package reached through the namespace, and the comparators still resolve.
# Run it in a fresh session to confirm.
#
# straight_2 <- fast.string::rLink(
#     data_1  = d,
#     data_2  = q,
#     block_1 = list(name_dob = list(soundex(first_name),
#                                    soundex(last_name), dob)),
#     compare = list(first_name = jaro_winkler_tokens(first_name)),
#     verbose = TRUE,
#     pair_id = "frame_id"
# )
