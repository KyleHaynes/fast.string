# Two realistic person files for the documentation, with a known truth.
#
# Sourced by the chapters that need something to link. Nothing here is
# fast.string-specific: it is a generator for the kind of pair of files a
# linkage project is actually handed --- the same people, recorded by two
# systems that disagree about everything except who the people are.
#
#   people_files(n_enrol, overlap, seed)
#     $enrol   a service enrolment extract
#     $claims  a claims extract from another system
#     $truth   person_id for every row of each file, for scoring
#
# The two files differ the way real ones do:
#
#   field      enrol                      claims
#   --------   ------------------------   ---------------------------------
#   name       given / middle / surname   one "SURNAME, GIVEN MIDDLE" field
#   case       upper                      mixed, with titles and trailing
#                                         whitespace
#   dob        Date                       "DD/MM/YYYY" text
#   sex        "M" / "F" / "U"            "Male" / "Female" / "Unknown" / ""
#   address    unit/number/street/type    one free-text line, abbreviated
#   postcode   character, zero-padded     numeric, leading zeros lost
#
# and the overlapping people carry the errors that make linkage hard:
# nicknames, married surnames, transposed date digits, missing middle
# names, a moved address, a lost apostrophe.
#
# Given names are drawn from sex-specific pools so that a printed row reads
# like a real one, and so that sex is a comparison field that carries
# information rather than noise.

people_files <- function(n_enrol = 20000, overlap = 0.6, seed = 20260101) {
    set.seed(seed)

    male_pool <- c(
        "JOHN", "ROBERT", "MICHAEL", "WILLIAM", "DAVID", "JAMES", "PETER",
        "ANDREW", "DANIEL", "MATTHEW", "THOMAS", "JOSHUA", "RYAN",
        "BENJAMIN", "LUKE", "KYLE", "NATHAN", "SAMUEL", "MOHAMMED", "WEI",
        "ARJUN", "HIROSHI", "GIUSEPPE", "DIMITRI", "SIOSAIA"
    )
    female_pool <- c(
        "MARY", "PATRICIA", "JENNIFER", "LINDA", "ELIZABETH", "BARBARA",
        "SUSAN", "JESSICA", "SARAH", "EMILY", "OLIVIA", "EMMA", "GRACE",
        "SOPHIE", "HANNAH", "ZOE", "AMY", "LUCY", "FATIMA", "LI", "PRIYA",
        "YUKI", "MARIA", "KATERINA", "MELE"
    )
    surname_pool <- c(
        "SMITH", "JONES", "WILLIAMS", "BROWN", "WILSON", "TAYLOR", "JOHNSON",
        "WHITE", "MARTIN", "ANDERSON", "THOMPSON", "NGUYEN", "THOMAS",
        "WALKER", "HARRIS", "LEE", "RYAN", "ROBINSON", "KELLY", "KING",
        "DAVIS", "WRIGHT", "EVANS", "ROBERTS", "GREEN", "HALL", "WOOD",
        "JACKSON", "CLARKE", "PATEL", "KHAN", "LEWIS", "JAMES", "PHILLIPS",
        "MITCHELL", "CAMPBELL", "MORRIS", "YOUNG", "SCOTT", "MURPHY", "COOK",
        "BAKER", "TURNER", "COOPER", "HILL", "WARD", "HUGHES", "MOORE",
        "CHEN", "WANG", "SINGH", "O'BRIEN", "O'CONNOR", "MCDONALD", "STEWART",
        "MACDONALD", "SULLIVAN", "MURRAY", "PRICE", "BELL", "ALLEN",
        "DE SILVA", "VAN DER BERG", "DI MARCO", "EL-MASRI", "TE RANGI",
        "SMITH-JONES", "WATSON-BROWN"
    )
    street_pool <- c(
        "HIGH", "MAIN", "CHURCH", "STATION", "PARK", "VICTORIA", "GEORGE",
        "KING", "QUEEN", "ELIZABETH", "COLLINS", "BOURKE", "FLINDERS",
        "SWANSTON", "ADELAIDE", "WICKHAM", "BRUNSWICK", "LOGAN", "IPSWICH",
        "SANDGATE", "GYMPIE", "STANLEY", "RIVER", "BEACH", "FOREST", "HILL"
    )
    street_type <- c("STREET", "ROAD", "AVENUE", "COURT", "PLACE", "TERRACE",
                     "PARADE", "CRESCENT", "DRIVE", "LANE")
    suburb_pool <- c("RICHMOND", "FITZROY", "CARLTON", "BRUNSWICK", "PRESTON",
                     "FOOTSCRAY", "ST KILDA", "PRAHRAN", "HAWTHORN",
                     "CAMBERWELL", "MOUNT WAVERLEY", "GLEN IRIS")
    # Nicknames, so a given name can disagree without being a typo.
    nickname <- c(JOHN = "JACK", ROBERT = "BOB", WILLIAM = "BILL",
                  ELIZABETH = "LIZ", JAMES = "JIM", PATRICIA = "PAT",
                  MICHAEL = "MIKE", JENNIFER = "JENNY", SUSAN = "SUE",
                  THOMAS = "TOM", DANIEL = "DAN", BENJAMIN = "BEN",
                  SAMUEL = "SAM", MATTHEW = "MATT", ANDREW = "ANDY",
                  JOSHUA = "JOSH", EMILY = "EM", SOPHIE = "SOPH")

    # Real name distributions have a short, very frequent head and a long
    # tail: the twenty commonest surnames cover a good share of a file, and
    # the rest are spread over thousands. A pool of sixty names would make
    # every blocking block enormous and every example unrealistic, so the
    # recognisable names above are the head and a generated tail follows
    # them, drawn with Zipf-like weights.
    # Consonant-vowel syllables, so a tail name is pronounceable rather
    # than a run of consonants.
    syllable <- c("BA", "BE", "BI", "BO", "BU", "DA", "DE", "DI", "DO", "DU",
                  "FA", "FE", "FI", "GA", "GE", "GI", "GO", "HA", "HE", "HO",
                  "JA", "JO", "KA", "KE", "KI", "KO", "LA", "LE", "LI", "LO",
                  "LU", "MA", "ME", "MI", "MO", "MU", "NA", "NE", "NI", "NO",
                  "PA", "PE", "PI", "PO", "RA", "RE", "RI", "RO", "RU", "SA",
                  "SE", "SI", "SO", "TA", "TE", "TI", "TO", "TU", "VA", "VE",
                  "VI", "WA", "WE", "ZA", "ZE", "CHA", "SHE", "PHI", "THO",
                  "BRA", "CLE", "STRO", "GRA", "PRE", "TRI")
    coda <- c("", "", "", "N", "S", "R", "LL", "SON", "SEN", "SKI", "EZ")
    make_tail <- function(k, parts)
        unique(vapply(seq_len(k), function(i)
            paste0(paste(sample(syllable, sample(parts, 1L), TRUE), collapse = ""),
                   sample(coda, 1L)), ""))

    # A gentle exponent, so the head is frequent without one name swamping
    # the file: the commonest surname lands near 2% of records.
    zipf_weights <- function(k, head_n, head_share) {
        w <- 1 / (seq_len(k) + 3)^0.75
        w[seq_len(head_n)] <- w[seq_len(head_n)] *
            (head_share / (1 - head_share)) *
            (sum(w[-seq_len(head_n)]) / sum(w[seq_len(head_n)]))
        w / sum(w)
    }

    surname_pool <- c(surname_pool, make_tail(6000, 2:3))
    surname_w <- zipf_weights(length(surname_pool), 68L, 0.5)
    male_pool <- c(male_pool, make_tail(1200, 2:2))
    female_pool <- c(female_pool, make_tail(1200, 2:2))
    male_w <- zipf_weights(length(male_pool), 25L, 0.7)
    female_w <- zipf_weights(length(female_pool), 25L, 0.7)

    draw_surname <- function(k) sample(surname_pool, k, TRUE, prob = surname_w)

    draw_given <- function(sex, avoid = NULL) {
        out <- character(length(sex))
        male <- sex != "F"            # "U" is drawn from the male pool
        out[male] <- sample(male_pool, sum(male), TRUE, prob = male_w)
        out[!male] <- sample(female_pool, sum(!male), TRUE, prob = female_w)
        # A middle name is drawn "avoiding" the given name, so nobody is
        # called Thomas Thomas.
        if (!is.null(avoid)) {
            clash <- which(out == avoid)
            for (i in clash) {
                repeat {
                    out[i] <- draw_given(sex[i])
                    if (out[i] != avoid[i]) break
                }
            }
        }
        out
    }

    # --- the people themselves ---------------------------------------------
    n <- n_enrol
    sex <- sample(c("M", "F", "U"), n, TRUE, prob = c(0.485, 0.485, 0.03))
    given <- draw_given(sex)
    person <- data.frame(
        person_id   = seq_len(n),
        given_name  = given,
        middle_name = ifelse(runif(n) < 0.35, NA_character_,
                             draw_given(sex, given)),
        surname     = draw_surname(n),
        dob         = as.Date("1935-01-01") + sample(0:31000, n, TRUE),
        sex         = sex,
        unit        = ifelse(runif(n) < 0.25,
                             as.character(sample(1:30, n, TRUE)), NA_character_),
        number      = sample(1:450, n, TRUE),
        street      = sample(street_pool, n, TRUE),
        type        = sample(street_type, n, TRUE),
        suburb      = sample(suburb_pool, n, TRUE),
        state       = "VIC",
        postcode    = sprintf("%04d", sample(c(800:899, 3000:3200), n, TRUE)),
        stringsAsFactors = FALSE
    )

    # --- file 1: the enrolment extract, tidy and already upper-cased -------
    enrol <- data.frame(
        enrol_id    = sprintf("E%06d", seq_len(n)),
        given_name  = person$given_name,
        middle_name = person$middle_name,
        surname     = person$surname,
        dob         = person$dob,
        sex         = person$sex,
        unit        = person$unit,
        number      = person$number,
        street      = person$street,
        street_type = person$type,
        suburb      = person$suburb,
        state       = person$state,
        postcode    = person$postcode,
        stringsAsFactors = FALSE
    )

    # --- file 2: the claims extract ----------------------------------------
    # The overlapping people, plus some the enrolment file has never seen.
    shared <- sort(sample(n, round(n * overlap)))
    extra <- round(n * overlap * 0.25)
    claims_person <- c(shared, rep(NA_integer_, extra))
    m <- length(claims_person)
    new <- is.na(claims_person)
    n_new <- sum(new)

    pick <- function(column, fresh) {
        value <- person[[column]][claims_person]
        if (n_new) value[new] <- fresh
        value
    }
    sex_new <- sample(c("M", "F", "U"), n_new, TRUE)
    given_new <- draw_given(sex_new)
    sx     <- pick("sex", sex_new)
    given  <- pick("given_name", given_new)
    middle <- pick("middle_name", ifelse(runif(n_new) < 0.35, NA_character_,
                                         draw_given(sex_new, given_new)))
    surn   <- pick("surname", draw_surname(n_new))
    dob    <- pick("dob", as.Date("1935-01-01") + sample(0:31000, n_new, TRUE))
    unit   <- pick("unit", ifelse(runif(n_new) < 0.25,
                                  as.character(sample(1:30, n_new, TRUE)),
                                  NA_character_))
    number <- pick("number", sample(1:450, n_new, TRUE))
    street <- pick("street", sample(street_pool, n_new, TRUE))
    type   <- pick("type", sample(street_type, n_new, TRUE))
    suburb <- pick("suburb", sample(suburb_pool, n_new, TRUE))
    pcode  <- pick("postcode", sprintf("%04d", sample(c(800:899, 3000:3200),
                                                      n_new, TRUE)))

    # --- the errors, one per overlapping record ---------------------------
    e <- runif(m)

    # A nickname instead of the registered given name.
    nick <- e < 0.12 & given %in% names(nickname)
    given[nick] <- unname(nickname[given[nick]])

    # A single-character typo in the surname.
    typo <- function(s) {
        k <- nchar(s)
        i <- pmax(1L, pmin(k, floor(runif(length(s)) * k) + 1L))
        substr(s, i, i) <- sample(LETTERS, length(s), TRUE)
        s
    }
    mis <- e >= 0.12 & e < 0.22
    surn[mis] <- typo(surn[mis])

    # A married surname: the registered one is gone entirely.
    married <- e >= 0.22 & e < 0.28 & sx == "F"
    surn[married] <- draw_surname(sum(married))

    # Day and month transposed. An explicit format makes an impossible date
    # NA rather than an error, so the swap only lands where it is real.
    swap <- which(e >= 0.28 & e < 0.36)
    swapped <- as.Date(paste(format(dob[swap], "%Y"), format(dob[swap], "%d"),
                             format(dob[swap], "%m"), sep = "-"),
                       format = "%Y-%m-%d")
    dob[swap[!is.na(swapped)]] <- swapped[!is.na(swapped)]

    # A date of birth a year out, or the 1 January placeholder.
    off <- e >= 0.36 & e < 0.40
    dob[off] <- dob[off] + sample(c(-366L, 366L), sum(off), TRUE)
    placeholder <- e >= 0.40 & e < 0.42
    dob[placeholder] <- as.Date(paste0(format(dob[placeholder], "%Y"), "-01-01"))

    # The middle name was never collected.
    middle[e >= 0.42 & e < 0.60] <- NA_character_

    # They moved: a different address, the same person.
    moved <- e >= 0.60 & e < 0.70
    number[moved] <- sample(1:450, sum(moved), TRUE)
    street[moved] <- sample(street_pool, sum(moved), TRUE)
    suburb[moved] <- sample(suburb_pool, sum(moved), TRUE)
    pcode[moved]  <- sprintf("%04d", sample(c(800:899, 3000:3200),
                                            sum(moved), TRUE))

    # Sex unrecorded.
    sx[e >= 0.70 & e < 0.74] <- "U"

    # --- how the claims system writes it all down -------------------------
    title <- rep("", m)
    has_title <- runif(m) < 0.45
    title[has_title & sx == "M"] <- sample(c("MR", "DR"), sum(has_title & sx == "M"),
                                           TRUE, prob = c(0.85, 0.15))
    title[has_title & sx == "F"] <- sample(c("MS", "MRS", "DR"),
                                           sum(has_title & sx == "F"), TRUE,
                                           prob = c(0.45, 0.4, 0.15))

    # Title case, which is how a web form hands text back.
    pretty <- function(s)
        gsub("(^|[ '-])([a-z])", "\\1\\U\\2", tolower(s), perl = TRUE)

    # Some apostrophes were lost on the way through a legacy system.
    lost <- runif(m) < 0.3
    surn_written <- surn
    surn_written[lost] <- gsub("'", "", surn_written[lost], fixed = TRUE)

    name_field <- paste0(
        pretty(surn_written), ", ",
        trimws(paste(pretty(title), pretty(given),
                     ifelse(is.na(middle), "", pretty(middle)))))
    name_field <- gsub(" +", " ", name_field)
    # Doubled and trailing whitespace, as exported.
    doubled <- runif(m) < 0.2
    name_field[doubled] <- sub(", ", ",  ", name_field[doubled], fixed = TRUE)
    ragged <- runif(m) < 0.25
    name_field[ragged] <- paste0(name_field[ragged], "  ")

    abbrev <- c(STREET = "ST", ROAD = "RD", AVENUE = "AVE", COURT = "CT",
                PLACE = "PL", TERRACE = "TCE", PARADE = "PDE",
                CRESCENT = "CRES", DRIVE = "DR", LANE = "LN")
    type_written <- ifelse(runif(m) < 0.65, unname(abbrev[type]), type)
    address_line <- paste0(
        ifelse(is.na(unit), "", paste0("U", unit, "/")),
        number, " ", pretty(street), " ", pretty(type_written), ", ",
        pretty(suburb), " VIC")

    gender <- unname(c(M = "Male", F = "Female", U = "")[sx])
    gender[gender == "" & runif(m) < 0.5] <- "Unknown"

    claims <- data.frame(
        claim_id      = sprintf("C%06d", seq_len(m)),
        patient_name  = name_field,          # "SURNAME, Title Given Middle"
        date_of_birth = format(dob, "%d/%m/%Y"),
        gender        = gender,
        address_line  = address_line,
        postcode      = as.numeric(pcode),   # numeric, so 0812 arrives as 812
        stringsAsFactors = FALSE
    )

    # Shuffle, so nothing can be matched by row order.
    shuffled <- sample(m)
    claims <- claims[shuffled, ]
    rownames(claims) <- NULL

    # --- contact details --------------------------------------------------
    # Drawn last, and only from values already fixed above, so that adding
    # them left every other column of both files exactly as it was: each
    # sample() here comes after every other one in the function, so no
    # earlier draw moved.
    #
    # Phone and email behave differently from a name in a linkage, and that
    # is the point of having them. They are *near-identifiers*: when two
    # records agree on a mobile number they are almost certainly the same
    # person, so agreement is worth far more than it is on a surname. But
    # they are missing far more often, they go stale, and a household shares
    # a landline -- so disagreement says much less. A comparator on them
    # should be exact-or-missing, and the missing has to be kept distinct
    # from the disagreeing.
    digits <- function(k, n) vapply(seq_len(k), function(i)
        paste(sample(0:9, n, TRUE), collapse = ""), "")

    # A mobile for most people, a landline for the rest. The landline is
    # drawn per *address*, not per person, so the people sharing a household
    # share it -- which is exactly the false match a phone number invites.
    household <- paste(person$number, person$street, person$postcode)
    hh <- match(household, unique(household))
    hh_landline <- paste0("0", sample(2:8, length(unique(household)), TRUE),
                          " ", digits(length(unique(household)), 4), " ",
                          digits(length(unique(household)), 4))
    mobile <- paste0("04", digits(n, 2), " ", digits(n, 3), " ",
                     digits(n, 3))
    is_mobile <- runif(n) < 0.72
    person_phone <- ifelse(is_mobile, mobile, hh_landline[hh])
    person_phone[runif(n) < 0.08] <- NA_character_      # never collected

    # given.surname@provider, with the punctuation a real address loses and
    # a disambiguating number on some of them.
    local_part <- paste0(tolower(person$given_name), ".",
                         gsub("[^a-z]", "", tolower(person$surname)))
    tagged <- runif(n) < 0.3
    local_part[tagged] <- paste0(local_part[tagged],
                                 substr(format(person$dob[tagged], "%Y"), 3, 4))
    provider <- sample(c("example.com", "example.net", "example.org",
                         "mail.example.com"), n, TRUE,
                       prob = c(0.5, 0.25, 0.15, 0.1))
    person_email <- paste0(local_part, "@", provider)
    person_email[runif(n) < 0.18] <- NA_character_      # never collected

    enrol$phone <- person_phone
    enrol$email <- person_email

    # The claims side: the same contact details for the same people, then
    # the usual damage. `src` is the person each claims row is about, NA for
    # the people the enrolment file has never seen.
    src <- claims_person[shuffled]
    c_phone <- person_phone[src]
    c_email <- person_email[src]
    fresh <- is.na(src)
    if (any(fresh)) {
        k <- sum(fresh)
        c_phone[fresh] <- paste0("04", digits(k, 2), " ", digits(k, 3), " ",
                                 digits(k, 3))
        c_email[fresh] <- paste0("person", digits(k, 5), "@example.com")
    }
    g <- runif(m)
    # Written without spaces, or with the country code: the same number,
    # spelled differently. This is what fdigits() is for.
    c_phone[g < 0.30] <- gsub(" ", "", c_phone[g < 0.30], fixed = TRUE)
    plus <- which(g >= 0.30 & g < 0.40 & !is.na(c_phone))
    c_phone[plus] <- paste0("+61 ", substring(gsub(" ", "", c_phone[plus]), 2))
    # Changed number, and never collected at all.
    moved_phone <- which(g >= 0.40 & g < 0.52)
    c_phone[moved_phone] <- paste0("04", digits(length(moved_phone), 2), " ",
                                   digits(length(moved_phone), 3), " ",
                                   digits(length(moved_phone), 3))
    c_phone[g >= 0.52 & g < 0.74] <- NA_character_
    # Email: a different provider for the same local part, a typed address,
    # and the usual silence.
    h <- runif(m)
    swap_provider <- which(h < 0.12 & !is.na(c_email))
    c_email[swap_provider] <- paste0(sub("@.*$", "", c_email[swap_provider]),
                                     "@", sample(c("example.net",
                                                   "example.org"),
                                                 length(swap_provider), TRUE))
    c_email[h >= 0.12 & h < 0.16] <- NA_character_
    # Case and stray whitespace, as typed into a form.
    mixed <- which(h >= 0.16 & h < 0.34 & !is.na(c_email))
    c_email[mixed] <- sub("^(.)", "\\U\\1", c_email[mixed], perl = TRUE)
    padded <- which(h >= 0.34 & h < 0.40 & !is.na(c_email))
    c_email[padded] <- paste0(" ", c_email[padded], " ")
    # The claims system writes a blank rather than a null.
    claims$contact_phone <- ifelse(is.na(c_phone), "", c_phone)
    claims$contact_email <- ifelse(is.na(c_email), "", c_email)

    list(
        enrol = enrol,
        claims = claims,
        truth = list(enrol = person$person_id,
                     claims = claims_person[shuffled])
    )
}

# The number of truly matching pairs between the two files, for recall.
true_pair_count <- function(files)
    sum(!is.na(files$truth$claims))

# TRUE where a pair of row indices is the same person.
same_person <- function(files, i, j) {
    a <- files$truth$enrol[i]
    b <- files$truth$claims[j]
    !is.na(a) & !is.na(b) & a == b
}
