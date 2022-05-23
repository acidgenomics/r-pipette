test_that("sanitizeNA", {
    mapply(
        object = list(
            "character" = c(1L, "x", "", "NA"),
            "data.frame" = data.frame(
                a = c("foo", ""),
                b = c(NA, "bar"),
                stringsAsFactors = FALSE
            ),
            "DataFrame1" = DataFrame(
                "a" = c("foo", ""),
                "b" = c(NA, "bar"),
                row.names = c("c", "d")
            ),
            "DataFrame2" = DataFrame(
                "a" = c("foo", ""),
                "b" = c(NA, "bar")
            )
        ),
        expected = list(
            "character" = c("1", "x", NA, NA),
            "data.frame" = data.frame(
                "a" = c("foo", NA),
                "b" = c(NA, "bar"),
                stringsAsFactors = FALSE
            ),
            "DataFrame1" = DataFrame(
                "a" = c("foo", NA),
                "b" = c(NA, "bar"),
                row.names = c("c", "d")
            ),
            "DataFrame2" = DataFrame(
                "a" = c("foo", NA),
                "b" = c(NA, "bar")
            )
        ),
        FUN = function(object, expected) {
            expect_identical(
                object = sanitizeNA(object),
                expected = expected
            )
        },
        SIMPLIFY = FALSE
    )
})

test_that("Harden against unexpected factor level swap", {
    object <- factor(
        x = rep(x = "transcript", times = 5L),
        levels = c(
            "gene",
            "transcript",
            "exon",
            "CDS",
            "three_prime_utr",
            "start_codon",
            "five_prime_utr",
            "stop_codon"
        )
    )
    expect_identical(
        object = sanitizeNA(object),
        expected = object
    )
})

test_that("Named factor", {
    x <- rep(c("a", "b", "NA"), times = 2L, each = 2L)
    x <- as.factor(x)
    names(x) <- letters[seq_len(length(x))]
    expect_false(anyNA(x))
    expect_s3_class(x, "factor")
    y <- sanitizeNA(x)
    expect_identical(names(x), names(y))
    expect_true(anyNA(y))
    expect_s3_class(y, "factor")
})


test_that("Don't modify atomic", {
    expect_identical(sanitizeNA(NA), NA)
})
