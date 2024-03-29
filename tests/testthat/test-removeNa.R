## Support for vectors (using `stats::na.omit`).
## This will return structure attributes about original size, with class omit.
test_that("removeNa", {
    Map(
        object = list(
            "character" = c("hello", "world", NA),
            "numeric" = c(1L, 2L, NA),
            "DFrame" = DataFrame(
                "a" = c("A", NA, "C"),
                "b" = c(NA, NA, NA),
                "c" = c("B", NA, "D"),
                row.names = c("x", "y", "z")
            )
        ),
        expected = list(
            ## nolint start
            "character" = structure(
                .Data = c("hello", "world"),
                na.action = structure(3L, class = "omit")
            ),
            "numeric" = structure(
                .Data = c(1L, 2L),
                na.action = structure(3L, class = "omit")
            ),
            ## nolint end
            "DFrame" = DataFrame(
                "a" = c("A", "C"),
                "c" = c("B", "D"),
                row.names = c("x", "z")
            )
        ),
        f = function(object, expected) {
            expect_identical(
                object = removeNa(object),
                expected = expected
            )
        }
    )
})
