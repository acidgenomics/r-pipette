context("metadata2")

with_parameters_test_that(
    "metadata2", {
        which <- "A"
        expect_null(metadata2(x, which = which))
        value <- "B"
        metadata2(x, which = which) <- value
        expect_identical(
            object = metadata2(x, which = which),
            expected = value
        )
    },
    x = list(
        data.frame(),
        S4Vectors::DataFrame()
    )
)
