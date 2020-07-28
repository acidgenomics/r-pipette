context("metadata2")

test_that("metadata2", {
    for (x = list(data.frame(), S4Vectors::DataFrame())) {
        which <- "A"
        expect_null(metadata2(x, which = which))
        value <- "B"
        metadata2(x, which = which) <- value
        expect_identical(
            object = metadata2(x, which = which),
            expected = value
        )
    }
})
