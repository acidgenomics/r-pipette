context("dots")

test_that("dots", {
    expect_identical(
        object = dots(a, b, c),
        expected = list(
            as.name("a"),
            as.name("b"),
            as.name("c")
        )
    )
    expect_identical(
        object = dots(a, b, c, character = TRUE),
        expected = c("a", "b", "c")
    )
    expect_error(
        object = dots(a, a, b, c),
        regexp = "hasNoDuplicates"
    )
    expect_error(
        object = dots(),
        regexp = "hasLength"
    )
})
