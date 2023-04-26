test_that("DFrame", {
    object <- DataFrame(
        "a" = c("a", "b", "c", "d"),
        "b" = c("a", "a", "b", "b"),
        "c" = c(1L, 2L, 3L, 4L),
        "d" = c(1L, 2L, 1L, 2L),
        "e" = c(TRUE, TRUE, FALSE, FALSE),
        row.names = c("A", "B", "C", "D")
    )
    object <- factorize(object)
    expected <- DataFrame(
        "a" = c("a", "b", "c", "d"),
        "b" = as.factor(c("a", "a", "b", "b")),
        "c" = c(1L, 2L, 3L, 4L),
        "d" = as.factor(c(1L, 2L, 1L, 2L)),
        "e" = c(TRUE, TRUE, FALSE, FALSE),
        row.names = c("A", "B", "C", "D")
    )
    expect_identical(object, expected)
})
