test_that("factor", {
    vecs <- list(
        c(100L, 100L, 101L, 101L, NA),
        c(100.1, 100, 100.1, 100, NA),
        c("a", "b", "a", "b", NA)
    )
    for (vec in vecs) {
        expect_identical(
            object = unfactorize(as.factor(vec)),
            expected = vec
        )
    }
})

test_that("DFrame", {
    expect_identical(
        object = unfactorize(DataFrame(
            "a" = as.factor(c(100L, 100L, 101L, 101L)),
            "b" = as.factor(c("a", "b", "a", "b")),
            row.names = c("AAA", "BBB", "CCC", "DDD")
        )),
        expected = DataFrame(
            "a" = c(100L, 100L, 101L, 101L),
            "b" = c("a", "b", "a", "b"),
            row.names = c("AAA", "BBB", "CCC", "DDD")
        )
    )
})

test_that("Column selection with j", {
    expect_identical(
        object = unfactorize(
            object = DataFrame(
                "a" = as.factor(c(100L, 100L, 101L, 101L)),
                "b" = as.factor(c("a", "b", "a", "b")),
                row.names = c("AAA", "BBB", "CCC", "DDD")
            ),
            j = 2L
        ),
        expected = DataFrame(
            "a" = as.factor(c(100L, 100L, 101L, 101L)),
            "b" = c("a", "b", "a", "b"),
            row.names = c("AAA", "BBB", "CCC", "DDD")
        )
    )
    expect_identical(
        object = unfactorize(
            object = DataFrame(
                "a" = as.factor(c(100L, 100L, 101L, 101L)),
                "b" = as.factor(c("a", "b", "a", "b")),
                row.names = c("AAA", "BBB", "CCC", "DDD")
            ),
            j = "a"
        ),
        expected = DataFrame(
            "a" = c(100L, 100L, 101L, 101L),
            "b" = as.factor(c("a", "b", "a", "b")),
            row.names = c("AAA", "BBB", "CCC", "DDD")
        )
    )
    expect_error(
        object = unfactorize(
            object = DataFrame(
                "a" = as.factor(c(100L, 100L, 101L, 101L)),
                "b" = as.factor(c("a", "b", "a", "b")),
                row.names = c("AAA", "BBB", "CCC", "DDD")
            ),
            j = 1L:3L
        ),
        regexp = "length"
    )
    expect_error(
        object = unfactorize(
            object = DataFrame(
                "a" = as.factor(c(100L, 100L, 101L, 101L)),
                "b" = as.factor(c("a", "b", "a", "b")),
                row.names = c("AAA", "BBB", "CCC", "DDD")
            ),
            j = "c"
        ),
        regexp = "isSubset"
    )
})
