test_that("factor", {
    vecs <- list(
        c(100L, 100L, 101L, 101L),
        c(100.1, 100, 100.1, 100),
        c("a", "b", "a", "b")
    )
    for (vec in vecs) {
        expect_identical(
            object = unfactorize(as.factor(vec)),
            expected = vec
        )
    }
})

test_that("data.frame", {
})

test_that("DFrame", {
})
