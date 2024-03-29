test_that("metadata2", {
    for (x in list(
        "DFrame" = DataFrame(),
        "data.frame" = data.frame()
    )) {
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
