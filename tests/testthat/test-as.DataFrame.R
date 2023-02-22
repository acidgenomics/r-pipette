test_that("list", {
    expect_identical(
        object = as.DataFrame(list()),
        expected = DataFrame()
    )
    lst <- list(
        "a" = list(c(1L, 2L), c(3L, 4L)),
        "b" = list(NULL, NULL)
    )
    object <- as.DataFrame(lst)
    expected <- new(
        Class = "DFrame",
        listData = lst,
        nrows = length(lst[[1L]])
    )
    expect_identical(object, expected)
    lst <- list(character(), character())
    object <- as.DataFrame(lst)
    expected <- new(
        Class = "DFrame",
        listData = list(
            "X1" = character(),
            "X2" = character()
        ),
        nrows = length(lst[[1L]])
    )
    expect_identical(object, expected)
})
