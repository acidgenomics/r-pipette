test_that("list : empty list", {
    expect_identical(
        object = as.DataFrame(list()),
        expected = DataFrame()
    )
})

test_that("list : unnamed list", {
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

test_that("list : nested list", {
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
})

test_that("list : mismatched rownames", {
    lst <- list(
        "a" = c("x" = 1L, "y" = 2L, "z" = 3L),
        "b" = c("y" = 4L, "x" = 5L, "z" = 6L),
        "c" = c("z" = 7L, "x" = 8L, "y" = 9L)
    )
    object <- as.DataFrame(lst)
    expected <- DataFrame(
        "a" = c(1L, 2L, 3L),
        "b" = c(5L, 4L, 6L),
        "c" = c(8L, 9L, 7L),
        row.names = c("x", "y", "z")
    )
    expect_identical(object, expected)
})
