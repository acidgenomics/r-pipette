context("coerce : tibble")

test_that("DataFrame", {
    from <- df
    to <- as_tibble(from)
    expect_is(to, "tbl_df")
    to <- as(from, "tbl_df")
    expect_is(to, "tbl_df")
    ## Expect that rownames are automatically moved to first column.
    expect_identical(
        object = colnames(to)[[1L]],
        expected = "rowname"
    )
    ## Early return if already tibble.
    from <- tibble()
    to <- as(from, "tbl_df")
    expect_identical(from, to)
    ## Coercion of a DataFrame containing a list column is allowed.
    from <- DataFrame("x" = I(list()))
    to <- as(from, "tbl_df")
    expect_is(to, "tbl_df")
    ## Check handling when rownames are NULL.
    data <- DataFrame("a" = 1L, "b" = "b")
    expect_null(rownames(data))
    data <- as(data, "tbl_df")
    expect_is(data, "tbl_df")
    ## Note that tibble doesn't support row names, but they still return like
    ## standard data.frame class, where you can't actually set NULL.
    expect_identical(rownames(data), "1")
})

test_that("Ranges", {
    for (object in list(gr, ir)) {
        expect_is(as_tibble(object), "tbl_df")
        expect_is(as(object, "tbl_df"), "tbl_df")
        expect_true(isSubset(
            x = "rowname",
            y = colnames(as_tibble(object))
        ))
        expect_false(isSubset(
            x = "rowname",
            y = colnames(as_tibble(object, rownames = NULL))
        ))
        ## Kill names and cover automatic rowname handling.
        expect_false(isSubset(
            x = "rowname",
            y = colnames(as_tibble(unname(object)))
        ))
    }
})
