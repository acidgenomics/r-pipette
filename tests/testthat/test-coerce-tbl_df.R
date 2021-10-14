context("coerce : tbl_df")

test_that("S4 'as()' on empty data.frame", {
    expect_is(as(data.frame(), "tbl_df"), "tbl_df")
})

test_that("DataFrame", {
    data <- DFrame  # FIXME
    x <- as(data, "tbl_df")
    expect_is(x, "tbl_df")
    ## Expect that rownames are automatically moved to first column.
    expect_identical(colnames(x)[[1L]], "rowname")
    ## Early return if already tibble.
    x <- tibble()
    expect_identical(x, as(x, "tbl_df"))
    ## Coercion of a DataFrame containing a list column is allowed.
    data <- DataFrame()
    data[["x"]] <- list()
    data <- as(data, "tbl_df")
    expect_is(data, "tbl_df")
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
