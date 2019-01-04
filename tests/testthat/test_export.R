context("Export")

data(rse, sce, package = "basejump", envir = environment())

mat <- assay(rse)
sparse <- assay(sce)
url <- basejumpCacheURL



# assignAndSaveData ============================================================
test_that("assignAndSaveData", {
    object <- suppressMessages(assignAndSaveData(
        name = "XXX",
        object = rse,
        dir = ".",
        ext = "rds"
    ))
    expect_identical(
        object = object,
        expected = c(XXX = file.path(getwd(), "XXX.rds"))
    )
    unlink("XXX.rds")
})



# saveData =====================================================================
test_that("saveData", {
    dir <- "example"
    paths <- file.path(
        getwd(),
        "example",
        c("rse.rda", "sce.rda")
    )
    names(paths) <- c("rse", "sce")

    # R data.
    object <- saveData(
        rse, sce,
        ext = "rda",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(object, paths)

    # R data serialized.
    object <- saveData(
        rse, sce,
        ext = "rds",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(
        object = basename(object),
        expected = c("rse.rds", "sce.rds")
    )

    # Check `overwrite = FALSE` mode.
    expect_warning(
        object = saveData(
            rse, sce,
            dir = dir, overwrite = FALSE
        ),
        regexp = "No files were saved."
    )

    unlink(dir, recursive = TRUE)
})

test_that("saveData : Invalid parameters", {
    expect_error(
        object = saveData(XXX),
        regexp = "object 'XXX' not found"
    )
    expect_error(
        object = saveData("example"),
        regexp = "non-standard evaluation"
    )
    expect_error(
        object = saveData(rse, dir = NULL),
        regexp = "isString"
    )
})



# writeCounts ==================================================================
test_that("writeCounts", {
    dir <- "example"
    expect_message(
        object = writeCounts(mat, sparse, dir = dir, compress = TRUE),
        regexp = "Writing mat, sparse"
    )
    expect_identical(
        object = list.files(dir),
        expected = c(
            "mat.csv.gz",
            "sparse.mtx.gz",
            "sparse.mtx.gz.colnames",
            "sparse.mtx.gz.rownames"
        )
    )
    # Require a matrix, and don't allow data frames.
    expect_error(
        object = writeCounts(mtcars),
        regexp = "mtcars is not a matrix"
    )
    # Check that `eval_bare` call errors on missing object.
    expect_error(
        object = writeCounts(XXX),
        regexp = "object 'XXX' not found"
    )
    unlink(dir, recursive = TRUE)
})
