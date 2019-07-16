context("export : matrix")

ext <- eval(formals(export.matrix)[["ext"]])
with_parameters_test_that(
    "`ext` argument", {
        file <- paste0("mat", ".", ext)

        x <- export(object = mat, ext = ext)
        expect_identical(x, realpath(file))
        expect_true(file.exists(file))
        # Check that row names stay intact.
        expect_true(grepl(
            pattern = "rowname",
            x = head(readLines(file), n = 1L)
        ))

        # Check accidental overwrite support.
        expect_error(
            export(mat, overwrite = FALSE),
            "File exists"
        )

        # Now strip the names, and confirm that export still works.
        mat <- unname(mat)
        x <- export(object = mat, ext = ext)
        expect_identical(x, realpath(file))
        expect_true(file.exists(file))
        expect_true(grepl(
            pattern = "V1",
            x = head(readLines(file), n = 1L)
        ))

        file.remove(file)
    },
    ext = ext
)



context("export : DataFrame")

ext <- eval(formals(export.matrix)[["ext"]])
with_parameters_test_that(
    "`ext` argument", {
        file <- paste0("df", ".", ext)
        x <- export(df, ext = ext)
        expect_identical(x, realpath(file))
        expect_true(file.exists(file))
        # Check that row names stay intact.
        expect_true(grepl(
            pattern = "rowname",
            x = head(readLines(file), n = 1L)
        ))
        file.remove(file)
    },
    ext = ext
)

test_that("`file` argument", {
    x <- export(df, file = "df.csv")
    expect_identical(x, realpath("df.csv"))
    expect_true(file.exists("df.csv"))
    file.remove("df.csv")
})



context("export : sparseMatrix")

test_that("`ext` argument, using gzip compression (default)", {
    x <- export(sparse, ext = "mtx.gz")
    expect_identical(
        x,
        c(
            matrix = realpath("sparse.mtx.gz"),
            barcodes = realpath("sparse.mtx.gz.colnames"),
            genes = realpath("sparse.mtx.gz.rownames")
        )
    )
    expect_true(all(file.exists(x)))

    # Check accidental overwrite support.
    expect_error(
        export(sparse, ext = "mtx.gz", overwrite = FALSE),
        "File exists"
    )

    file.remove(x)
})

test_that("`file` argument", {
    x <- export(sparse, file = "sparse.mtx")
    expect_identical(
        x,
        c(
            matrix = realpath("sparse.mtx"),
            barcodes = realpath("sparse.mtx.colnames"),
            genes = realpath("sparse.mtx.rownames")
        )
    )
    expect_true(all(file.exists(x)))
    file.remove(x)
})



context("export : SummarizedExperiment")

test_that("`dir` argument, no `name`", {
    out <- export(rse, name = NULL, dir = "XXX", compress = TRUE)
    expect_identical(
        out,
        list(
            assays = list(
                counts = realpath(
                    file.path("XXX", "rse", "assays", "counts.csv.gz")
                )
            ),
            colData = realpath(
                file.path("XXX", "rse", "colData.csv.gz")
            ),
            rowData = realpath(
                file.path("XXX", "rse", "rowData.csv.gz")
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Both `name` and `dir` declared", {
    out <- export(rse, name = "test", dir = "XXX", compress = FALSE)
    expect_identical(
        out,
        list(
            assays = list(
                counts = realpath(
                    file.path("XXX", "test", "assays", "counts.csv")
                )
            ),
            colData = realpath(
                file.path("XXX", "test", "colData.csv")
            ),
            rowData = realpath(
                file.path("XXX", "test", "rowData.csv")
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Unnamed primary assay", {
    se <- as(rse, "SummarizedExperiment")
    # Note that `assayNames()` assignment doesn't work here.
    names(assays(se)) <- NULL
    expect_null(assayNames(se))
    x <- export(se, dir = "XXX")
    expect_identical(names(x[["assays"]]), "assay")
    unlink("XXX", recursive = TRUE)
})



context("export : SingleCellExperiment")

test_that("`dir` argument, no `name`", {
    out <- export(sce, name = NULL, dir = "XXX", compress = FALSE)
    expect_identical(
        out,
        list(
            assays = list(
                counts = c(
                    matrix = realpath(
                        file.path("XXX", "sce", "assays", "counts.mtx")
                    ),
                    barcodes = realpath(
                        file.path("XXX", "sce", "assays", "counts.mtx.colnames")
                    ),
                    genes = realpath(
                        file.path("XXX", "sce", "assays", "counts.mtx.rownames")
                    )
                )
            ),
            colData = realpath(
                file.path("XXX", "sce", "colData.csv")
            ),
            rowData = realpath(
                file.path("XXX", "sce", "rowData.csv")
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Both `name` and `dir` declared", {
    out <- export(sce, name = "test", dir = "XXX")
    expect_identical(
        out,
        list(
            assays = list(
                counts = c(
                    matrix = realpath(
                        file.path("XXX", "test", "assays", "counts.mtx")
                    ),
                    barcodes = realpath(
                        file.path(
                            "XXX", "test", "assays", "counts.mtx.colnames"
                        )
                    ),
                    genes = realpath(
                        file.path(
                            "XXX", "test", "assays", "counts.mtx.rownames"
                        )
                    )
                )
            ),
            colData = realpath(
                file.path("XXX", "test", "colData.csv")
            ),
            rowData = realpath(
                file.path("XXX", "test", "rowData.csv")
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})
