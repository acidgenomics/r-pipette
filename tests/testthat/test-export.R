context("export : DataFrame")

test_that("`file` argument", {
    x <- export(df, file = "df.csv")
    expect_identical(x, realpath("df.csv"))
    expect_true(file.exists("df.csv"))
    file.remove("df.csv")
})

test_that("`format` argument", {
    x <- export(df, format = "csv")
    expect_identical(x, realpath("df.csv"))
    expect_true(file.exists("df.csv"))
    file.remove("df.csv")
})

test_that("Check for error on both `file` and `format` argument.", {
    expect_error(
        object = export(df, file = "df.csv", format = "csv"),
        regexp = "Specify `file` or `format` but not both."
    )
})



context("export : SummarizedExperiment")

test_that("`dir` argument, no `name`", {
    out <- export(rse, name = NULL, dir = "XXX")
    expect_identical(
        out,
        list(
            assays = list(
                counts = realpath(
                    file.path("XXX", "rse", "assays", "counts.csv")
                )
            ),
            colData = realpath(
                file.path("XXX", "rse", "colData.csv")
            ),
            rowData = realpath(
                file.path("XXX", "rse", "rowData.csv")
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Both `name` and `dir` declared", {
    out <- export(rse, name = "test", dir = "XXX")
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



export("export : SingleCellExperiment")

test_that("`dir` argument, no `name`", {
    out <- export(sce, name = NULL, dir = "XXX")
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
