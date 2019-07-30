context("export : matrix")

ext <- eval(formals(`export,matrix`)[["ext"]])

with_parameters_test_that(
    "`ext` argument", {
        file <- paste0("mat", ".", ext)

        x <- export(object = mat, ext = ext)
        expect_identical(x, realpath(file))
        expect_true(file.exists(file))
        ## Check that row names stay intact.
        expect_true(grepl(
            pattern = "rowname",
            x = head(readLines(file), n = 1L)
        ))

        ## Check accidental overwrite support.
        expect_error(
            export(mat, ext = ext, overwrite = FALSE),
            "File exists"
        )
        expect_message(
            export(mat, ext = ext, overwrite = TRUE),
            "Overwriting"
        )

        ## Now strip the names, and confirm that export still works.
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

test_that("Invalid input", {
    expect_error(
        export(object = unname(mat)),
        "symbol"
    )
})



context("export : DataFrame")

ext <- eval(formals(`export,DataFrame`)[["ext"]])

with_parameters_test_that(
    "`ext` argument", {
        file <- paste0("df", ".", ext)
        x <- export(df, ext = ext)
        expect_identical(x, realpath(file))
        expect_true(file.exists(file))
        ## Check that row names stay intact.
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

test_that("Invalid input", {
    ## Note that `unname()` usage will result in a DataFrame error.
    expect_error(
        export(object = as.data.frame(df)),
        "symbol"
    )
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

    ## Check accidental overwrite support.
    expect_error(
        export(sparse, ext = "mtx.gz", overwrite = FALSE),
        "File exists"
    )
    expect_message(
        export(sparse, ext = "mtx.gz", overwrite = TRUE),
        "Overwriting"
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

test_that("Invalid input", {
    expect_error(
        export(object = unname(sparse)),
        "symbol"
    )
})



context("export : SummarizedExperiment")

test_that("`dir` argument, no `name`", {
    out <- export(rse, name = NULL, dir = "XXX", compress = TRUE)
    prefix <- realpath(file.path("XXX", "rse"))
    expect_identical(
        out,
        list(
            assays = list(
                counts = file.path(prefix, "assays", "counts.csv.gz")
            ),
            colData = file.path(prefix, "colData.csv.gz"),
            rowData = file.path(prefix, "rowData.csv.gz")
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Both `name` and `dir` declared", {
    out <- export(rse, name = "test", dir = "XXX", compress = FALSE)
    prefix <- realpath(file.path("XXX", "test"))
    expect_identical(
        out,
        list(
            assays = list(
                counts = file.path(prefix, "assays", "counts.csv")
            ),
            colData = file.path(prefix, "colData.csv"),
            rowData = file.path(prefix, "rowData.csv")
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Unnamed primary assay", {
    se <- as(rse, "SummarizedExperiment")
    ## Note that `assayNames()` assignment doesn't work here.
    names(assays(se)) <- NULL
    expect_null(assayNames(se))
    x <- export(se, dir = "XXX")
    expect_identical(names(x[["assays"]]), "assay")
    unlink("XXX", recursive = TRUE)
})



context("export : SingleCellExperiment")

## Note that the SingleCellExperiment_Seurat object has reducedDims slotted,
## whereas the SingleCellExperiment (splatter) example doesn't.

test_that("`dir` argument, no `name`", {
    x <- export(sce_seurat, name = NULL, dir = "XXX", compress = TRUE)
    prefix <- realpath(file.path("XXX", "sce_seurat"))
    assays <- file.path(prefix, "assays")
    expect_identical(
        x,
        list(
            assays = list(
                counts = c(
                    matrix = file.path(assays, "counts.mtx.gz"),
                    barcodes = file.path(assays, "counts.mtx.gz.colnames"),
                    genes = file.path(assays, "counts.mtx.gz.rownames")
                ),
                logcounts = c(
                    matrix = file.path(assays, "logcounts.mtx.gz"),
                    barcodes = file.path(assays, "logcounts.mtx.gz.colnames"),
                    genes = file.path(assays, "logcounts.mtx.gz.rownames")
                )
            ),
            colData = file.path(prefix, "colData.csv.gz"),
            rowData = file.path(prefix, "rowData.csv.gz"),
            reducedDims = list(
                umap = file.path(prefix, "reducedDims", "umap.csv.gz")
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Both `name` and `dir` declared", {
    x <- export(sce_seurat, name = "test", dir = "XXX")
    prefix <- realpath(file.path("XXX", "test"))
    assays <- file.path(prefix, "assays")
    expect_identical(
        x,
        list(
            assays = list(
                counts = c(
                    matrix = file.path(assays, "counts.mtx"),
                    barcodes = file.path(assays, "counts.mtx.colnames"),
                    genes = file.path(assays, "counts.mtx.rownames")
                ),
                logcounts = c(
                    matrix = file.path(assays, "logcounts.mtx"),
                    barcodes = file.path(assays, "logcounts.mtx.colnames"),
                    genes = file.path(assays, "logcounts.mtx.rownames")
                )
            ),
            colData = file.path(prefix, "colData.csv"),
            rowData = file.path(prefix, "rowData.csv"),
            reducedDims = list(
                umap = file.path(prefix, "reducedDims", "umap.csv")
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})
