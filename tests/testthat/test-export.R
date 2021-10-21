context("export : character")

for (engine in .engines) {
    test_that(
        desc = paste("'append' argument", engine, sep = " : "),
        code = {
            con <- file.path(tempdir(), "lines.txt")
            unlink(con, recursive = FALSE)
            object1 <- c("aaa", "bbb")
            object2 <- c("ccc", "ddd")
            switch(
                EXPR = engine,
                "base" = {
                    expect_error(
                        object = export(
                            object = object1,
                            con = con,
                            append = TRUE,
                            engine = engine
                        ),
                        regexp = "base"
                    )
                },
                {
                    x <- export(
                        object = object1,
                        con = con,
                        append = FALSE,
                        engine = engine
                    )
                    expect_identical(x, realpath(con))
                    expect_true(file.exists(con))
                    expect_identical(
                        object = import(
                            con = con,
                            format = "lines",
                            engine = engine
                        ),
                        expected = object1
                    )
                    x <- export(
                        object = object2,
                        con = con,
                        append = TRUE,
                        engine = engine
                    )
                    expect_identical(x, realpath(con))
                    expect_true(file.exists(con))
                    expect_identical(
                        object = import(
                            con = con,
                            format = "lines",
                            engine = engine
                        ),
                        expected = c(object1, object2)
                    )
                }
            )
            unlink(con, recursive = FALSE)
        }
    )
    for (format in .exportFormatChoices[["character"]]) {
        test_that(
            desc = paste("'format' argument", format, engine, sep = " : "),
            code = {
                testdir <- file.path(tempdir(), "export")
                unlink(testdir, recursive = TRUE)
                vec <- c("hello", "world")
                con <- file.path(testdir, paste0("vec", ".", format))
                x <- export(
                    object = vec,
                    format = format,
                    dir = testdir,
                    engine = engine
                )
                expect_identical(x, realpath(con))
                expect_true(file.exists(con))
                expect_identical(
                    object = import(
                        con = con,
                        format = "lines",
                        engine = engine
                    ),
                    expected = vec
                )
                expect_error(
                    export(
                        object = vec,
                        format = format,
                        dir = testdir,
                        overwrite = FALSE,
                        engine = engine
                    ),
                    "File exists"
                )
                expect_message(
                    export(
                        object = vec,
                        format = format,
                        dir = testdir,
                        overwrite = TRUE,
                        engine = engine
                    ),
                    "Overwriting"
                )
                unlink(testdir, recursive = TRUE)
            }
        )
    }
}



context("export : Delimited files")

objects <- list(
    "DataFrame" = df,
    "data.table" = dt,
    "matrix" = mat,
    "tbl_df" = tbl
)

## FIXME Need to cover the handling of `colnames = FALSE` here.

for (engine in .engines) {
    for (class in names(objects)) {
        for (format in .exportFormatChoices[["delim"]]) {
            test_that(
                desc = paste(
                    "'format' argument",
                    format, class, engine,
                    sep = " : "
                ),
                code = {
                    testdir <- file.path(tempdir(), "export")
                    unlink(testdir, recursive = TRUE)
                    object <- objects[[class]]
                    expect_is(object, class)
                    file <- export(
                        object = object,
                        format = format,
                        dir = testdir,
                        engine = engine
                    )
                    expect_true(file.exists(file))
                    expect_identical(
                        object = basename(file),
                        expected = paste0("object", ".", format)
                    )
                    expect_true(grepl(
                        pattern = "rowname",
                        x = head(import(
                            con = file,
                            format = "lines",
                            engine = engine
                        ), n = 1L)
                    ))
                    ## Check the overwrite support.
                    expect_error(
                        export(
                            object = object,
                            format = format,
                            dir = testdir,
                            overwrite = FALSE,
                            engine = engine
                        ),
                        "File exists"
                    )
                    expect_message(
                        export(
                            object = object,
                            format = format,
                            dir = testdir,
                            overwrite = TRUE,
                            engine = engine
                        ),
                        "Overwriting"
                    )
                    unlink(testdir, recursive = TRUE)






                    unlink(testdir, recursive = TRUE)
                    ## FIXME Move this out to a separate unit test.
                    ## Now strip the names, and confirm that export still works.
                    mat2 <- unname(mat1)
                    file2 <- paste0("mat2", ".", ext)
                    x <- export(object = mat2, ext = ext)
                    expect_identical(x, realpath(file2))
                    expect_true(file.exists(file2))
                    expect_true(grepl(
                        pattern = "V1",
                        x = head(import(file2, format = "lines"), n = 1L)
                    ))
                    file.remove(file1, file2)




                    unlink(testdir, recursive = TRUE)
                }
            )
        }
    }
}

## FIXME Need to parameterize this.
test_that("Deprecated 'file' argument", {
    x <- export(df, file = "df.csv")
    expect_identical(x, realpath("df.csv"))
    expect_true(file.exists("df.csv"))
    file.remove("df.csv")
})

test_that("Invalid input", {
    expect_error(
        export(object = unname(mat)),
        "symbol"
    )
})



context("export : sparseMatrix")

test_that("'ext' argument, using gzip compression", {
    ## FIXME Ensure we write into temporary directory.
    x <- export(
        object = sparse,
        ext = "mtx.gz"
    )
    expect_identical(
        object = x,
        expected = c(
            "matrix" = realpath("sparse.mtx.gz"),
            "barcodes" = realpath("sparse.mtx.gz.colnames"),
            "genes" = realpath("sparse.mtx.gz.rownames")
        )
    )
    expect_true(all(file.exists(x)))
    ## Check accidental overwrite support.
    expect_error(
        object = export(sparse, ext = "mtx.gz", overwrite = FALSE),
        regexp = "File exists"
    )
    expect_message(
        object = export(sparse, ext = "mtx.gz", overwrite = TRUE),
        regexp = "Overwriting"
    )
    file.remove(x)
})

test_that("Deprecated 'file' argument", {
    x <- export(sparse, file = "sparse.mtx")
    expect_identical(
        object = x,
        expected = c(
            "matrix" = realpath("sparse.mtx"),
            "barcodes" = realpath("sparse.mtx.colnames"),
            "genes" = realpath("sparse.mtx.rownames")
        )
    )
    expect_true(all(file.exists(x)))
    file.remove(x)
})

test_that("Invalid input", {
    expect_error(
        object = export(object = unname(sparse)),
        regexp = "symbol"
    )
})
