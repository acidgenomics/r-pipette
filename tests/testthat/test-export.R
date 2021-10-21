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

for (format in .exportFormatChoices[["delim"]]) {
    for (class in names(objects)) {
        for (engine in .engines) {
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
                    reimport <- import(file, engine = engine)
                    expect_true(hasRownames(reimport))
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
                }
            )
        }
        test_that(
            desc = paste(
                "Invalid input",
                format, class,
                sep = " : "
            ),
            code = {
                object <- objects[[class]]
                expect_error(
                    export(object = as.data.frame(object)),
                    "symbol"
                )
            }
        )
    }
}

test_that("Deprecated 'file' argument", {
    object <- df
    file <- file.path(tempdir(), "export", "test.csv")
    unlink(file, recursive = FALSE)
    x <- export(object = object, file = file)
    expect_true(file.exists(x))
    unlink(file, recursive = FALSE)
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
