for (engine in .engines) {
    test_that(
        desc = paste("'append' argument", engine, sep = " : "),
        code = {
            con <- file.path(
                tempdir,
                "export",
                paste(engine, "lines.txt", sep = "-")
            )
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
                    .unlink(con)
                }
            )
        }
    )
    ## FIXME This step is failing for readr engine on Windows.
    ## FIXME Hits a permission issue regarding being unable to write file...
    ## FIXME Think this has to do with file.path and normalizePath winslash
    ## consistency issues...so annoying.
    for (format in .exportFormatChoices[["character"]]) {
        test_that(
            desc = paste("'format' argument", format, engine, sep = " : "),
            code = {
                testdir <- file.path(tempdir, "export")
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
                .unlink(testdir)
            }
        )
    }
}

objects <- list(
    "DataFrame" = df,
    "matrix" = mat
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
                    testdir <- file.path(tempdir, "export")
                    object <- objects[[class]]
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
                    .unlink(testdir)
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

for (engine in .engines) {
    test_that(
        desc = paste("Column names disabled", engine, sep = " : "),
        code = {
            object <- DataFrame(
                "txId" = c("tx0001", "tx0002", "tx0003", "tx0004"),
                "geneId" = c("gene0001", "gene0001", "gene0002", "gene0002")
            )
            con <- file.path(tempdir, "export", "tx2gene.csv")
            x <- export(
                object = object,
                con = con,
                colnames = FALSE,
                engine = engine
            )
            header <- import(
                con = x,
                format = "lines",
                nMax = 1L,
                engine = engine
            )
            expect_identical(
                object = header,
                expected = "\"tx0001\",\"gene0001\""
            )
            .unlink(con)
        }
    )
}

test_that("Deprecated 'ext' argument", {
    object <- df
    expect_s4_class(object, "DataFrame")
    testdir <- file.path(tempdir, "export")
    x <- export(
        object = object,
        ext = "csv",
        dir = testdir
    )
    expect_true(file.exists(x))
    .unlink(testdir)
})

test_that("Deprecated 'file' argument", {
    object <- df
    expect_s4_class(object, "DataFrame")
    file <- file.path(tempdir, "export", "test.csv")
    x <- export(object = object, file = file)
    expect_true(file.exists(x))
    .unlink(file)
})

for (format in .exportFormatChoices[["Matrix"]]) {
    test_that(
        desc = paste("'format' argument", format, sep = " : "),
        code = {
            object <- sparse
            expect_s4_class(object, "sparseMatrix")
            testdir <- file.path(tempdir, "export")
            x <- export(
                object = object,
                format = format,
                dir = testdir
            )
            expect_identical(
                object = x,
                expected = c(
                    "matrix" = realpath(file.path(
                        testdir,
                        paste0("object", ".", format)
                    )),
                    "barcodes" = realpath(file.path(
                        testdir,
                        paste0("object", ".", format, ".", "colnames")
                    )),
                    "genes" = realpath(file.path(
                        testdir,
                        paste0("object", ".", format, ".", "rownames")
                    ))
                )
            )
            expect_true(all(file.exists(x)))
            ## Check accidental overwrite support.
            expect_error(
                object = export(
                    object = object,
                    format = format,
                    dir = testdir,
                    overwrite = FALSE
                ),
                regexp = "File exists"
            )
            expect_message(
                object = export(
                    object = object,
                    format = format,
                    dir = testdir,
                    overwrite = TRUE
                ),
                regexp = "Overwriting"
            )
            .unlink(testdir)
        }
    )
}

test_that("Deprecated 'file' argument", {
    object <- sparse
    expect_s4_class(object, "sparseMatrix")
    testdir <- file.path(tempdir, "export")
    x <- export(
        object = object,
        file = file.path(testdir, "sparse.mtx")
    )
    expect_identical(
        object = x,
        expected = c(
            "matrix" = realpath(file.path(testdir, "sparse.mtx")),
            "barcodes" = realpath(file.path(testdir, "sparse.mtx.colnames")),
            "genes" = realpath(file.path(testdir, "sparse.mtx.rownames"))
        )
    )
    expect_true(all(file.exists(x)))
    .unlink(testdir)
})

test_that("Invalid input", {
    object <- sparse
    expect_error(
        object = export(object = unname(object)),
        regexp = "symbol"
    )
})
