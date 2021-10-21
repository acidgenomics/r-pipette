context("export : character")

## FIXME data.table is not outputting correctly here, without quoting.
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
                    export(
                        object = object1,
                        con = con,
                        append = FALSE,
                        overwrite = TRUE,
                        engine = engine
                    )
                    expect_identical(
                        object = import(
                            con = con,
                            format = "lines",
                            engine = engine
                        ),
                        expected = object1
                    )
                    export(
                        object = object2,
                        con = con,
                        append = TRUE,
                        overwrite = FALSE,
                        engine = engine
                    )
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
}







## FIXME Need to ensure that deprecated "file" and "ext" coverage still works.
## FIXME Need to check support for all engines except base...



test_that("'engine' argument", {
    vec <- c("hello", "world")
    file <- file.path(tempdir(), "vec.txt.gz")
    for (engine in c(
        "base",
        "data.table",
        "readr"
    )) {
        x <- export(
            object = vec,
            con = file,
            engine = engine,
            overwrite = TRUE
        )
        expect_identical(x, realpath(file))
        expect_true(file.exists(file))
        expect_identical(
            object = import(file, format = "lines"),
            expected = vec
        )
    }
    file.remove(file)
})

test_that("'format' argument", {
    vec <- c("hello", "world")
    formats <- .exportFormatChoices[["character"]]
    for (format in formats) {
        file <- paste0("vec", ".", format)
        x <- export(object = vec, format = format)
        expect_identical(x, realpath(file))
        expect_true(file.exists(file))
        expect_identical(
            object = import(file, format = "lines"),
            expected = vec
        )
        ## Check accidental overwrite support.
        expect_error(
            export(vec, ext = ext, overwrite = FALSE),
            "File exists"
        )
        expect_message(
            export(vec, ext = ext, overwrite = TRUE),
            "Overwriting"
        )
        file.remove(file)
    }
})



context("export : delim (DataFrame, data.frame, matrix)")

## FIXME Need to migrate DataFrame duplicated checks here...

test_that("'engine' argument", {
    for (engine in c("base", "data.table", "readr")) {
        options("acid.export.engine" = engine)
        file <- export(object = mat, ext = "csv")
        expect_true(file.exists(file))
        expect_identical(basename(file), "mat.csv")
        unlink(file)
    }
    options("acid.export.engine" = NULL)
})

test_that("'format' argument", {
    formats <- .exportFormatChoices[["matrix"]]
    for (format in formats) {
        mat1 <- mat
        file1 <- paste0("mat1", ".", format)
        x <- export(object = mat1, format = format)
        expect_identical(x, realpath(file1))
        expect_true(file.exists(file1))
        expect_true(grepl(
            pattern = "rowname",
            x = head(import(file1, format = "lines"), n = 1L)
        ))
        ## Check accidental overwrite support.
        expect_error(
            export(mat1, ext = ext, overwrite = FALSE),
            "File exists"
        )
        expect_message(
            export(mat1, ext = ext, overwrite = TRUE),
            "Overwriting"
        )
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
    }
})

test_that("Invalid input", {
    expect_error(
        export(object = unname(mat)),
        "symbol"
    )
})



context("export : DataFrame")

## FIXME These seem a bit duplicated with matrix / data.frame approach above.

test_that("'ext' argument", {
    formats <- .exportFormatChoices[["matrix"]]
    for (ext in formats) {
        file <- paste0("df", ".", ext)
        x <- export(df, ext = ext)
        expect_identical(x, realpath(file))
        expect_true(file.exists(file))
        ## Check that row names stay intact.
        expect_true(grepl(
            pattern = "rowname",
            x = head(import(file, format = "lines"), n = 1L)
        ))
        file.remove(file)
    }
})

test_that("Deprecated 'file' argument", {
    x <- export(df, file = "df.csv")
    expect_identical(x, realpath("df.csv"))
    expect_true(file.exists("df.csv"))
    file.remove("df.csv")
})

test_that("Invalid input", {
    expect_error(
        object = export(object = as.data.frame(df)),
        regexp = "symbol"
    )
})



context("export : Matrix / sparseMatrix")

test_that("'ext' argument, using gzip compression", {
    x <- export(sparse, ext = "mtx.gz")
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
