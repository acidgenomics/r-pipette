context("export : character")

test_that("'ext' argument", {
    vec <- c("hello", "world")
    formats <- .exportFormatChoices[["character"]]
    for (ext in formats) {
        file <- paste0("vec", ".", ext)
        x <- export(object = vec, ext = ext)
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

test_that("'engine' argument", {
    vec <- c("hello", "world")
    file <- file.path(tempdir(), "vec.txt.gz")
    for (engine in c(
        "base",
        "data.table",
        "readr",
        "vroom"
    )) {
        ## FIXME This check is now failing.
        x <- export(
            object = vec,
            file = file,
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

test_that("'append' argument", {
    file <- file.path(tempdir(), "lines.txt")
    unlink(file, recursive = FALSE)
    vec1 <- c("aaa", "bbb")
    vec2 <- c("ccc", "ddd")
    engine <- "data.table"
    export(
        object = vec1,
        file = file,
        engine = engine
    )
    expect_identical(
        object = import(file, format = "lines"),
        expected = vec1
    )
    export(
        object = vec2,
        file = file,
        append = TRUE,
        engine = engine
    )
    expect_identical(
        object = import(file, format = "lines"),
        expected = c(vec1, vec2)
    )
    expect_error(
        object = export(
            object = vec2,
            file = file,
            append = TRUE,
            engine = "base"
        ),
        regexp = "base"
    )
    unlink(file, recursive = FALSE)
})



context("export : matrix")

test_that("'ext' argument", {
    formats <- .exportFormatChoices[["matrix"]]
    for (ext in formats) {
        mat1 <- mat
        file1 <- paste0("mat1", ".", ext)
        x <- export(object = mat1, ext = ext)
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

test_that("acid.export.engine override", {
    for (engine in c("base", "data.table", "readr", "vroom")) {
        options("acid.export.engine" = engine)
        file <- export(object = mat, ext = "csv")
        expect_true(file.exists(file))
        expect_identical(basename(file), "mat.csv")
        unlink(file)
    }
    options("acid.export.engine" = NULL)
})

test_that("Invalid input", {
    expect_error(
        export(object = unname(mat)),
        "symbol"
    )
})



context("export : DataFrame")

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

test_that("'file' argument", {
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

test_that("'ext' argument, using gzip compression", {
    ## FIXME This step is erroring.
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

test_that("'file' argument", {
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
