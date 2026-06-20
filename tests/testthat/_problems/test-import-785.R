# Extracted from test-import.R:785

# prequel ----------------------------------------------------------------------
for (engine in engines) {
    test_that(
        desc = paste("R script", engine, sep = " : "),
        code = {
            con <- file.path(cacheDir, "example.R")
            object <- import(
                con = con,
                engine = engine
            )
            expect_type(object, "character")
        }
    )
    test_that(
        desc = paste("Empty file", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            file.create(con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    engine = engine
                ),
                expected = character(0L)
            )
            unlink2(con)
        }
    )
    test_that(
        desc = paste("'comment' argument", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            vec <- c(
                "# comment 1",
                "aaa",
                "## comment 2",
                "bbb",
                "# comment 3",
                "ccc"
            )
            writeLines(text = vec, con = con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    comment = "",
                    engine = engine
                ),
                expected = vec
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    comment = "#",
                    engine = engine
                ),
                expected = c(
                    "aaa",
                    "bbb",
                    "ccc"
                )
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    comment = "# ",
                    engine = engine
                ),
                expected = c(
                    "aaa",
                    "## comment 2",
                    "bbb",
                    "ccc"
                )
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    comment = "##",
                    engine = engine
                ),
                expected = c(
                    "# comment 1",
                    "aaa",
                    "bbb",
                    "# comment 3",
                    "ccc"
                )
            )
            unlink2(con)
        }
    )
    test_that(
        desc = paste("'nMax' argument", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            vec <- c("aaa", "bbb", "ccc")
            writeLines(text = vec, con = con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    nMax = Inf,
                    engine = engine
                ),
                expected = vec
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    nMax = 2L,
                    engine = engine
                ),
                expected = vec[seq_len(2L)]
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    nMax = 1L,
                    engine = engine
                ),
                expected = vec[[1L]]
            )
            unlink2(con)
        }
    )
    test_that(
        desc = paste("'removeBlank' argument", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            vec <- c(
                "  aaa",
                "bbb  ",
                "   ",
                ""
            )
            writeLines(text = vec, con = con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    removeBlank = FALSE,
                    engine = engine
                ),
                expected = vec
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    removeBlank = TRUE,
                    stripWhitespace = FALSE,
                    engine = engine
                ),
                expected = c(
                    "  aaa",
                    "bbb  ",
                    "   "
                )
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    removeBlank = TRUE,
                    stripWhitespace = TRUE,
                    engine = engine
                ),
                expected = c(
                    "aaa",
                    "bbb"
                )
            )
            unlink2(con)
        }
    )
    test_that(
        desc = paste("'skip' argument", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            vec <- c("aaa", "bbb", "ccc", "ddd")
            writeLines(text = vec, con = con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    skip = 0L,
                    engine = engine
                ),
                expected = vec
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    skip = length(vec) - 1L,
                    engine = engine
                ),
                expected = vec[[length(vec)]]
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    skip = 2L,
                    nMax = 1L,
                    engine = engine
                ),
                expected = vec[[3L]]
            )
            expect_error(
                object = import(
                    con = con,
                    format = "lines",
                    skip = 1L,
                    comment = "#",
                    engine = engine
                ),
                regexp = "comment"
            )
            expect_error(
                object = import(
                    con = con,
                    format = "lines",
                    skip = 1L,
                    removeBlank = TRUE,
                    engine = engine
                ),
                regexp = "removeBlank"
            )
            unlink2(con)
        }
    )
    test_that(desc = "'stripWhitespace' argument", code = {
        con <- file.path(tempdir2(), "lines.txt")
        vec <- c(
            "  aaa",
            "bbb  ",
            " ccc ",
            "  ddd  ",
            "eee",
            "   "
        )
        writeLines(text = vec, con = con)
        expect_identical(
            object = import(
                con = con,
                format = "lines",
                stripWhitespace = FALSE,
                engine = engine
            ),
            expected = vec
        )
        expect_identical(
            object = import(
                con = con,
                format = "lines",
                stripWhitespace = TRUE,
                engine = engine
            ),
            expected = c(
                "aaa",
                "bbb",
                "ccc",
                "ddd",
                "eee",
                ""
            )
        )
        unlink2(con)
    })
}
for (engine in engines) {
    for (ext in c("csv", "csv.gz", "tsv")) {
        test_that(
            desc = paste(ext, engine, sep = " : "),
            code = {
                con <- file.path(cacheDir, paste0("example.", ext))
                object <- import(
                    con = con,
                    engine = engine,
                    metadata = TRUE
                )
                expect_s3_class(object, "data.frame")
                expect_true(hasRownames(object))
                expect_type(
                    object = attributes(object)[["import"]][["file"]],
                    type = "character"
                )
                expect_match(
                    object = attributes(object)[["import"]][["importerName"]],
                    regexp = engine
                )
                object <- import(
                    con = con,
                    engine = engine,
                    rownameCol = "rowname"
                )
                expect_true(hasRownames(object))
            }
        )
    }
}

# test -------------------------------------------------------------------------
file <- file.path(cacheDir, "example.maf")
object <- import(file)
