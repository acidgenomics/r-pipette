context("compress")

text <- c("hello", "world")
file <- "test.txt"
writeLines(text = text, con = file)

exts <- eval(formals(compress)[["ext"]])
test_that(toString(exts), {
    for (ext in exts) {
        ## Compress the file.
        zfile <- compress(
            file = file,
            ext = ext,
            remove = TRUE,
            overwrite = TRUE
        )
        expect_true(file.exists(zfile))
        expect_false(file.exists(file))
        ## Decompress the compressed file.
        file <- decompress(file = zfile, remove = TRUE, overwrite = TRUE)
        expect_true(file.exists(file))
        expect_false(file.exists(zfile))
        expect_identical(
            object = readLines(con = file),
            expected = text
        )
    }
})

unlink(file)
