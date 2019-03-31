context("assignAndSaveData")

test_that("assignAndSaveData", {
    envir <- new.env()
    x <- 1L
    out <- assignAndSaveData(
        name = "example",
        object = x,
        envir = envir,
        dir = ".",
        ext = "rds"
    )
    exists("example", envir = envir, inherits = FALSE)
    file.exists("example.rds")
    rm(example)
    unlink("example.rds")
})
