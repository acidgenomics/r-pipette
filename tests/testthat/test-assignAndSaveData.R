test_that("assignAndSaveData", {
    dir <- tempdir2()
    file <- file.path(dir, "example.rds")
    envir <- new.env()
    x <- 1L
    out <- assignAndSaveData(
        name = "example",
        object = x,
        envir = envir,
        dir = dir,
        ext = "rds"
    )
    expect_true(exists("example", envir = envir, inherits = FALSE))
    expect_true(file.exists(file))
    unlink2(dir)
})
