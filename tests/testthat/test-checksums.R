test_that("checksums", {
    file <- file.path(cacheDir, "example.rds")
    for (fun in list(md5, sha256)) {
        x <- fun(file)
        expect_type(x, "character")
    }
})
