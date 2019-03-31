context("fileExt")

test_that("CSV file", {
    expect_identical(fileExt("XXX.csv"), "csv")
    expect_identical(fileExt("XXX.tsv.csv"), "csv")
})

test_that("Compression formats", {
    expect_identical(fileExt("XXX.tar.bz2"), "tar.bz2")
    expect_identical(fileExt("XXX.tar.gz"), "tar.gz")
    expect_identical(fileExt("XXX.tar.xz"), "tar.xz")
    expect_identical(fileExt("XXX.tar.gz"), "tar.gz")
})

test_that("No extension", {
    expect_identical(fileExt("XXX"), NA_character_)
})
