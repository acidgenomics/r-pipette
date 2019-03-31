test_that("basenameSansExt", {
    path <- c("dir/foo.txt", "dir/bar.tar.gz", "dir/")
    expect_identical(
        basenameSansExt(path),
        c("foo", "bar", NA_character_)
    )
})
