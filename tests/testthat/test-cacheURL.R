context("cacheURL")

test_that("URL into BiocFileCache", {
    url <- pasteURL(
        pipetteTestsURL,
        "biocfilecache-test.txt",
        protocol = "none"
    )
    file <- cacheURL(url)
    expect_is(file, "character")
    expect_match(object = file, regexp = basename(url))
})
