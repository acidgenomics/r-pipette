context("cacheURL")

test_that("URL into BiocFileCache", {
    url <- pasteURL(
        pipetteTestsURL,
        "biocfilecache-test.txt",
        protocol = "none"
    )
    file <- cacheURL(url)
    print(file)
})
