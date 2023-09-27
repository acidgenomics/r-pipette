## These tests can be a bit slow, since some of the files are large.
## But we want to make sure they work for important genomics files.

test_that("URL into BiocFileCache", {
    url <- pasteUrl(
        pipetteTestsUrl,
        "biocfilecache-test.txt",
        protocol = "none"
    )
    file <- cacheUrl(url)
    expect_type(file, "character")
    expect_match(object = file, regexp = basename(url))
})

test_that("URLs with the same base name", {
    ## SHA256: 9d77df788bc30aa86d6aa8d4b8d5bbe9d9c2a0ae6c0a8eda600524685aa11875
    url1 <- pasteUrl(
        "ftp.ensembl.org",
        "pub",
        "release-87",
        "fasta",
        "homo_sapiens",
        "cdna",
        "Homo_sapiens.GRCh38.cdna.all.fa.gz",
        protocol = "ftp"
    )
    ## > lines <- import(con = url1, format = "lines")
    ## > expect_identical(
    ## >     object = length(lines),
    ## >     expected = 5063317L
    ## > )
    ## SHA256: 0adbf00421ad63b7139481cefe03d9e3d226945696b17e48a4396c5d663ee7b0
    url2 <- pasteUrl(
        "ftp.ensembl.org",
        "pub",
        "release-102",
        "fasta",
        "homo_sapiens",
        "cdna",
        "Homo_sapiens.GRCh38.cdna.all.fa.gz",
        protocol = "ftp"
    )
    ## > lines <- import(con = url2, format = "lines")
    ## > expect_identical(
    ## >     object = length(lines),
    ## >     expected = 5883368L
    ## > )
    cacheFile1 <- cacheUrl(url1)
    cacheFile2 <- cacheUrl(url2)
    expect_true(file.exists(cacheFile1))
    expect_true(file.exists(cacheFile2))
    expect_false(identical(cacheFile1, cacheFile2))
})
