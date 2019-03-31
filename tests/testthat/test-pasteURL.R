context("pasteURL")

test_that("HTTPS", {
    expect_identical(
        object = pasteURL(
            "steinbaugh.com",
            "basejump",
            "reference",
            protocol = "https"
        ),
        expected = "https://steinbaugh.com/basejump/reference"
    )
})

test_that("FTP", {
    expect_identical(
        object = pasteURL(
            "ftp.ensembl.org",
            "pub",
            "release-94",
            "gtf",
            "homo_sapiens",
            "Homo_sapiens.GRCh38.94.gtf.gz",
            protocol = "ftp"
        ),
        expected = paste(
            "ftp://ftp.ensembl.org/pub/release-94/gtf/homo_sapiens",
            "Homo_sapiens.GRCh38.94.gtf.gz",
            sep = "/"
        )
    )
})
