test_that("UCSC Genome Browser API", {
    url <- pasteURL(
        "api.genome.ucsc.edu",
        "list",
        "ucscGenomes",
        protocol = "https"
    )
    json <- getJSON(url)
    expect_type(json, "list")
})

test_that("Ensembl REST API", {
    url <- pasteURL(
        "rest.ensembl.org",
        "info",
        "assembly",
        paste0(
            "Homo sapiens",
            "?", "content-type=application/json"
        ),
        protocol = "https"
    )
    json <- getJSON(url)
    expect_type(json, "list")
})
