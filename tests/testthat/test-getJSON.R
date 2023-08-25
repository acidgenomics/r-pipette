test_that("UCSC Genome Browser API", {
    url <- pasteURL(
        "api.genome.ucsc.edu",
        "list",
        "ucscGenomes",
        protocol = "https"
    )
    skip_if_not_true(isAnExistingURL(url))
    json <- getJSON(url)
    expect_type(json, "list")
})

## The Ensembl REST API server is prone to connection issues, so disabling this
## at the moment to avoid build checks hanging.

## > test_that("Ensembl REST API", {
## >     url <- pasteURL(
## >         "rest.ensembl.org",
## >         "info",
## >         "assembly",
## >         paste0(
## >             "Homo sapiens",
## >             "?", "content-type=application/json"
## >         ),
## >         protocol = "https"
## >     )
## >     skip_if_not_true(isAnExistingURL(url))
## >     json <- getJSON(url)
## >     expect_type(json, "list")
## > })
