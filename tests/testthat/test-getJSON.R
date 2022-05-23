test_that("UCSC Genome Browser API", {
    url <- "https://api.genome.ucsc.edu/list/ucscGenomes"
    json <- getJSON(url)
    expect_type(json, "list")
})
