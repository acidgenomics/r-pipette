test_that("CSV", {
    object <- c(
        "\"aaa\",\"bbb\",\"ccc\",\"ddd\"",
        "\"eee\",\"fff\",\"ggg\"",
        "\"hhh\",\"iii\""
    )
    object <- fillLines(object, format = "csv")
    expect_identical(
        object = object,
        expected = c(
            "\"aaa\",\"bbb\",\"ccc\",\"ddd\"",
            "\"eee\",\"fff\",\"ggg\",NA",
            "\"hhh\",\"iii\",NA,NA"
        )
    )
    con <- textConnection(object)
    expect_identical(
        object = import(con, format = "csv"),
        expected = data.frame(
            "aaa" = c("eee", "hhh"),
            "bbb" = c("fff", "iii"),
            "ccc" = c("ggg", NA_character_),
            "ddd" = rep(NA, 2L)
        )
    )
    close(con)
})

test_that("TSV", {
    object <- c(
        "aaa\tbbb\tccc\tddd",
        "eee\tfff\tggg",
        "hhh\tiii"
    )
    object <- fillLines(object, format = "tsv")
    expect_identical(
        object = object,
        expected = c(
            "aaa\tbbb\tccc\tddd",
            "eee\tfff\tggg\tNA",
            "hhh\tiii\tNA\tNA"
        )
    )
    con <- textConnection(object)
    expect_identical(
        object = import(con, format = "tsv"),
        expected = data.frame(
            "aaa" = c("eee", "hhh"),
            "bbb" = c("fff", "iii"),
            "ccc" = c("ggg", NA_character_),
            "ddd" = rep(NA, 2L)
        )
    )
    close(con)
})
