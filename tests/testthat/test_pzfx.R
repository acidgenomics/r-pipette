context("GraphPad Prism")

skip_if_not_installed(pkg = "pzfx")

file <- system.file("extdata", "exponential_decay.pzfx", package = "pzfx")
stopifnot(file.exists(file))

test_that("PZFX", {
    x <- import(file, sheet = 1L)
    colnames(x)
    expect_identical(
        colnames(x),
        c(
            "Minutes",
            "Control_1",
            "Control_2",
            "Control_3",
            "Treated_1",
            "Treated_2",
            "Treated_3"
        )
    )
})
