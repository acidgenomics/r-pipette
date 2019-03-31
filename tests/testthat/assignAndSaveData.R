context("assignAndSaveData")

test_that("assignAndSaveData", {
    expect_identical(
        object = assignAndSaveData(
            name = "XXX",
            object = rse,
            dir = ".",
            ext = "rds"
        ),
        expected = c(XXX = file.path(getwd(), "XXX.rds"))
    )
    unlink("XXX.rds")
})
