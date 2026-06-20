# Extracted from test-saveData.R:32

# prequel ----------------------------------------------------------------------
object1 <- "aaa"
object2 <- "bbb"
dir <- tempdir2()
paths <- file.path(dir, c("object1.rds", "object2.rds"))
names(paths) <- c("object1", "object2")

# test -------------------------------------------------------------------------
object <- saveData(
        object1,
        object2,
        ext = "rda",
        dir = dir,
        overwrite = TRUE
    )
paths <- gsub(
        pattern = "\\.rds",
        replacement = ".rda",
        x = paths,
        fixed = TRUE
    )
expect_identical(object, paths)
