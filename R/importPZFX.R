## Import GraphPad Prism files directly into R.
## Note that this function doesn't support optional column names.
## Updated 2019-07-19.
importPZFX <- function(file, sheet = 1L) {
    message(
        "GraphPad Prism support is experimental.\n",
        "Consider exporting results to CSV format instead."
    )
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "pzfx::read_pzfx"
    ))
    requireNamespace("pzfx", quietly = TRUE)
    object <- pzfx::read_pzfx(
        path = file,
        table = sheet
    )
    object <- .slotMetadata(object, pkg = "pzfx", fun = "read_pzfx")
    object
}
