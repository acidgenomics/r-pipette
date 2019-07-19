# Import GraphPad Prism files directly into R.
# Note that this function doesn't support optional column names.
# Updated 2019-07-19.
importPZFX <- function(file, sheet = 1L) {
    message(paste(
        "GraphPad Prism support is experimental.",
        "Consider exporting results to CSV format instead.",
        sep = "\n"
    ))
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using pzfx::read_pzfx()."))
    requireNamespace("pzfx", quietly = TRUE)
    object <- pzfx::read_pzfx(
        path = file,
        table = sheet
    )
    object <- .slotMetadata(object, pkg = "pzfx", fun = "read_pzfx")
    object
}
