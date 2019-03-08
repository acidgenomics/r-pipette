# Import GraphPad Prism files directly into R.
importPZFX <- function(file, sheet = 1L) {
    message(paste(
        "GraphPad Prism support is experimental.",
        "Consider exporting results to CSV format instead.",
        sep = "\n"
    ))
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using pzfx::read_pzfx()."))
    requireNamespace("pzfx", quietly = TRUE)
    object <- pzfx::read_pzfx(path = file, table = sheet)
    object <- .slotMetadata(object, pkg = "pzfx", fun = "read_pzfx")
    object
}
