# Import GraphPad Prism files directly into R.
importPZFX <- function(file, ...) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using pzfx::read_pzfx()."))
    requireNamespace("pzfx", quietly = TRUE)
    object <- pzfx::read_pzfx(path = file, ...)
    object <- .slotMetadata(object, pkg = "pzfx", fun = "read_pzfx")
    object
}
