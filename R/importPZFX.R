# Import GraphPad Prism files directly into R!
importPZFX <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using pzfx::read_pzfx()."
    ))
    requireNamespace("pzfx", quietly = TRUE)
    pzfx::read_pzfx(path = file, ...)
}
