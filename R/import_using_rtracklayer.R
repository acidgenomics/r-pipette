# Using tryCatch here to error if there are any warnings.
import_using_rtracklayer <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using rtracklayer::import()."
    ))
    requireNamespace("rtracklayer", quietly = TRUE)
    tryCatch(
        expr = rtracklayer::import(file, ...),
        error = function(e) {
            stop("File failed to load.")  # nocov
        },
        warning = function(w) {
            stop("File failed to load.")  # nocov
        }
    )
}
