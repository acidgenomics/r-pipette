importRDA <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using base::load()."
    ))
    safe <- new.env()
    object <- load(file, envir = safe, ...)
    if (length(safe) != 1L) {
        stop("File does not contain a single object.")
    }
    get(object, envir = safe, inherits = FALSE)
}
