#' @describeIn import Import an R data file.
#' @export
importRDA <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "base::load"
    ))
    safe <- new.env()
    object <- load(file, envir = safe)
    if (length(safe) != 1L) {
        stop("File does not contain a single object.")
    }
    object <- get(object, envir = safe, inherits = FALSE)
    object
}
