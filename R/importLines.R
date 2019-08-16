#' @describeIn import Import source code lines.
#' @export
importLines <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "base::readLines"
    ))
    con <- file(file)
    object <- readLines(con = con)
    close(con)
    object <- .slotMetadata(object, pkg = "base", fun = "readLines")
    object
}
