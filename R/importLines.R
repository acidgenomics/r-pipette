#' @describeIn import Import source code lines.
#' @export
importLines <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "base::readLines"
    ))
    object <- readLines(con = file)
    object <- .slotMetadata(object, pkg = "base", fun = "readLines")
    object
}
