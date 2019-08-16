#' @describeIn import Import source code lines.
#' @export
importLines <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "readr::read_lines"
    ))
    object <- read_lines(file = file)
    object <- .slotMetadata(object, pkg = "readr", fun = "read_lines")
    object
}
