#' @describeIn import Import an R data serialized file.
#' @export
importRDS <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file),
        "base::readRDS"
    ))
    object <- readRDS(file)
    object
}
