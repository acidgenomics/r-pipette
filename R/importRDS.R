## Updated 2019-07-19.
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
