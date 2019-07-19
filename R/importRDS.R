# Updated 2019-07-19.
importRDS <- function(file) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using base::readRDS()."))
    object <- readRDS(file)
    object
}
