## Updated 2019-07-19.
importJSON <- function(file) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using jsonlite::read_json()."))
    requireNamespace("jsonlite", quietly = TRUE)
    object <- jsonlite::read_json(path = file)
    object <- .slotMetadata(object, pkg = "jsonlite", fun = "read_json")
    object
}
