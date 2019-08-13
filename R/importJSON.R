## Updated 2019-07-19.
importJSON <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "jsonlite::read_json"
    ))
    requireNamespace("jsonlite", quietly = TRUE)
    object <- jsonlite::read_json(path = file)
    object <- .slotMetadata(object, pkg = "jsonlite", fun = "read_json")
    object
}
