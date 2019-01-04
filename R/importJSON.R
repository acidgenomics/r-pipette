importJSON <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using jsonlite::read_json()."
    ))
    requireNamespace("jsonlite", quietly = TRUE)
    jsonlite::read_json(path = file, ...)
}
