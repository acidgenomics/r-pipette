import_using_rio <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using rio::import()."
    ))
    requireNamespace("rio", quietly = TRUE)
    rio::import(file, ...)
}
