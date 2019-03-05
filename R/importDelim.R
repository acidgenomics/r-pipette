importDelim <- function(file, ...) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using data.table::fread()."))
    object <- fread(file = file, na.strings = naStrings, ...)
    # Coerce data.table to data.frame.
    object <- as.data.frame(object)
    object <- .slotVersion(object, pkg = "data.table")
    object
}
