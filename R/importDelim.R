importDelim <- function(file, ...) {
    file <- localOrRemoteFile(file)
    message(paste(
        "Importing", basename(file), "using data.table::fread()."
    ))
    data <- fread(file = file, na.strings = naStrings, ...)
    # Coerce data.table to data.frame.
    data <- as.data.frame(data)
}
