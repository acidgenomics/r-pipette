# Refer to `fread()` `colClasses` parameter for column class definition, which
# is intended for rare overrides and not routine use.

importDelim <- function(file) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using data.table::fread()."))
    object <- fread(
        file = file,
        header = TRUE,
        # Sanitize NA columns, with our improved defaults.
        na.strings = naStrings,
        # Never set factors on import automatically.
        stringsAsFactors = FALSE,
        # Always import starting from first line.
        skip = 0L,
        # Don't attempt to adjust names using `make.names()`.
        check.names = FALSE,
        # Return as `data.frame` instead of `data.table`.
        data.table = FALSE,
        # Keep quiet.
        showProgress = FALSE,
        verbose = FALSE
    )
    assert(is.data.frame(object))
    object <- .slotMetadata(object, pkg = "data.table", fun = "fread")
    object
}
