## Import a delimited file (e.g. CSV, TSV).
## Updated 2019-07-19.
importDelim <- function(file, colnames = TRUE) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using data.table::fread()."))
    object <- fread(
        file = file,
        header = colnames,
        ## Sanitize NA columns, with our improved defaults.
        na.strings = naStrings,
        ## Never set factors on import automatically.
        stringsAsFactors = FALSE,
        ## Keep quiet.
        verbose = FALSE,
        ## Always import starting from first line.
        skip = 0L,
        ## Don't attempt to adjust names using `make.names()`.
        check.names = FALSE,
        strip.white = TRUE,
        ## This matches the conventions in the tidyverse readers.
        blank.lines.skip = TRUE,
        ## Keep quiet.
        showProgress = FALSE,
        ## Return as `data.frame` instead of `data.table`.
        data.table = FALSE
    )
    assert(is.data.frame(object))
    object <- .slotMetadata(object, pkg = "data.table", fun = "fread")
    object
}
