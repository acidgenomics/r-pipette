## Sparse matrix. Note that we're warning the user if row and column
## name sidecar files don't exist.
## Updated 2019-07-19.
importMTX <- function(file) {
    assert(isString(file))
    ## Add the rownames automatically using `.rownames` sidecar file.
    rownamesFile <- paste(file, "rownames", sep = ".")
    rownamesFile <- tryCatch(
        expr = localOrRemoteFile(rownamesFile),
        error = function(e) {
            ## nocov start
            warning(sprintf(
                fmt = paste0(
                    "'%s' does not exist.\n",
                    "  Row names will not be added to sparse matrix."
                ),
                basename(rownamesFile)
            ))
            NULL
            ## nocov end
        }
    )
    ## Add the colnames automatically using `.colnames` sidecar file.
    colnamesFile <- paste(file, "colnames", sep = ".")
    colnamesFile <- tryCatch(
        expr = localOrRemoteFile(colnamesFile),
        error = function(e) {
            ## nocov start
            warning(sprintf(
                fmt = paste0(
                    "'%s' does not exist.\n",
                    "  Column names will not be added to sparse matrix."
                ),
                basename(colnamesFile)
            ))
            NULL
            ## nocov end
        }
    )
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "Matrix::readMM"
    ))
    object <- readMM(file = file)
    if (!is.null(rownamesFile)) {
        rownames(object) <- .importSidecar(rownamesFile)
    }
    if (!is.null(colnamesFile)) {
        colnames(object) <- .importSidecar(colnamesFile)
    }
    object <- .slotMetadata(object, pkg = "Matrix", fun = "readMM")
    object
}



## Sparse matrix sidecar files (.rownames, .colnames)
## Updated 2019-07-19.
.importSidecar <- function(file) {
    message(sprintf(
        "Importing sidecar '%s' using '%s()'.",
        basename(file), "readr::read_lines"
    ))
    read_lines(file = file, na = naStrings)
}
