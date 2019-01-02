# Sparse matrix. Note that we're warning the user if row and column
# name sidecar files don't exist.
import_mtx <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using Matrix::readMM()."
    ))
    data <- readMM(file = file, ...)

    # Add the rownames automatically using `.rownames` sidecar file.
    rownamesFile <- paste(file, "rownames", sep = ".")
    rownamesFile <- tryCatch(
        expr = localOrRemoteFile(rownamesFile),
        error = function(e) {
            warning(paste0(
                basename(rownamesFile), " does not exist.\n",
                "  Row names will not be added to sparse matrix."
            ), call. = FALSE)
            NULL
        }
    )
    if (!is.null(rownamesFile)) {
        rownames(data) <- import.mtx.sidecar(rownamesFile)
    }

    # Add the colnames automatically using `.colnames` sidecar file.
    colnamesFile <- paste(file, "colnames", sep = ".")
    colnamesFile <- tryCatch(
        expr = localOrRemoteFile(colnamesFile),
        error = function(e) {
            warning(paste0(
                basename(colnamesFile), " does not exist.\n",
                "  Column names will not be added to sparse matrix."
            ), call. = FALSE)
            NULL
        }
    )
    if (!is.null(colnamesFile)) {
        colnames(data) <- import_mtx_sidecar(colnamesFile)
    }

    data
}



# Sparse matrix sidecar files (.rownames, .colnames)
import_mtx_sidecar <- function(file, ...) {
    message(paste(
        "Importing sidecar", basename(file),
        "using readr::read_lines()."
    ))
    read_lines(file = file, na = naStrings, ...)
}
