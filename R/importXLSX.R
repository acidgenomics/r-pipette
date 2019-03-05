importXLSX <- function(file, ...) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using readxl::read_excel()."))
    requireNamespace("readxl", quietly = TRUE)
    object <- readxl::read_excel(path = file, na = naStrings, ...)
    # Coerce tbl_df to data.frame.
    object <- as.data.frame(object)
    object <- .slotVersion(object, pkg = "readxl")
    object
}
