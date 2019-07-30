## Note that `read_excel()` doesn't currently support automatic blank lines
## removal, so ensure that is fixed downstream.
## Updated 2019-07-30.
importXLSX <- function(file, sheet = 1L, colnames = TRUE) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using readxl::read_excel()."))
    requireNamespace("readxl", quietly = TRUE)
    object <- readxl::read_excel(
        path = file,
        sheet = sheet,
        col_names = colnames,
        na = naStrings,
        trim_ws = TRUE,
        ## Keep quiet.
        progress = FALSE,
        ## Don't attempt name repair.
        ## Refer to `tibble()` documentation for details.
        .name_repair = "minimal"
    )
    ## Always return as data.frame instead of tibble at this step.
    object <- as.data.frame(
        x = object,
        make.names = FALSE,
        stringsAsFactors = FALSE
    )
    object <- .slotMetadata(object, pkg = "readxl", fun = "read_excel")
    object
}
