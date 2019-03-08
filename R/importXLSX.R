importXLSX <- function(file, sheet = 1L) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using readxl::read_excel()."))
    requireNamespace("readxl", quietly = TRUE)
    object <- readxl::read_excel(
        path = file,
        sheet = sheet,
        col_names = TRUE,
        na = naStrings,
        # Keep quiet.
        progress = FALSE,
        # Don't attempt name repair.
        # Refer to `tibble()` documentation for details.
        .name_repair = "minimal"
    )
    # Always return as data.frame instead of tibble at this step.
    # Refer to S3 method for class `matrix` for supported params.
    object <- as.data.frame(
        x = object,
        make.names = FALSE,
        stringsAsFactors = FALSE
    )
    object <- .slotMetadata(object, pkg = "readxl", fun = "read_excel")
    object
}
