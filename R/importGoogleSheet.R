#' Import Google Sheet
#' Using experimental googlesheets4 package instead of googlesheets.
#' @noRd
importGoogleSheet <- function(file, sheet = 1L, colnames = TRUE) {
    message(paste(
        "Importing", basename(file), "using googlesheets4::read_sheet()."
    ))
    requireNamespace("googlesheets4", quietly = TRUE)
    object <- googlesheets4::read_sheet(
        ss = basename(file),
        sheet = sheet,
        col_names = colnames,
        na = naStrings,
        trim_ws = TRUE
    )
    # Always return as data.frame instead of tibble at this step.
    object <- as.data.frame(
        x = object,
        make.names = FALSE,
        stringsAsFactors = FALSE
    )
    object <- .slotMetadata(object, pkg = "googlesheets4", fun = "read_sheet")
    object
}
