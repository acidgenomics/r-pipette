import_xlsx <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using readxl::read_excel()."
    ))
    requireNamespace("readxl", quietly = TRUE)
    data <- readxl::read_excel(path = file, na = naStrings, ...)
    # Coerce tbl_df to data.frame.
    data <- as.data.frame(data)
    data
}
