## readxl does support XLS format also but it's buggy for many files.
## Make some minimal repex examples and file issue on GitHub.
##
## See also:
## - https://github.com/tidyverse/readxl/issues/466
## - https://github.com/tidyverse/readxl/issues/472
##
## In the meantime, load using gdata, which is slow but does work.

#' @describeIn import Import a legacy Microsoft Excel worksheet.
#' @export
importXLS <- function(file, sheet = 1L, colnames = TRUE) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "gdata::read.xls"
    ))
    requireNamespace("gdata", quietly = TRUE)
    ## gdata currently has an OS.type partial match issue.
    ## `read.xls()` passes `...` to `utils::read.table()`.
    object <- withCallingHandlers(
        expr = gdata::read.xls(
            xls = file,
            sheet = sheet,
            verbose = FALSE,
            na.strings = naStrings,
            header = colnames
        ),
        warning = function(w) {
            ## nocov start
            if (isTRUE(grepl(
                pattern = "partial match of 'OS' to 'OS.type'",
                x = as.character(w)
            ))) {
                invokeRestart("muffleWarning")
            } else {
                w
            }
            ## nocov end
        }
    )
    object <- .slotMetadata(object, pkg = "gdata", fun = "read.xls")
    object
}
