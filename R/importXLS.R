# readxl does support XLS format also but it's buggy for many files.
# Make some minimal repex examples and file issue on GitHub.
# See also:
# - https://github.com/tidyverse/readxl/issues/466
# - https://github.com/tidyverse/readxl/issues/472
# In the meantime, load using gdata, which is slow but does work.
importXLS <- function(file, sheet = 1L, colnames = TRUE) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using gdata::read.xls()."))
    requireNamespace("gdata", quietly = TRUE)
    object <- gdata::read.xls(
        xls = file,
        sheet = sheet,
        verbose = FALSE,
        na.strings = naStrings,
        # This function passes `...` to `utils::read.table()`.
        header = colnames
    )
    object <- .slotMetadata(object, pkg = "gdata", fun = "read.xls")
    object
}
