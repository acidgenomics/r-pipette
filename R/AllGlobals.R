.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' R data extension pattern
#'
#' @note Updated 2020-10-07.
#' @noRd
.rdataExtPattern <- "\\.(rd[a|ata|s])$"



#' R data load error
#'
#' @note Updated 2021-08-24.
#' @noRd
.rdataLoadError <- sprintf(
    fmt = paste0(
        "Failed to load data.\n",
        "R data files must contain ",
        "{.var %s}, {.var %s}, or {.var %s} extension."
    ),
    "rda", "rds", "RData"
)



## Updated 2021-03-16.
.symError <- paste(
    "'export()' requires 'name' argument when 'object'",
    "is passed in directly, not as a symbol."
)



#' NA strings
#'
#' @export
#' @note Updated 2023-09-19.
#'
#' @examples
#' naStrings
naStrings <- c(
    ## > " ",  # data.table will warn about this.
    ## > "-", # Used by NCBI (e.g. gene info files).
    "",
    "#N/A",
    "#n/a",
    "N/A",
    "NA",
    "NULL",
    "\\N",
    "_",
    "n/a",
    "na",
    "null"
)



#' pipette test data URL
#'
#' @export
#' @keywords internal
#' @note Updated 2023-09-28.
#'
#' @examples
#' pipetteTestsUrl
pipetteTestsUrl <- "https://r.acidgenomics.com/testdata/pipette"
