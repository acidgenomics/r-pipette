.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' Import/export engines for parsing plain text delimited files
#'
#' @note Updated 2021-10-21.
#' @noRd
.engines <- c("base", "data.table", "readr")



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
#' @note Updated 2021-03-16.
#'
#' @examples
#' naStrings
naStrings <- c(
    ## > " ",  # data.table will warn about this.
    "",
    "#N/A",
    "#n/a",
    "-", # Used by NCBI (e.g. gene info files).
    "N/A",
    "NA",
    "NULL",
    "_",
    "n/a",
    "na",
    "null"
)



#' pipette test data URL
#'
#' @export
#' @keywords internal
#' @note Updated 2021-03-16.
#'
#' @examples
#' pipetteTestsURL
pipetteTestsURL <- paste0(
    "https://r.acidgenomics.com/testdata/pipette/",
    "v", .pkgVersion$major, ".", .pkgVersion$minor # nolint
)
