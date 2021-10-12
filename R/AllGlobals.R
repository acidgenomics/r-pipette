.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' Import/export engines for parsing plain text delimited files
#'
#' @note Updated 2021-03-16.
#' @noRd
.engines <- c("data.table", "vroom", "readr", "base")



## FIXME Only include parameters that are used more than 1x.

#' Shared list of optional default formals
#'
#' @note Updated 2021-09-24.
#' @noRd
.formalsList <- list(
    "export.dir" = quote(
        getOption(x = "acid.export.dir", default = getwd())
    ),
    "import.make.names" = quote(
        getOption(x = "acid.import.make.names", default = syntactic::makeNames)
    ),
    "import.metadata" = quote(
        getOption(x = "acid.import.metadata", default = FALSE)
    ),
    "overwrite" = quote(
        getOption(x = "acid.overwrite", default = TRUE)
    ),
    "quiet" = quote(
        getOption(x = "acid.quiet", default = FALSE)
    )
)



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
    "-",  # Used by NCBI (e.g. gene info files).
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
    "v", .pkgVersion$major, ".", .pkgVersion$minor  # nolint
)
