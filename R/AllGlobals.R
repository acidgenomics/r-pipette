.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' Import/export engines for parsing plain text delimited files
#'
#' @note Updated 2021-03-16.
#' @noRd
.engines <- c("data.table", "vroom", "readr", "base")



## FIXME This might be OK to remove.
## FIXME Remove elements that aren't used here.

#' Shared list of optional default formals
#'
#' @note Updated 2021-09-24.
#' @noRd
.formalsList <- list(
    "export.compress" = quote(
        getOption("acid.export.compress", default = FALSE)
    ),
    "export.dir" = quote(
        getOption("acid.export.dir", default = ".")
    ),
    "export.ext" = quote(
        getOption("acid.export.ext", default = "csv")
    ),
    "export.sparse.ext" = quote(
        getOption("acid.export.sparse.ext", default = "mtx")
    ),
    "export.quiet" = quote(
        getOption("acid.export.quiet", default = FALSE)
    ),
    "import.make.names" = quote(
        getOption("acid.import.make.names", default = syntactic::makeNames)
    ),
    "import.metadata" = quote(
        getOption("acid.import.metadata", default = FALSE)
    ),
    "load.dir" = quote(
        getOption("acid.load.dir", default = ".")
    ),
    "overwrite" = quote(
        getOption("acid.overwrite", default = TRUE)
    ),
    "quiet" = quote(
        getOption("acid.quiet", default = FALSE)
    ),
    ## FIXME Consider removing this.
    "save.compress" = quote(
        getOption("acid.save.compress", default = TRUE)
    ),
    ## FIXME Move this to pipette?
    "save.dir" = quote(
        getOption("acid.save.dir", default = ".")
    ),
    ## FIXME Move this to pipette?
    "save.ext" = quote(
        getOption("acid.save.ext", default = "rds")
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
