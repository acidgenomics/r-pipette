#' File extension types by group
#'
#' @note Updated 2021-01-13.
#' @noRd
.extGroup <- list(
    delim = c("csv", "table", "tsv", "txt"),
    excel = c("xls", "xlsb", "xlsx"),
    lines = c(
        "lines",
        "log",
        "md",        # Markdown
        "py",        # Python
        "r",         # R
        "rmd",       # R Markdown
        "sh"         # Shell
    ),
    rda = c("rda", "rdata"),
    rio = c(
        "arff",      # Weka Attribute-Relation File Format
        "dbf",       # dBase Database File
        "dif",       # Data Interchange Format
        "dta",       # Stata
        "fwf",       # Fixed width formatting
        "mat",       # Matlab
        "mtp",       # Minitab
        "ods",       # OpenDocument (LibreOffice)
        "por",       # SPSS
        "psv",       # Pipe separated values
        "sas7bdat",  # SASS
        "sav",       # SPSS
        "syd",       # Systat
        "rec",       # Epi Info
        "xpt"        # SASS
    ),
    rtracklayer = c(
        "bed", "bed15", "bedgraph", "bedpe",
        "bigwig", "bw", "wig",
        "gff", "gff1", "gff2", "gff3", "gtf",
        "broadpeak", "narrowpeak"
    ),
    yaml = c("yaml", "yml")
)



#' Import/export engines for parsing plain text delimited files
#'
#' @note Updated 2021-01-13.
#' @noRd
.delimEngines <- c("vroom", "data.table", "readr", "base")

#' Default plain text engine
#'
#' @note Updated 2021-01-13.
#' @noRd
.defaultDelimEngine <- .delimEngines[[1L]]



#' R data extension pattern
#'
#' @note Updated 2020-10-07.
#' @noRd
.rdataExtPattern <- "\\.(rd[a|ata|s])$"



#' R data load error
#'
#' @note Updated 2020-10-07.
#' @noRd
.rdataLoadError <- paste(
    "Failed to load data.",
    "R data files must contain '.rda', '.rds', or '.RData' extension.",
    sep = "\n"
)



#' Package version
#'
#' @note Updated 2020-10-07.
#' @noRd
.version <- packageVersion(packageName())



#' NA strings
#'
#' @export
#' @note Updated 2021-01-13.
#'
#' @examples
#' naStrings
naStrings <- c(
    "",
    "#N/A",
    "#n/a",
    "N/A",
    "NA",
    "NULL",
    "n/a",
    "na",
    "null"
)



#' pipette test data URL
#'
#' @export
#' @keywords internal
#' @note Updated 2021-01-13.
#'
#' @examples
#' pipetteTestsURL
pipetteTestsURL <- paste0(
    "https://tests.acidgenomics.com/pipette/",
    "v", .version$major, ".", .version$minor  # nolint
)
