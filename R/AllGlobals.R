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



#' pipette test data URL
#' @export
#' @keywords internal
#' @examples
#' pipetteTestsURL
pipetteTestsURL <- paste0(
    "https://tests.acidgenomics.com/pipette/",
    "v", .version$major, ".", .version$minor  # nolint
)



#' NA strings
#' @export
#' @examples
#' naStrings
naStrings <- c(
    "NA",
    "#N/A",
    "NULL",
    "null",
    ""
)
