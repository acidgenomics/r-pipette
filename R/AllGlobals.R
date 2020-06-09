globalVariables(".")



.rdataExtPattern <- "\\.(rd[a|ata|s])$"

.rdataLoadError <- paste(
    "Failed to load data.",
    "R data files must contain '.rda', '.rds', or '.RData' extension.",
    sep = "\n"
)

.version <- packageVersion("pipette")



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
