#' @describeIn import Import bcbio count matrix file.
#' These files contain an `"id"` column that we need to coerce to row names.
importBCBCounts <- function(file) {
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "readr::read_tsv"
    ))
    object <- read_tsv(
        file = file,
        col_names = TRUE,
        na = naStrings,
        ## Keep quiet.
        progress = FALSE,
        skip_empty_rows = TRUE
    )
    assert(
        isSubset("id", colnames(object)),
        hasNoDuplicates(object[["id"]])
    )
    ## Coerce tibble to data frame.
    object <- as.data.frame(object)
    ## Need to move the "id" column to rownames.
    object <- column_to_rownames(object, var = "id")
    ## Coerce data frame to matrix.
    object <- as.matrix(object)
    object <- .slotMetadata(object, pkg = "readr", fun = "read_tsv")
    object
}
