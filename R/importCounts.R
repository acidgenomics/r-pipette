# bcbio count matrix file.
importCounts <- function(file, ...) {
    message(paste("Importing", basename(file), "using readr::read_tsv()."))
    object <- read_tsv(file = file, na = naStrings, ...)
    assert(
        isSubset("id", colnames(object)),
        hasNoDuplicates(object[["id"]])
    )
    # Coerce tibble to data frame.
    object <- as.data.frame(object)
    # Need to move the "id" column to rownames.
    object <- column_to_rownames(object, var = "id")
    # Coerce data frame to matrix.
    object <- as.matrix(object)
    object <- .slotMetadata(object, pkg = "readr", fun = "read_tsv")
    object
}
