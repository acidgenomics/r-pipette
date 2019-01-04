# bcbio count matrix file.
importCounts <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using readr::read_tsv()."
    ))
    data <- read_tsv(file = file, na = naStrings, ...)
    assert(
        isSubset("id", colnames(data)),
        hasNoDuplicates(data[["id"]])
    )
    # Coerce tibble to data frame.
    data <- as.data.frame(data)
    # Need to move the "id" column to rownames.
    data <- column_to_rownames(data, var = "id")
    # Coerce data.frame to matrix.
    data <- as.matrix(data)
    data
}
