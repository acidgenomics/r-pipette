#' @describeIn import Import bcbio count matrix file.
#' These files contain an `"id"` column that we need to coerce to row names.
importBCBCounts <- function(file) {
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "data.table::fread"
    ))
    file <- localOrRemoteFile(file)
    object <- fread(
        file = file,
        na.strings = naStrings
    )
    assert(
        isSubset("id", colnames(object)),
        hasNoDuplicates(object[["id"]])
    )
    object <- as.data.frame(object)
    object <- column_to_rownames(object, var = "id")
    object <- as.matrix(object)
    mode(object) <- "integer"
    object <- .slotMetadata(object, pkg = "data.table", fun = "fread")
    object
}
