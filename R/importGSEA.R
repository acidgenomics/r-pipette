#' @describeIn import Import a gene matrix transposed file.
#' See also `fgsea::gmtPathways()`.
#' @export
importGMT <- function(file) {
    message(sprintf("Importing '%s'.", basename(file)))
    lines <- importLines(file)
    lines <- strsplit(lines, split = "\t")
    pathways <- lapply(lines, tail, n = -2L)
    names(pathways) <- vapply(
        X = lines,
        FUN = head,
        FUN.VALUE = character(1L),
        n = 1L
    )
    pathways
}



#' @describeIn import Import a gene matrix file.
#' @export
importGMX <- function(file) {
    message(sprintf("Importing '%s'.", basename(file)))
    lines <- importLines(file)
    pathways <- list(tail(lines, n = -2L))
    names(pathways) <- lines[[1L]]
    pathways
}



#' @describeIn import Import a gene set file.
#' @export
importGRP <- importGMX
