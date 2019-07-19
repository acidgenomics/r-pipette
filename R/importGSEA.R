#' Gene Matrix Transposed file format
#' Import a GMT file, for GSEA.
#' @seealso `fgsea::gmtPathways()`.
#' @noRd
# Updated 2019-07-19.
importGMT <- function(file) {
    message(paste("Importing", basename(file)))
    file <- localOrRemoteFile(file)
    lines <- read_lines(file)
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



#' Gene MatriX file format
#' Import a GMX file, for GSEA.
#' @noRd
# Updated 2019-07-19.
importGMX <- function(file) {
    message(paste("Importing", basename(file)))
    file <- localOrRemoteFile(file)
    lines <- read_lines(file)
    pathways <- list(tail(lines, n = -2L))
    names(pathways) <- lines[[1L]]
    pathways
}



#' Gene set file format
#' Import a GRP file, for GSEA.
#' @noRd
# Updated 2019-07-19.
importGRP <- importGMX
