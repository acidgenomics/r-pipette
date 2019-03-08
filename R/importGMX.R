# Import a GMX file, for GSEA.
importGMX <- function(file) {
    file <- localOrRemoteFile(file)
    lines <- read_lines(file)
    pathways <- list(tail(lines, n = -2L))
    names(pathways) <- lines[[1L]]
    pathways
}
