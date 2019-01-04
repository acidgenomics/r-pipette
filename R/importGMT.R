# @seealso `fgsea::gmtPathways()`.
importGMT <- function(file, ...) {
    lines <- read_lines(file, ...)
    lines <- strsplit(lines, split = "\t")
    pathways <- lapply(lines, tail, n = -2L)
    names(pathways) <- vapply(
        X = lines,
        FUN = head,
        FUN.VALUE = character(1),
        n = 1L
    )
    pathways
}
