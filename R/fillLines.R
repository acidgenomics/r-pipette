#' Fill delimited lines with NA values
#'
#' @export
#' @note Updated 2023-09-27.
#'
#' @param object `character`.
#'
#' @return `character`.
#' Modified character vector, with consistent number of delimited values.
#'
#' @seealso
#' - `textConnection`: For import approach after modification.
#'
#' @examples
#' ## character ====
#' object <- c(
#'     "aaa\tbbb",
#'     "ccc\tddd\teee",
#'     "fff\tggg\thhh\tiii"
#' )
#' print(object)
#' object <- fillLines(object, sep = "\t")
#' print(object)
fillLines <- function(object, sep = c("\t", ",")) {
    sep <- match.arg(sep)
    assert(isMatchingFixed(x = object[[1L]], pattern = sep))
    spl <- strsplit(x = object, split = sep, fixed = TRUE)
    len <- lengths(spl)
    nMax <- max(len)
    fixIdx <- which(len <= nMax)
    spl[fixIdx] <- Map(
        x = spl[fixIdx],
        n = len[fixIdx],
        MoreArgs = list("nMax" = nMax),
        f = function(x, n, nMax) {
            append(x = x, values = rep("NA", times = nMax - n))
        }
    )
    assert(all(lengths(spl) == nMax))
    out <- vapply(
        X = spl,
        FUN = paste0,
        collapse = sep,
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    out
}
