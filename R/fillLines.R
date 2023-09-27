#' Fill delimited lines with NA values
#'
#' @export
#' @note Updated 2023-09-27.
#'
#' @param object `character`.
#'
#' @param format `character(1)`.
#' Delimited input format.
#'
#' @return `character`.
#' Modified character vector, with consistent number of delimited values.
#'
#' @seealso
#' - `textConnection`: For import approach after modification.
#'
#' @examples
#' ## character ====
#' ## CSV format.
#' object <- c(
#'     "\"aaa\",\"bbb\",\"ccc\",\"ddd\"",
#'     "\"eee\",\"fff\",\"ggg\"",
#'     "\"hhh\",\"iii\""
#' )
#' print(object)
#' object <- fillLines(object, format = "csv")
#' print(object)
#' con <- textConnection(object)
#' object <- import(con, format = "csv")
#' print(object)
#' close(con)
#'
#' ## TSV format.
#' object <- c(
#'     "aaa\tbbb\tccc\tddd",
#'     "eee\tfff\tggg",
#'     "hhh\tiii"
#' )
#' print(object)
#' object <- fillLines(object, format = "tsv")
#' print(object)
#' con <- textConnection(object)
#' object <- import(con, format = "tsv")
#' print(object)
#' close(con)
fillLines <- function(object, format = c("csv", "tsv")) {
    format <- match.arg(format)
    sep <- switch(EXPR = format, "csv" = ",", "tsv" = "\t")
    assert(
        is.character(object),
        isMatchingFixed(x = object[[1L]], pattern = sep)
    )
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
