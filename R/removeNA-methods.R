#' @name removeNA
#' @inherit bioverbs::removeNA
#' @inheritParams params
#' @examples
#' ## atomic ====
#' removeNA(c("hello", "world", NA))
#' removeNA(c(1, 2, NA))
#'
#' ## matrix ====
#' from <- matrix(
#'     data = c(1, NA, 3, NA, NA, NA, 2, NA, 4),
#'     nrow = 3,
#'     ncol = 3
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
#'
#' ## DataFrame ====
#' from <- S4Vectors::DataFrame(
#'     a = c("A", NA, "C"),
#'     b = c(NA, NA, NA),
#'     c = c("B", NA, "D")
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
NULL



#' @importFrom bioverbs removeNA
#' @aliases NULL
#' @export
bioverbs::removeNA



removeNA.atomic <-  # nolint
    function(object) {
        na.omit(object)
    }



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("atomic"),
    definition = removeNA.atomic
)



.allNonNA <- function(x) {
    !all(is.na(x))
}



removeNA.matrix <-  # nolint
    function(object) {
        keepRows <- apply(X = object, MARGIN = 1L, FUN = .allNonNA)
        keepCols <- apply(X = object, MARGIN = 2L, FUN = .allNonNA)
        object[keepRows, keepCols, drop = FALSE]
    }



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("matrix"),
    definition = removeNA.matrix
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("sparseMatrix"),
    definition = removeNA.matrix
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("data.frame"),
    definition = removeNA.matrix
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("DataFrame"),
    definition = removeNA.matrix
)