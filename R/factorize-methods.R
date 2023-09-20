## FIXME Allow the user to specify which columns.



#' @name factorize
#' @inherit AcidGenerics::factorize
#' @note Updated 2023-09-20.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param j `vector`.
#' Column names or positions to evaluate.
#'
#' @param ... Additional arguments.
#'
#' @seealso
#' - Legacy `stringsAsFactors` approach for `data.frame` import.
#'
#' @examples
#' ## DFrame ====
#' object <- S4Vectors::DataFrame(
#'     "a" = c("a", "b", "c", "d"),
#'     "b" = c("a", "a", "b", "b"),
#'     "c" = c(1L, 2L, 3L, 4L),
#'     "d" = c(1L, 2L, 1L, 2L),
#'     "e" = c(TRUE, TRUE, FALSE, FALSE),
#'     row.names = c("A", "B", "C", "D")
#' )
#' object <- factorize(object)
#' print(object)
NULL



## Updated 2023-09-20.
`factorize,DFrame` <- # nolint
    function(object, j = NULL) {
        assert(is.null(j) || is.vector(j))
        if (is.null(j)) {
            lgl <- bapply(
                X = object,
                FUN = function(x) {
                    if (is.factor(x)) {
                        return(TRUE)
                    }
                    if (isS4(x) || !is.atomic(x) || is.logical(x)) {
                        return(FALSE)
                    }
                    ok <- hasDuplicates(na.omit(x))
                    ok
                }
            )
        }
        if (!any(lgl)) {
            return(object)
        }
        object <- as(object, "DFrame")
        idx <- which(lgl)
        object[idx] <- lapply(
            X = object[idx],
            FUN = function(x) {
                if (!is.factor(x)) {
                    x <- as.factor(x)
                }
                x <- droplevels(x)
                x
            }
        )
        object
    }



## Updated 2021-10-14.
`factorize,data.frame` <- # nolint
    `factorize,DFrame`



#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature(object = "DFrame"),
    definition = `factorize,DFrame`
)

#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature(object = "data.frame"),
    definition = `factorize,data.frame`
)
