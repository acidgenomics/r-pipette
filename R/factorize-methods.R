#' @name factorize
#' @inherit AcidGenerics::factorize
#' @note Updated 2022-08-19.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso
#' - Legacy `stringsAsFactors` approach for `data.frame` import.
#'
#' @examples
## DataFrame ====
#' object <- S4Vectors::DataFrame(
#'     "a" = c("a", "b", "c", "d"),
#'     "b" = c("a", "a", "b", "b"),
#'     "c" = c(1L, 2L, 3L, 4L),
#'     "d" = c(1L, 2L, 1L, 2L),
#'     row.names = c("A", "B", "C", "D")
#' )
#' object <- factorize(object)
#' print(object)
NULL



## Updated 2022-08-19.
`factorize,DataFrame` <- # nolint
    function(object) {
        isFactor <- bapply(
            X = object,
            FUN = function(x) {
                if (is.factor(x)) {
                    return(TRUE)
                }
                if (!is.atomic(x)) {
                    return(FALSE)
                }
                ok <- anyDuplicated(na.omit(x)) > 0L
                ok
            }
        )
        if (!any(isFactor)) {
            return(object)
        }
        object <- as(object, "DataFrame")
        idx <- which(isFactor)
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
    `factorize,DataFrame`



#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature(object = "DataFrame"),
    definition = `factorize,DataFrame`
)

#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature(object = "data.frame"),
    definition = `factorize,data.frame`
)
