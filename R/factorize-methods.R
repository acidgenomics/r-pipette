## FIXME Add a setting here (or default?) to only apply as.factor coerction
## to columns with repeated values...
## FIXME In the unit test, check that non-repeated values do not get factorized.
## FIXME Improve the working example here.



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
        hasDupes <- bapply(
            X = object,
            FUN = function(x) {
                anyDuplicated(na.omit(x)) > 0L
            }
        )
        if (!any(hasDupes)) {
            return(object)
        }
        object <- as(object, "DataFrame")
        idx <- which(hasDupes)
        object[idx] <- lapply(
            X = object[idx],
            FUN = function(x) {
                droplevels(as.factor(x))
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
