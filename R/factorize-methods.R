#' @name factorize
#' @inherit AcidGenerics::factorize
#' @note Updated 2022-08-17.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso
#' - Legacy `stringsAsFactors` approach for `data.frame` import.
#'
#' @examples
#' ## DataFrame ====
#' df <- S4Vectors::DataFrame(
#'     "a" = letters[seq_len(5L)],
#'     "b" = seq_len(5L)
#' )
#' x <- factorize(df)
NULL



## FIXME Add a setting here (or default?) to only apply as.factor coerction
## to columns with repeated values...

## FIXME In the unit test, check that non-repeated values do not get factorized.



## Updated 2022-08-17.
`factorize,DataFrame` <- # nolint
    function(object) {
        class <- class(object)[[1L]]
        out <- lapply(
            X = object,
            FUN = function(x) {
                ## FIXME Early return if there are no repeated values
                ## Use anyDuplicated to check for this and early return.
                droplevels(as.factor(x))

                ## FIXME Better method that we should consider.
                ## Which metadata columns should be factorized?
                apply(
                    X = colData,
                    FUN = function(x) {
                        anyDuplicated(na.omit(x)) > 0L
                    },
                    MARGIN = 2L
                )
            }
        )
        out <- as(out, Class = class)
        names(out) <- names(object)
        rownames <- rownames(object)
        if (!is.null(rownames)) {
            rownames(out) <- rownames
        }
        out
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
