#' @name factorize
#' @inherit AcidGenerics::factorize
#' @note Updated 2021-02-11.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' ## DataFrame ====
#' df <- DataFrame(
#'     "a" = letters[seq_len(5L)],
#'     "b" = seq_len(5L)
#' )
#' x <- factorize(df)
NULL



## Updated 2019-07-19.
`factorize,DataFrame` <-  # nolint
    function(object) {
        class <- class(object)[[1L]]
        out <- lapply(
            X = object,
            FUN = function(x) {
                droplevels(as.factor(x))
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
`factorize,data.frame` <-  # nolint
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
