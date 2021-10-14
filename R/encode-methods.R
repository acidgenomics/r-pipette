#' @name encode
#' @inherit AcidGenerics::encode
#' @note Updated 2021-06-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso `Rle()`.
#'
#' @return Modified object.
#' All `atomic` columns will be encoded to `Rle` S4 class.
#'
#' @examples
#' ## DataFrame ====
#' binary <- seq(from = 0L, to = 1L)
#' object <- DataFrame(
#'     "a" = rep(x = binary, times = 50L),
#'     "b" = rep(x = binary, each = 50L)
#' )
#' lapply(object, class)
#' object <- encode(object)
#' lapply(object, class)
NULL



## Updated 2021-08-24.
`encode,DataFrame` <-  # nolint
    function(x) {
        if (!(hasCols(x) && hasRows(x))) {
            return(x)  # nocov
        }
        meta <- metadata(x)
        rn <- rownames(x)
        list <- lapply(
            X = x,
            FUN = function(x) {
                if (is(x, "List")) {
                    return(x)  # nocov
                }
                ## Decode Rle, if necessary.
                if (is(x, "Rle")) {
                    x <- decode(x)
                }
                ## Adjust (drop) factor levels, if necessary.
                if (is.factor(x)) {
                    x <- droplevels(x)  # nocov
                }
                ## Use run-length encoding on atomics.
                if (is.atomic(x)) {
                    x <- Rle(x)
                }
                x
            }
        )
        out <- as.DataFrame(list)
        rownames(out) <- rn
        metadata(out) <- meta
        out
    }



## Updated 2019-07-20.
`encode,Ranges` <-  # nolint
    function(x) {
        if (!is.null(mcols(x))) {
            mcols(x) <- encode(mcols(x))
        }
        x
    }



#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature(x = "DataFrame"),
    definition = `encode,DataFrame`
)

#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature(x = "Ranges"),
    definition = `encode,Ranges`
)
