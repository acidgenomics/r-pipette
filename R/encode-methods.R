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
#' df <- DataFrame(
#'     a = rep(x = binary, times = 50L),
#'     b = rep(x = binary, each = 50L)
#' )
#' lapply(df, class)
#' x <- encode(df)
#' lapply(x, class)
NULL



## Updated 2021-06-09.
`encode,DataFrame` <-  # nolint
    function(x) {
        meta <- metadata(x)
        rn <- rownames(x)
        x <- lapply(
            X = x,
            FUN = function(x) {
                if (is(x, "List")) {
                    return(x)
                }
                ## Decode Rle, if necessary.
                if (is(x, "Rle")) {
                    x <- decode(x)
                }
                ## Adjust (drop) factor levels, if necessary.
                if (is.factor(x)) {
                    x <- droplevels(x)
                }
                ## Use run-length encoding on atomics.
                if (is.atomic(x)) {
                    x <- Rle(x)
                    return(x)
                }
                x <- I(x)
                x
            }
        )
        args <- list(x, row.names = rn)
        out <- do.call(what = DataFrame, args = args)
        metadata(out) <- meta
        out
    }



#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature("DataFrame"),
    definition = `encode,DataFrame`
)



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
    signature = signature("Ranges"),
    definition = `encode,Ranges`
)
