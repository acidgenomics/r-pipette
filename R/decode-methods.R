#' Decode column data that uses run-length encoding
#'
#' @name decode
#' @inherit AcidGenerics::decode description return
#' @note Updated 2021-02-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#' Columns will be decoded and no longer `Rle` class.
#'
#' @seealso `S4Vectors::decode()`.
#'
#' @examples
#' data(DFrame, package = "AcidTest")
#' df <- DFrame
#'
#' ## DataFrame ====
#' df <- encode(df)
#' lapply(df, class)
#' x <- decode(df)
#' lapply(x, class)
NULL



## FIXME This may not work as expected for Bioconductor 3.13.
## Need to refer to list to DataFrame coercion method...

## Updated 2021-05-18.
`decode,DataFrame` <-  # nolint
    function(x) {
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
                if (is.atomic(x)) {
                    return(x)
                }
                x <- I(x)
                x
            }
        )
        args <- list(x, row.names = rn)
        do.call(what = DataFrame, args = args)
    }



#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature("DataFrame"),
    definition = `decode,DataFrame`
)



## Updated 2019-07-20.
`decode,Ranges` <-  # nolint
    function(x) {
        if (!is.null(mcols(x))) {
            mcols(x) <- decode(mcols(x))
        }
        x
    }



#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature("Ranges"),
    definition = `decode,Ranges`
)
