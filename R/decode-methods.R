#' Decode data that uses run-length encoding
#'
#' @name decode
#' @note Updated 2022-02-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#' Columns will be decoded and no longer `Rle` class.
#'
#' @seealso
#' - `S4Vectors::decode()`.
#'
#' @examples
#' data(DataFrame, package = "AcidTest")
#'
#' ## DataFrame ====
#' object <- DataFrame
#' object <- encode(object)
#' lapply(object, class)
#' object <- decode(object)
#' lapply(object, class)
NULL



## Updated 2021-06-09.
`decode,DataFrame` <- # nolint
    function(x) {
        if (!(hasCols(x) && hasRows(x))) {
            return(x)
        }
        meta <- metadata(x)
        rn <- rownames(x)
        list <- lapply(
            X = x,
            FUN = function(x) {
                if (is(x, "List")) {
                    return(x) # nocov
                }
                ## Decode Rle, if necessary.
                if (is(x, "Rle")) {
                    x <- decode(x)
                }
                ## Adjust (drop) factor levels, if necessary.
                if (is.factor(x)) {
                    x <- droplevels(x)
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
`decode,Ranges` <- # nolint
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
    signature = signature(x = "DataFrame"),
    definition = `decode,DataFrame`
)

#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature(x = "Ranges"),
    definition = `decode,Ranges`
)
