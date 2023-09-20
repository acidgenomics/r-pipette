#' Decode data that uses run-length encoding
#'
#' @name decode
#' @note Updated 2023-09-20.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param j `vector`.
#' Column names or positions to evaluate.
#'
#' @param ... Additional arguments.
#'
#' @return Modified object.
#' Columns will be decoded and no longer `Rle` class.
#'
#' @seealso
#' - `S4Vectors::decode`.
#' - `S4Vectors::Rle`.
#'
#' @examples
#' data(DFrame, package = "AcidTest")
#'
#' ## DFrame ====
#' object <- DFrame
#' object <- encode(object)
#' lapply(object, class)
#' object <- decode(object)
#' lapply(object, class)
NULL



## Updated 2023-09-12.
`decode,DFrame` <- # nolint
    function(x, j = NULL) {
        assert(is.null(j) || is.vector(j))
        if (is.null(j)) {
            if (!(hasCols(x) && hasRows(x))) {
                return(x)
            }
            lgl <- rep(x = TRUE, times = ncol(x))
        } else if (is.character(j)) {
            assert(
                hasColnames(x),
                isSubset(j, colnames(x))
            )
            lgl <- colnames(x) %in% j
        } else {
            assert(
                allAreIntegerish(j),
                length(j) <= ncol(x)
            )
            idx <- seq(from = 1L, to = ncol(x))
            lgl <- idx %in% j
        }
        lgl <- unlist(Map(
            f = function(x, eval) {
                if (isFALSE(eval)) {
                    return(FALSE)
                }
                is(x, "Rle")
            },
            x = x,
            eval = lgl
        ))
        if (!any(lgl)) {
            return(x)
        }
        idx <- which(lgl)
        x[idx] <- lapply(X = x[idx], FUN = decode)
        x
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
    signature = signature(x = "DFrame"),
    definition = `decode,DFrame`
)

#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature(x = "Ranges"),
    definition = `decode,Ranges`
)
