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
            assert(length(j) <= ncol(x))
            idx <- seq(from = 1L, to = ncol(x))
            lgl <- idx %in% j
        }
        assert(
            is.logical(lgl),
            hasLength(lgl, n = ncol(x))
        )
        lst <- Map(
            f = function(x, eval) {
                if (isFALSE(eval)) {
                    return(x)
                }
                x <- unname(x)
                if (is(x, "Rle")) {
                    x <- decode(x)
                }
                if (is.factor(x)) {
                    x <- droplevels(x)
                }
                x
            },
            x = x,
            eval = lgl
        )
        out <- as.DataFrame(lst)
        dimnames(out) <- dimnames(x)
        metadata(out) <- metadata(x)
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
