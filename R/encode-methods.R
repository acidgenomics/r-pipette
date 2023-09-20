## FIXME Consider reworking to use "object[idx] <- lapply(" approach from factorize.



#' @name encode
#' @inherit AcidGenerics::encode
#' @note Updated 2023-09-20.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param j `vector`.
#' Column names or positions to evaluate.
#'
#' @param ... Additional arguments.
#'
#' @seealso `Rle()`.
#'
#' @return Modified object.
#' All `atomic` columns will be encoded to `Rle` S4 class.
#'
#' @examples
#' ## DFrame ====
#' binary <- seq(from = 0L, to = 1L)
#' object <- S4Vectors::DataFrame(
#'     "a" = rep(x = binary, times = 50L),
#'     "b" = rep(x = binary, each = 50L)
#' )
#' lapply(object, class)
#' object <- encode(object)
#' lapply(object, class)
NULL



## Updated 2023-09-20.
`encode,DFrame` <- # nolint
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
            f = function(x) {
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



## Updated 2023-09-19.
`encode,Ranges` <- # nolint
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
    signature = signature(x = "DFrame"),
    definition = `encode,DFrame`
)

#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature(x = "Ranges"),
    definition = `encode,Ranges`
)
