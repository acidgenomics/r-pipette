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
#' @seealso
#' - `Rle()`.
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



## FIXME Figure out which columns to encode here.

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
            assert(
                allAreIntegerish(j),
                length(j) <= ncol(x)
            )
            idx <- seq(from = 1L, to = ncol(x))
            lgl <- idx %in% j
        }
        ## FIXME Check for atomic or Rle.
        lst <- Map(
            f = function(x, eval) {
                if (isFALSE(eval)) {
                }
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



## Updated 2023-09-19.
`encode,Rle` <- # nolint
    function(x) {
        x
    }



## Updated 2023-09-19.
`encode,atomic` <- # nolint
    function(x) {
        Rle(x)
    }



## Updated 2023-09-19.
`encode,factor` <- # nolint
    function(x) {
        x <- Rle(x)
        x <- droplevels(x)
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

#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature(x = "Rle"),
    definition = `encode,Rle`
)

#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature(x = "atomic"),
    definition = `encode,atomic`
)

#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature(x = "factor"),
    definition = `encode,factor`
)
