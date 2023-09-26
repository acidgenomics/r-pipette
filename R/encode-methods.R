## FIXME Need to add support for date.



#' @name encode
#' @inherit AcidGenerics::encode
#' @note Updated 2023-09-26.
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



## Updated 2023-09-26.
`encode,Date` <- # nolint
    function(x) {
        x
    }



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
        lgl <- unlist(Map(
            f = function(x, eval) {
                if (isFALSE(eval)) {
                    return(FALSE)
                }
                is.atomic(x)
            },
            x = x,
            eval = lgl
        ))
        if (!any(lgl)) {
            return(x)
        }
        idx <- which(lgl)
        x[idx] <- lapply(X = x[idx], FUN = encode)
        x
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
    signature = signature(x = "Date"),
    definition = `encode,Date`
)

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
