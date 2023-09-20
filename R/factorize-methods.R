#' @name factorize
#' @inherit AcidGenerics::factorize
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
#' - Legacy `stringsAsFactors` approach for `data.frame` import.
#'
#' @examples
#' ## DFrame ====
#' object <- S4Vectors::DataFrame(
#'     "a" = c("a", "b", "c", "d"),
#'     "b" = c("a", "a", "b", "b"),
#'     "c" = c(1L, 2L, 3L, 4L),
#'     "d" = c(1L, 2L, 1L, 2L),
#'     "e" = c(TRUE, TRUE, FALSE, FALSE),
#'     row.names = c("A", "B", "C", "D")
#' )
#' object <- factorize(object)
#' print(object)
NULL



## Updated 2023-09-20.
`factorize,DFrame` <- # nolint
    function(object, j = NULL) {
        assert(is.null(j) || is.vector(j))
        if (is.null(j)) {
            if (!(hasCols(object) && hasRows(object))) {
                return(object)
            }
            lgl <- rep(x = TRUE, times = ncol(object))
        } else if (is.character(j)) {
            assert(
                hasColnames(object),
                isSubset(j, colnames(object))
            )
            lgl <- colnames(object) %in% j
        } else {
            assert(
                allAreIntegerish(j),
                length(j) <= ncol(object)
            )
            idx <- seq(from = 1L, to = ncol(object))
            lgl <- idx %in% j
        }
        lgl <- Map(
            f = function(x, eval) {
                if (isFALSE(eval)) {
                    return(FALSE)
                }
                if (is.factor(x)) {
                    return(TRUE)
                }
                if (isS4(x) || !is.atomic(x) || is.logical(x)) {
                    return(FALSE)
                }
                ok <- hasDuplicates(na.omit(x))
                ok
            },
            x = object,
            eval = lgl
        )
        lgl <- unlist(x = lgl, recursive = FALSE, use.names = FALSE)
        if (!any(lgl)) {
            return(object)
        }
        idx <- which(lgl)
        object[idx] <- lapply(X = object[idx], FUN = factorize)
        object
    }



## Updated 2023-09-20.
`factorize,atomic` <- # nolint
    function(object) {
        out <- as.factor(object)
        names(out) <- names(object)
        out
    }



## Updated 2021-10-14.
`factorize,data.frame` <- # nolint
    `factorize,DFrame`



## Updated 2023-09-20.
`factorize,factor` <- # nolint
    function(object) {
        droplevels(object)
    }



#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature(object = "DFrame"),
    definition = `factorize,DFrame`
)

#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature(object = "atomic"),
    definition = `factorize,atomic`
)

#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature(object = "data.frame"),
    definition = `factorize,data.frame`
)

#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature(object = "factor"),
    definition = `factorize,factor`
)
