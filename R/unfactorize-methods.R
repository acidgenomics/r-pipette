#' @name unfactorize
#' @inherit AcidGenerics::unfactorize
#' @note Updated 2023-09-20.
#'
#' @param j `vector`.
#' Column names or positions to evaluate.
#'
#' @param ... Additional arguments.
#'
#' @return Modified object, with `factor` coerced back to atomic vector.
#'
#' @seealso
#' - Warning section of `help("factor")`.
#' - R FAQ 7.10: How do I convert factors to numeric?
#' - `stringsAsFactors` for `read.table` and `data.frame`.
#' - https://stackoverflow.com/questions/3418128/
#' - forcats package.
#'
#' @examples
#' ## factor ====
#' object <- as.factor(c(100L, 100L, 101L, 101L))
#' ## Coercion with `as.numeric` or `as.integer` don't work the way we want.
#' print(as.numeric(object))
#' print(as.integer(object))
#' object <- factorize(object)
#' print(object)
#'
#' ## DFrame ====
#' object <- S4Vectors::DataFrame(
#'     "a" = as.factor(c(100L, 100L, 101L, 101L)),
#'     "b" = as.factor(c("a", "b", "a", "b"))
#' )
#' print(object)
#' object <- unfactorize(object)
#' print(object)
NULL



## Using the R FAQ 7.10 recommended approach here.
## Updated 2023-09-20.
`unfactorize,factor` <- # nolint
    function(object) {
        idx <- as.integer(object)
        lvl <- levels(object)
        if (all(grepl(pattern = "^[0-9]+$", x = lvl))) {
            lvl <- as.integer(lvl)
        } else if (all(grepl(pattern = "^[0-9.]+$", x = lvl))) {
            lvl <- as.numeric(lvl)
        }
        out <- lvl[idx]
        names(out) <- names(object)
        out
    }



## Updated 2023-09-20.
`unfactorize,DFrame` <- # nolint
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
        lgl <- unlist(Map(
            f = function(x, eval) {
                if (isFALSE(eval)) {
                    return(FALSE)
                }
                is.factor(x)
            },
            x = object,
            eval = lgl
        ))
        if (!any(lgl)) {
            return(object)
        }
        idx <- which(lgl)
        object[idx] <- lapply(X = object[idx], FUN = unfactorize)
        object
    }



## Updated 2021-10-14.
`unfactorize,data.frame` <- # nolint
    `unfactorize,DFrame`



#' @export
#' @rdname unfactorize
setMethod(
    f = "unfactorize",
    signature = signature(object = "factor"),
    definition = `unfactorize,factor`
)

#' @export
#' @rdname unfactorize
setMethod(
    f = "unfactorize",
    signature = signature(object = "DFrame"),
    definition = `unfactorize,DFrame`
)

#' @export
#' @rdname unfactorize
setMethod(
    f = "unfactorize",
    signature = signature(object = "data.frame"),
    definition = `unfactorize,data.frame`
)
