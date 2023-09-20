#' @name unfactorize
#' @inherit AcidGenerics::unfactorize
#' @note Updated 2023-09-20.
#'
#' @param j `vector`.
#' Column names or positions to evaluate.
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
#' ## data.frame ====
#' object <- data.frame(
#'     "a" = as.factor(c(100L, 100L, 101L, 101L)),
#'     "b" = as.factor(c("a", "b", "a", "b"))
#' )
#' print(object)
#' object <- unfactorize(object)
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
        out
    }



## Updated 2023-09-20.
`unfactorize,data.frame` <- # nolint
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
            assert(length(j) <= ncol(object))
            idx <- seq(from = 1L, to = ncol(object))
            lgl <- idx %in% j
        }
        assert(
            is.logical(lgl),
            hasLength(lgl, n = ncol(object))
        )
        lst <- Map(
            f = function(x, eval) {
                if (isFALSE(eval)) {
                    return(x)
                }
                if (!is.factor(x)) {
                    return(x)
                }
                unfactorize(x)
            },
            x = object,
            eval = lgl
        )
        if (is(object, "DFrame")) {
            df <- as.DataFrame(lst)
        } else {
            df <- as.data.frame(lst)
        }
        dimnames(df) <- dimnames(object)
        df
    }



## Updated 2023-09-20.
`unfactorize,DFrame` <- # nolint
    `unfactorize,data.frame`



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
    signature = signature(object = "data.frame"),
    definition = `unfactorize,data.frame`
)

#' @export
#' @rdname unfactorize
setMethod(
    f = "unfactorize",
    signature = signature(object = "DFrame"),
    definition = `unfactorize,DFrame`
)
