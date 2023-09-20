#' @name unfactorize
#' @inherit AcidGenerics::unfactorize
#' @note Updated 2023-09-20.
#'
#' @return Modified object, with `factor` coerced back to atomic vector.
#'
#' @seealso
#' - Warning section of `help("factor")`.
#' - R FAQ 7.10: How do I convert factors to numeric?
#' - https://stackoverflow.com/questions/3418128/
#' - forcats package.
#'
#' @examples
#' ## factor ====
#' object <- as.factor(c(100L, 100L, 101L, 101L))
#' ## Coercion with `as.numeric` or `as.integer` don't work the way we want.
#' print(as.numeric(object))
#' print(as.integer(object))
NULL



## Using the R FAQ 7.10 recommended approach here.
## Updated 2023-09-20.
`unfactorize,factor` <- # nolint
    function(object) {
        idx <- as.integer(object)
        lvl <- levels(object)
        if (all(grepl(pattern = "^[0-9]+", x = lvl))) {
            lvl <- as.integer(lvl)
        } else if (grepl(pattern = "^[0-9.]+", x = lvl)) {
            lvl <- as.numeric(lvl)
        }
        out <- lvl[idx]
        out
    }



## Updated 2023-09-20.
`unfactorize,data.frame` <- # nolint
    function(object) {
        lst <- lapply(
            X = object,
            FUN = function(x) {
                if (is.factor(x)) {
                    unfactorize(x)
                } else {
                    x
                }
            }
        )
        df <- as.data.frame(lst)
        dimnames(df) <- dimnames(object)
        df
    }



## Updated 2023-09-20.
`unfactorize,DFrame` <- # nolint
    function(object) {
        lst <- lapply(
            X = object,
            FUN = function(x) {
                if (is.factor(x)) {
                    unfactorize(x)
                } else {
                    x
                }
            }
        )
        df <- as.DataFrame(lst)
        dimnames(df) <- dimnames(object)
        df
    }



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
