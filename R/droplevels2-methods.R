#' Drop factor levels
#'
#' @name droplevels2
#' @inherit AcidGenerics::droplevels2
#' @note Updated 2023-09-12.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(GRanges, package = "AcidTest")
#'
#' ## Ranges ====
#' object <- GRanges
#' object <- droplevels2(object)
#' print(object)
#'
#' ## Rle ====
#' object <- Rle(factor(c("a", "a", "b", "b"), levels = c("a", "b", "c")))
#' print(object)
#' object <- droplevels2(object)
#' print(object)
#'
#' ## factor ====
#' object <- factor(c("a", "a", "b", "b"), levels = c("a", "b", "c"))
#' print(object)
#' object <- droplevels2(object)
#' print(object)
NULL



## Updated 2023-09-12.
`droplevels2,DFrame` <- # nolint
    function(x) {
        if (!(hasCols(x) && hasRows(x))) {
            return(x)
        }
        lgl <- bapply(X = x, FUN = isAny, classes = c("factor", "Rle"))
        if (!any(lgl)) {
            return(x)
        }
        idx <- which(lgl)
        x[idx] <- lapply(X = x[idx], FUN = droplevels2)
        x
    }



## Updated 2021-02-03.
`droplevels2,Ranges` <- # nolint
    function(x) {
        if (hasCols(mcols(x))) {
            mcols(x) <- droplevels2(mcols(x))
        }
        x
    }



## Updated 2023-09-20.
`droplevels2,Rle` <- # nolint
    function(x) {
        if (!is.factor(runValue(x))) {
            return(x)
        }
        droplevels(x)
    }



## Updated 2023-09-20.
`droplevels2,factor` <- # nolint
    function(x) {
        droplevels(x)
    }



## Updated 2023-09-20.
`droplevels2,data.frame` <- # nolint
    `droplevels2,DFrame`



#' @rdname droplevels2
setMethod(
    f = "droplevels2",
    signature = signature(x = "DFrame"),
    definition = `droplevels2,DFrame`
)

#' @rdname droplevels2
setMethod(
    f = "droplevels2",
    signature = signature(x = "Ranges"),
    definition = `droplevels2,Ranges`
)

#' @rdname droplevels2
setMethod(
    f = "droplevels2",
    signature = signature(x = "Rle"),
    definition = `droplevels2,Rle`
)

#' @rdname droplevels2
setMethod(
    f = "droplevels2",
    signature = signature(x = "data.frame"),
    definition = `droplevels2,data.frame`
)

#' @rdname droplevels2
setMethod(
    f = "droplevels2",
    signature = signature(x = "factor"),
    definition = `droplevels2,factor`
)
