#' Drop factor levels
#'
#' @name droplevels2
#' @inherit AcidGenerics::droplevels2
#' @note Updated 2022-04-25.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(GenomicRanges, package = "AcidTest")
#'
#' ## Ranges ====
#' object <- GenomicRanges
#' object <- droplevels2(object)
#' print(object)
NULL



## Updated 2022-04-25.
`droplevels2,DataFrame` <-  # nolint
    function(x) {
        except <- !bapply(X = decode(x), FUN = is.factor)
        if (all(except)) {
            return(x)
        }
        lst <- as(x, "List")
        lst <- droplevels(x = lst, except = except)
        out <- as.DataFrame(x = lst, row.names = rownames(x))
        out
    }



## Updated 2021-02-03.
`droplevels2,Ranges` <-  # nolint
    function(x) {
        if (hasCols(mcols(x))) {
            mcols(x) <- droplevels2(mcols(x))
        }
        x
    }



#' @rdname droplevels2
setMethod(
    f = "droplevels2",
    signature = signature(x = "DataFrame"),
    definition = `droplevels2,DataFrame`
)

#' @rdname droplevels2
setMethod(
    f = "droplevels2",
    signature = signature(x = "Ranges"),
    definition = `droplevels2,Ranges`
)
