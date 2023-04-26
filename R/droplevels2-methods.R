#' Drop factor levels
#'
#' @name droplevels2
#' @inherit AcidGenerics::droplevels2
#' @note Updated 2023-03-01.
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
NULL



## Updated 2023-04-26.
`droplevels2,DFrame` <- # nolint
    function(x) {
        except <- !bapply(
            X = x,
            FUN = function(x) {
                if (is(x, "Rle")) {
                    x <- decode(x)
                }
                is.factor(x)
            }
        )
        if (all(except)) {
            return(x)
        }
        lst <- SimpleList(as.list(x))
        lst <- droplevels(x = lst, except = except)
        out <- as.DataFrame(lst)
        rownames(out) <- rownames(x)
        out
    }



## Updated 2021-02-03.
`droplevels2,Ranges` <- # nolint
    function(x) {
        if (hasCols(mcols(x))) {
            mcols(x) <- droplevels2(mcols(x))
        }
        x
    }



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
