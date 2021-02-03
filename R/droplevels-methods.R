#' Drop unused levels from factors
#'
#' @name droplevels
#' @inherit base::droplevels description
#' @note Updated 2021-02-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#'
#' @examples
#' data(GRanges, package = "AcidTest")
#'
#' ## Ranges ====
#' object <- GRanges
#' object <- S4Vectors::droplevels(object)
#' print(object)
NULL



## Updated 2021-02-03.
`droplevels,DataFrame` <-  # nolint
    function(x) {
        except <- !bapply(X = decode(x), FUN = is.factor)
        if (all(except)) return(x)
        x <- droplevels(x = x, except = except)
        x
    }



#' @rdname droplevels
setMethod(
    f = "droplevels",
    signature = signature("DataFrame"),
    definition = `droplevels,DataFrame`
)



## Updated 2021-02-03.
`droplevels,Ranges` <-  # nolint
    function(x) {
        if (hasCols(mcols(x))) {
            mcols(x) <- droplevels(mcols(x))
        }
        x
    }



#' @rdname droplevels
setMethod(
    f = "droplevels",
    signature = signature("Ranges"),
    definition = `droplevels,Ranges`
)
