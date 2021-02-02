#' Drop unused levels from factors
#'
#' @name droplevels
#' @inherit base::droplevels description
#' @note Updated 2021-02-02.
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
#' S4Vectors::droplevels(object)
NULL



`droplevels,Ranges` <-  # nolint
    function(x) {
        mcols <- mcols(x)
        if (hasCols(mcols)) {
            except <- !bapply(decode(mcols), is.factor)
            mcols <- droplevels(mcols, except = except)
            mcols(x) <- mcols
        }
        x
    }



#' @rdname droplevels
setMethod(
    f = "droplevels",
    signature = signature("Ranges"),
    definition = `droplevels,Ranges`
)
