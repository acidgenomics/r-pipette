#' @name factorize
#' @inherit AcidGenerics::factorize
#' @note Updated 2021-02-11.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' df <- DataFrame(a = letters[seq_len(5L)], b = seq_len(5L))
#' x <- factorize(df)
NULL



## Updated 2019-07-19.
`factorize,DFrame` <-  # nolint
    function(object) {
        class <- class(object)[[1L]]
        out <- lapply(
            X = object,
            FUN = function(x) {
                droplevels(as.factor(x))
            }
        )
        out <- as(out, Class = class)
        names(out) <- names(object)
        rownames <- rownames(object)
        if (!is.null(rownames)) {
            rownames(out) <- rownames
        }
        out
    }



#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature(object = "DFrame"),
    definition = `factorize,DFrame`
)
