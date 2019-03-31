#' @name factorize
#' @inherit bioverbs::factorize
#' @inheritParams params
#' @examples
#' df <- S4Vectors::DataFrame(a = letters[seq_len(5L)], b = seq_len(5L))
#' x <- factorize(df)
NULL



#' @name factorize
#' @importFrom bioverbs factorize
#' @export
NULL



factorize.ANY <-  # nolint
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
    signature = signature("ANY"),
    definition = factorize.ANY
)
