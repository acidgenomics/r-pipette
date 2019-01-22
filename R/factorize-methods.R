#' @name factorize
#' @inherit bioverbs::factorize
#' @inheritParams params
#' @examples
#' df <- S4Vectors::DataFrame(a = letters[seq_len(5)], b = seq_len(5))
#' x <- factorize(df)
NULL



#' @importFrom bioverbs factorize
#' @aliases NULL
#' @export
bioverbs::factorize



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
