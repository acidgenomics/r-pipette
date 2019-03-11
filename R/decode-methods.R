#' Decode column data that uses run-length encoding
#'
#' @name decode
#' @inherit S4Vectors::decode description return
#' @inheritParams params
#'
#' @return Modified object.
#' Columns will be decoded and no longer `Rle` class.
#'
#' @seealso `S4Vectors::decode()`.
#'
#' @examples
#' load(system.file("extdata", "rse.rda", package = "transformer"))
#' mcols <- S4Vectors::mcols(rse)
#' lapply(mcols, class)
#' x <- decode(mcols)
#' lapply(x, class)
NULL



#' @name decode
#' @importFrom S4Vectors decode
#' @aliases NULL
#' @export
NULL



decode.DataFrame <-  # nolint
    function(x) {
        DataFrame(
            lapply(
                X = x,
                FUN = function(x) {
                    if (is(x, "Rle")) {
                        x <- decode(x)
                        if (is.factor(x)) {
                            x <- droplevels(x)
                        }
                        x
                    } else if (!is.atomic(x)) {
                        I(x)
                    } else {
                        x
                    }
                }
            ),
            row.names = rownames(x)
        )
    }



#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature("DataFrame"),
    definition = decode.DataFrame
)



decode.GRanges <-  # nolint
    function(x) {
        mcols <- mcols(x)
        assert(is(mcols, "DataFrame"))
        mcols <- decode(mcols)
        mcols(x) <- mcols
        x
    }



#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature("GRanges"),
    definition = decode.GRanges
)
