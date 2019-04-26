#' @name atomize
#' @inherit bioverbs::atomize
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' df <- S4Vectors::DataFrame(a = "a", b = list(a = seq_len(3)))
#' lapply(df, is.atomic)
#' x <- atomize(df)
NULL



#' @rdname atomize
#' @name atomize
#' @importFrom bioverbs atomize
#' @usage atomize(object, ...)
#' @export
NULL



# @seealso
# - `base:::as.data.frame.matrix()`.
# - `S4Vectors:::as.data.frame.DataTable()`.
atomize.data.frame <-  # nolint
    function(object) {
        # Keep only atomic columns. Complex columns won't write to disk as CSVs
        # or work with R Markdown functions.
        keep <- vapply(X = object, FUN = is.atomic, FUN.VALUE = logical(1L))
        assert(hasLength(keep))

        # Inform the user about which columns to drop.
        drop <- names(keep)[!keep]
        if (hasLength(drop)) {
            message(paste(
                "Dropping non-atomic columns:",
                toString(drop, width = 200L)
            ))
        }

        object[, keep, drop = FALSE]
    }



#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature("data.frame"),
    definition = atomize.data.frame
)



atomize.DataFrame <-  # nolint
    function(object) {
        object <- decode(object)
        object <- as.data.frame(object)
        object <- atomize(object)
        object <- as(object, "DataFrame")
        object
    }



#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature("DataFrame"),
    definition = atomize.DataFrame
)



atomize.GRanges <-  # nolint
    function(object) {
        mcols(object) <- atomize(mcols(object))
        object
    }



#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature("GRanges"),
    definition = atomize.GRanges
)
