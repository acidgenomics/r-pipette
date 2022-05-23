#' @name atomize
#' @inherit AcidGenerics::atomize
#'
#' @details
#' Keep only atomic columns. Complex columns won't write to disk as CSVs or work
#' with R Markdown functions, in some cases.
#'
#' @note Updated 2021-10-12.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' df <- S4Vectors::DataFrame(
#'     "a" = "a",
#'     "b" = I(list("a" = seq_len(3L)))
#' )
#' lapply(df, is.atomic)
#' x <- atomize(df)
NULL



## Updated 2019-07-19.
`atomize,DataFrame` <- ## nolint
    function(object) {
        object <- decode(object)
        object <- as.data.frame(object)
        object <- atomize(object)
        object <- as(object, "DataFrame")
        object
    }



## Updated 2019-07-21.
`atomize,Ranges` <- # nolint
    function(object) {
        mcols(object) <- atomize(mcols(object))
        object
    }



## Updated 2021-08-24.
`atomize,data.frame` <- # nolint
    function(object) {
        keep <- vapply(X = object, FUN = is.atomic, FUN.VALUE = logical(1L))
        assert(hasLength(keep))
        drop <- names(keep)[!keep]
        if (hasLength(drop)) {
            alertInfo(sprintf(
                "Dropping %s non-atomic %s: %s.",
                length(drop),
                ngettext(
                    n = length(drop),
                    msg1 = "column",
                    msg2 = "columns"
                ),
                toInlineString(drop, n = 10L)
            ))
        }
        object[, keep, drop = FALSE]
    }



#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature(object = "DataFrame"),
    definition = `atomize,DataFrame`
)

#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature(object = "Ranges"),
    definition = `atomize,Ranges`
)

#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature(object = "data.frame"),
    definition = `atomize,data.frame`
)
