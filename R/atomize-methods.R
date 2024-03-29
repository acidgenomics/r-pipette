#' @name atomize
#' @inherit AcidGenerics::atomize
#'
#' @details
#' Keep only atomic columns. Complex columns won't write to disk as CSVs or work
#' with R Markdown functions, in some cases.
#'
#' @note Updated 2022-05-25.
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



## Updated 2023-10-06.
`atomize,DFrame` <- ## nolint
    function(object) {
        if (!hasLength(object)) {
            return(object)
        }
        rn <- rownames(object)
        object <- decode(object)
        object <- as.data.frame(object, optional = TRUE)
        object <- atomize(object)
        object <- as(object, "DFrame")
        rownames(object) <- rn
        object
    }



## Updated 2022-05-25.
`atomize,Ranges` <- # nolint
    function(object) {
        if (!hasLength(object)) {
            return(object)
        }
        mcols(object) <- atomize(mcols(object))
        object
    }



## Updated 2022-05-25.
`atomize,data.frame` <- # nolint
    function(object) {
        if (!hasLength(object)) {
            return(object)
        }
        keep <- vapply(X = object, FUN = is.atomic, FUN.VALUE = logical(1L))
        if (all(keep)) {
            return(object)
        }
        if (!any(keep)) {
            return(data.frame(row.names = rownames(object)))
        }
        drop <- names(keep)[!keep]
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
        object <- object[, keep, drop = FALSE]
        object
    }



#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature(object = "DFrame"),
    definition = `atomize,DFrame`
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
