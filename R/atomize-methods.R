#' @name atomize
#' @inherit AcidGenerics::atomize
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' df <- S4Vectors::DataFrame(a = "a", b = list(a = seq_len(3)))
#' lapply(df, is.atomic)
#' x <- atomize(df)
NULL



## Keep only atomic columns. Complex columns won't write to disk as CSVs or work
## with R Markdown functions.
##
## Updated 2019-08-11.
`atomize,data.frame` <-  # nolint
    function(object) {
        keep <- vapply(X = object, FUN = is.atomic, FUN.VALUE = logical(1L))
        assert(hasLength(keep))
        drop <- names(keep)[!keep]
        if (hasLength(drop)) {
            message(sprintf(
                "Dropping non-atomic columns: %s.",
                toString(drop, width = 100L)
            ))
        }
        object[, keep, drop = FALSE]
    }



#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature("data.frame"),
    definition = `atomize,data.frame`
)



## Updated 2019-07-19.
`atomize,DataFrame` <-  ## nolint
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
    definition = `atomize,DataFrame`
)



## Updated 2019-07-21.
`atomize,Ranges` <-  # nolint
    function(object) {
        mcols(object) <- atomize(mcols(object))
        object
    }



#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature("Ranges"),
    definition = `atomize,Ranges`
)
