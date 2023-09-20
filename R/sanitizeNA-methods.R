## FIXME Rework to assign directly into the columns, similar to factorize.
## Then we can add simpler data.frame support here, rather than coercing...



#' @name sanitizeNA
#' @inherit AcidGenerics::sanitizeNA
#' @note Updated 2023-09-20.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' ## character ====
#' from <- as.character(c(1L, "x", "", "NA", "NULL"))
#' print(from)
#' to <- sanitizeNA(from)
#' print(to)
#'
#' ## DFrame ====
#' from <- S4Vectors::DataFrame(
#'     "a" = c("foo", ""),
#'     "b" = c(NA, "bar"),
#'     row.names = c("c", "d")
#' )
#' print(from)
#' to <- sanitizeNA(from)
#' print(to)
NULL



## Updated 2023-09-20.
`sanitizeNA,DFrame` <- # nolint
    function(object) {
        if (!(hasCols(object) && hasRows(object))) {
            return(object)
        }
        lgl <- bapply(X = object, FUN = is.character)
        if (!any(lgl)) {
            return(object)
        }
        idx <- which(lgl)
        object[idx] <- lapply(X = object[idx], FUN = sanitizeNA)
        object
    }



## Updated 2019-07-19.
`sanitizeNA,atomic` <- # nolint
    function(object) {
        object
    }



## Note that names will be kept here after the gsub call.
## Updated 2021-01-13.
`sanitizeNA,character` <- # nolint
    function(object) {
        patterns <- c(
            "^$",
            "^\\s+$",
            "^n/a$",
            "^na$",
            "^none available$",
            "^null$"
        )
        gsub(
            pattern = paste(patterns, collapse = "|"),
            replacement = NA_character_,
            x = object,
            ignore.case = TRUE
        )
    }



## Updated 2023-09-20.
`sanitizeNA,data.frame` <- # nolint
    `sanitizeNA,DFrame`



## Don't use `as.factor()` and then reset levels using a separate `levels()`
## call here. It can cause a single value to flip to the first element in the
## levels vector.

## Updated 2021-08-05.
`sanitizeNA,factor` <- # nolint
    function(object) {
        x <- object
        levels <- unique(sanitizeNA(levels(x)))
        x <- as.character(x)
        x <- sanitizeNA(x)
        x <- factor(x = x, levels = levels)
        names(x) <- names(object)
        x
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature(object = "DFrame"),
    definition = `sanitizeNA,DFrame`
)

#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature(object = "atomic"),
    definition = `sanitizeNA,atomic`
)

#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature(object = "character"),
    definition = `sanitizeNA,character`
)

#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature(object = "data.frame"),
    definition = `sanitizeNA,data.frame`
)

#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature(object = "factor"),
    definition = `sanitizeNA,factor`
)
