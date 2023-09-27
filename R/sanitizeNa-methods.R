#' @name sanitizeNa
#' @inherit AcidGenerics::sanitizeNa
#' @note Updated 2023-09-20.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' ## character ====
#' from <- as.character(c(1L, "x", "", "NA", "NULL"))
#' print(from)
#' to <- sanitizeNa(from)
#' print(to)
#'
#' ## DFrame ====
#' from <- S4Vectors::DataFrame(
#'     "a" = c("foo", ""),
#'     "b" = c(NA, "bar"),
#'     row.names = c("c", "d")
#' )
#' print(from)
#' to <- sanitizeNa(from)
#' print(to)
NULL



## Updated 2023-09-20.
`sanitizeNa,DFrame` <- # nolint
    function(object) {
        if (!(hasCols(object) && hasRows(object))) {
            return(object)
        }
        lgl <- bapply(X = object, FUN = is.character)
        if (!any(lgl)) {
            return(object)
        }
        idx <- which(lgl)
        object[idx] <- lapply(X = object[idx], FUN = sanitizeNa)
        object
    }



## Updated 2019-07-19.
`sanitizeNa,atomic` <- # nolint
    function(object) {
        object
    }



## Note that names will be kept here after the gsub call.
## Updated 2021-01-13.
`sanitizeNa,character` <- # nolint
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
`sanitizeNa,data.frame` <- # nolint
    `sanitizeNa,DFrame`



## Don't use `as.factor()` and then reset levels using a separate `levels()`
## call here. It can cause a single value to flip to the first element in the
## levels vector.

## Updated 2021-08-05.
`sanitizeNa,factor` <- # nolint
    function(object) {
        x <- object
        levels <- unique(sanitizeNa(levels(x)))
        x <- as.character(x)
        x <- sanitizeNa(x)
        x <- factor(x = x, levels = levels)
        names(x) <- names(object)
        x
    }



#' @rdname sanitizeNa
#' @export
setMethod(
    f = "sanitizeNa",
    signature = signature(object = "DFrame"),
    definition = `sanitizeNa,DFrame`
)

#' @rdname sanitizeNa
#' @export
setMethod(
    f = "sanitizeNa",
    signature = signature(object = "atomic"),
    definition = `sanitizeNa,atomic`
)

#' @rdname sanitizeNa
#' @export
setMethod(
    f = "sanitizeNa",
    signature = signature(object = "character"),
    definition = `sanitizeNa,character`
)

#' @rdname sanitizeNa
#' @export
setMethod(
    f = "sanitizeNa",
    signature = signature(object = "data.frame"),
    definition = `sanitizeNa,data.frame`
)

#' @rdname sanitizeNa
#' @export
setMethod(
    f = "sanitizeNa",
    signature = signature(object = "factor"),
    definition = `sanitizeNa,factor`
)
