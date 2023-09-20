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



## FIXME Need to rework to not construct the whole data frame...
## figure out where we need to apply first.
## Can optimize by checking to see if we need to apply first.

## Updated 2023-09-20.
`sanitizeNA,DFrame` <- # nolint
    function(object) {
        if (!(hasCols(object) && hasRows(object))) {
            return(object)
        }
        lst <- lapply(
            X = object,
            FUN = function(x) {
                if (is.character(x)) {
                    sanitizeNA(x)
                } else {
                    x
                }
            }
        )
        out <- as.DataFrame(lst)
        dimnames(out) <- dimnames(object)
        metadata(out) <- metadata(object)
        out
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



## FIXME Rework to use the same code as DFrame.
## Updated 2021-06-09.
`sanitizeNA,data.frame` <- # nolint
    function(object) {
        if (!(hasCols(object) && hasRows(object))) {
            return(object)
        }
        assert(allAreAtomic(object))
        if (hasRownames(object)) {
            rownames <- rownames(object)
        } else {
            rownames <- NULL
        }
        list <- lapply(
            X = object,
            FUN = function(x) {
                if (is.character(x)) {
                    sanitizeNA(x)
                } else {
                    x
                }
            }
        )
        out <- data.frame(
            list,
            row.names = rownames,
            stringsAsFactors = FALSE
        )
        ## This step ensures we keep `tbl_df`, `data.table` class, if necessary.
        if (!identical(class(object), "data.frame")) {
            out <- as(out, class(object)[[1L]])
        }
        out
    }



## Don't use `as.factor()` and then reset levels using a separate `levels()`
## call here. It can cause a single value to flip to the first element in the
## levels vector.
##
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
