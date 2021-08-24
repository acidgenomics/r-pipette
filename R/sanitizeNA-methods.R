#' @name sanitizeNA
#' @inherit AcidGenerics::sanitizeNA
#' @note Updated 2021-02-11.
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
#' ## DataFrame ====
#' from <- DataFrame(
#'     a = c("foo", ""),
#'     b = c(NA, "bar"),
#'     row.names = c("c", "d")
#' )
#' print(from)
#' to <- sanitizeNA(from)
#' print(to)
NULL



## Updated 2019-07-19.
`sanitizeNA,atomic` <-  # nolint
    function(object) {
        object
    }


#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("atomic"),
    definition = `sanitizeNA,atomic`
)



## Note that names will be kept here after the gsub call.
## Updated 2021-01-13.
`sanitizeNA,character` <-  # nolint
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



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("character"),
    definition = `sanitizeNA,character`
)



## Don't use `as.factor()` and then reset levels using a separate `levels()`
## call here. It can cause a single value to flip to the first element in the
## levels vector.
##
## Updated 2021-08-05.
`sanitizeNA,factor` <-  # nolint
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
    signature = signature("factor"),
    definition = `sanitizeNA,factor`
)



## Updated 2021-06-09.
`sanitizeNA,data.frame` <-  # nolint
    function(object) {
        if (!(hasCols(object) && hasRows(object))) {
            return(object)  # nocov
        }
        assert(allAreAtomic(object))
        if (hasRownames(object)) {
            rownames <- rownames(object)  # nocov
        } else {
            rownames <- NULL
        }
        list <- lapply(
            X = object,
            FUN = function(x) {
                if (is.character(x)) {
                    sanitizeNA(x)
                } else {
                    x  # nocov
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
            out <- as(out, class(object)[[1L]])  # nocov
        }
        out
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("data.frame"),
    definition = `sanitizeNA,data.frame`
)



## Updated 2021-06-09.
`sanitizeNA,DataFrame` <-  # nolint
    function(object) {
        if (!(hasCols(object) && hasRows(object))) {
            return(object)
        }
        meta <- metadata(object)
        rn <- rownames(object)
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
        out <- as.DataFrame(list)
        rownames(out) <- rn
        metadata(out) <- meta
        out
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("DataFrame"),
    definition = `sanitizeNA,DataFrame`
)
