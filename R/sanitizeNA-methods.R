#' @name sanitizeNA
#' @inherit AcidGenerics::sanitizeNA
#' @note Updated 2020-10-05.
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
#' from <- S4Vectors::DataFrame(
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
## Updated 2020-09-14.
`sanitizeNA,character` <-  # nolint
    function(object) {
        patterns <- c(
            "^$",
            "^N/A$",
            "^NA$",
            "^NULL$",
            "^\\s+$",
            "^none available$"
        )
        gsub(
            pattern = paste(patterns, collapse = "|"),
            replacement = NA_character_,
            x = object
        )
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("character"),
    definition = `sanitizeNA,character`
)



## Updated 2019-07-19.
`sanitizeNA,factor` <-  # nolint
    function(object) {
        x <- as.character(object)
        x <- sanitizeNA(x)
        x <- as.factor(x)
        levels(x) <- unique(sanitizeNA(levels(object)))
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



## nolint start
##
## Alternate dplyr method:
## > object <- mutate_if(object, is.character, sanitizeNA)
##
## This requires use to import dplyr, which can be otherwise avoided.
##
## nolint end

## Updated 2019-07-19.
`sanitizeNA,data.frame` <-  # nolint
    function(object) {
        if (hasRownames(object)) {
            rownames <- rownames(object)  # nocov
        } else {
            rownames <- NULL
        }
        list <- lapply(
            X = object,
            FUN = function(col) {
                if (is.character(col)) {
                    sanitizeNA(col)
                } else {
                    I(col)  # nocov
                }
            }
        )
        out <- data.frame(list, row.names = rownames, stringsAsFactors = FALSE)
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



## Updated 2019-07-19.
`sanitizeNA,DataFrame` <-  # nolint
    function(object) {
        rownames <- rownames(object)
        ## nocov start
        list <- lapply(
            X = object,
            FUN = function(x) {
                if (is.character(x)) {
                    sanitizeNA(x)
                } else if (isS4(x) || is(x, "AsIs") || !is.atomic(x))  {
                    I(x)
                } else {
                    x
                }
            }
        )
        ## nocov end
        DataFrame(list, row.names = rownames)
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("DataFrame"),
    definition = `sanitizeNA,DataFrame`
)
