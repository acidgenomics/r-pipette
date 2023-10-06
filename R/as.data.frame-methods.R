#' Coerce object to data.frame
#'
#' @name as.data.frame
#' @note Updated 2023-10-06.
#'
#' @inheritParams AcidRoxygen::params
#' @param row.names,optional
#' Refer to `base::data.frame` for usage details.
#'
#' @return `data.frame`.
#'
#' @examples
#' data(
#'     IRanges,
#'     sparseMatrix,
#'     package = "AcidTest"
#' )
#'
#' ## `IRanges` to `data.frame` ====
#' from <- IRanges
#' to <- as.data.frame(from)
#' head(to)
#'
#' ## `Matrix` to `data.frame` ====
#' from <- sparseMatrix
#' to <- as.data.frame(from)
#' head(to)
NULL



#' Coerce an S4 `DFrame` to a standard `data.frame`.
#'
#' @note Updated 2023-04-26.
#' @noRd
#'
#' @details
#' This function will return an informative error if an S4 `DFrame` contains
#' complex columns that can't be coerced to atomic or list.
#'
#' Not exporting this method because we don't want to mask the default
#' conventions currently used by Bioconductor.
`as.data.frame,DFrame` <- # nolint
    function(x) {
        ## Decode Rle columns, which can be coerced.
        x <- decode(x)
        ## Check for valid columns (atomic, list).
        valid <- vapply(
            X = x,
            FUN = function(x) {
                is.atomic(x) || is.list(x)
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = TRUE
        )
        ## Error if S4 columns are nested.
        if (!all(valid)) {
            invalid <- x[, names(valid[!valid]), drop = FALSE]
            invalid <- vapply(
                X = invalid,
                FUN = class,
                FUN.VALUE = character(1L)
            )
            invalid <- sprintf("%s (%s)", names(invalid), invalid)
            abort(sprintf(
                fmt = paste(
                    "Only atomic and list columns are supported.",
                    "Invalid columns: %s.",
                    sep = "\n"
                ),
                toInlineString(invalid, n = 10L)
            ))
        }
        as.data.frame(x, optional = TRUE)
    }



## Updated 2023-10-06.
`as.data.frame,IRanges` <- # nolint
    function(x,
             row.names = NULL, # nolint
             optional = FALSE,
             ...) {
        dfArgs <- list(
            "start" = start(x),
            "end" = end(x),
            "width" = width(x),
            "row.names" = row.names,
            "check.rows" = !optional,
            "check.names" = !optional,
            "stringsAsFactors" = FALSE
        )
        mcols <- mcols(x, use.names = FALSE)
        if (!is.null(mcols)) {
            dfArgs[["mcols"]] <- as.data.frame(mcols, optional = optional)
        }
        do.call(what = data.frame, args = dfArgs)
    }



## Updated 2023-10-06.
`as.data.frame,Matrix` <- # nolint
    function(x,
             row.names = NULL, # nolint
             optional = FALSE,
             ...) {
        as.data.frame(
            x = as.matrix(x),
            row.names = row.names,
            optional = optional,
            ...
        )
    }



#' @rdname as.data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature(x = "IRanges"),
    definition = `as.data.frame,IRanges`
)

#' @rdname as.data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature(x = "Matrix"),
    definition = `as.data.frame,Matrix`
)
