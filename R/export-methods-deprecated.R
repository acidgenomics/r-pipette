## Deprecated S4 method exports ================================================

## Updated 2022-09-13.
`export,deprecated` <- function(object, con, format, ...) {
    stop("FIXME")
}


## FIXME Add back support for export without con or format definition!
## This is super useful for interactive export.
## FIXME Error if the user has defined "ext" or "file" here instead.
## Inform them of the updated argument names.


## FIXME Rethink this as clever approach for handling automatic name export.
## FIXME What if we don't set con but we DO set format?

## Updated 2021-10-19.
`export,character,deprecated` <- # nolint
    function(object,
             con, # missingOrNULL
             format, # missingOrNULL
             dir = getOption(
                 x = "acid.export.dir",
                 default = getwd()
             ),
             ...) {
        dots <- list(...)
        if (isSubset("file", names(dots))) {
            abort(sprintf(
                "Use {.arg %s} instead of {.arg %s}.",
                "con", "file"
            ))
        }
        if (isSubset("ext", names(dots))) {
            abort(sprintf(
                "Use {.arg %s} instead of {.arg %s}.",
                "format", "ext"
            ))
        }
        assert(isString(dir))
        if (missing(con)) {
            con <- NULL
        }
        if (missing(format)) {
            format <- NULL
        }
        ## Mode 1: neither `con` nor `format` are defined.
        if (is.null(con) && is.null(format)) {
        }
        ## Mode 2: `con` is defined.
        if (!is.null(con)) {
            assert(is.null(format))
        }
        ## Mode 3: `format` is defined.
        if (!is.null(format)) {
            assert(is.null(con))
        }


        if (is.null(con)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            assert(is.symbol(sym), msg = .symError)
            name <- as.character(sym)
            con <- file.path(dir, paste0(name, ".", format))
        }


        args <- list()
        args[["object"]] <- object
        if (!missing(con)) {
            args[["con"]] <- con
        }
        if (!missing(format)) {
            args[["format"]] <- format
        }
        args <- append(x = args, values = dots)
        do.call(what = export, args = args)
    }






#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DataFrame",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DataFrame",
        con = "missingOrNULL",
        format = "character"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GenomicRanges",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GenomicRanges",
        con = "missingOrNULL",
        format = "character"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Matrix",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Matrix",
        con = "missingOrNULL",
        format = "character"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "character",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "character",
        con = "missingOrNULL",
        format = "character"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "data.frame",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "data.frame",
        con = "missingOrNULL",
        format = "character"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "matrix",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,deprecated`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "matrix",
        con = "missingOrNULL",
        format = "character"
    ),
    definition = `export,deprecated`
)
