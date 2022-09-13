#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "ANY",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = function(object,
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
        if (missing(con)) {
            con <- NULL
        }
        if (missing(format)) {
            format <- NULL
        }
        assert(
            is.null(con),
            is.null(format),
            isString(dir)
        )
        call <- standardizeCall()
        sym <- call[["object"]]
        assert(is.symbol(sym), msg = .symError)
        name <- as.character(sym)
        con <- file.path(dir, paste0(name, ".", format))
        format <- .defaultFormat(object)
        args <- list(
            "object" = object,
            "con" = con,
            "format" = format
        )
        args <- append(x = args, values = dots)
        do.call(what = export, args = args)
    }
)



## Updated 2022-09-13.
.defaultFormat <- function(object) {
    if (is.character(object)) {
        key <- "character"
    }
    if (is(object, "Matrix")) {
        key <- "Matrix"
    } else {
        key <- "delim"
    }
    choices <- .exportFormatChoices
    format <- choices[[key]][[1L]]
    assert(isString(format))
    format
}





#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "ANY",
        con = "missingOrNULL",
        format = "character"
    ),
    definition = function(object, con, format) {
        stop("FIXME NOT SUPPORTED YET")
    }
)
