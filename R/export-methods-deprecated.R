## Updated 2022-09-13.
.defaultExt <- function(object) {
    if (is.character(object)) {
        key <- "character"
    }
    if (is(object, "Matrix")) {
        key <- "Matrix"
    } else {
        key <- "delim"
    }
    choices <- .exportFormatChoices
    ext <- choices[[key]][[1L]]
    assert(isString(ext))
    ext
}



## Updated 2022-09-13.
.exportFormat <- function(object, con, format) {
    abort(sprintf(
        "Use {.arg %s} instead of {.arg %s}.",
        "con", "format"
    ))
}



## Updated 2022-09-13.
.exportNSE <- function(object,
         con,
         format,
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
    ext <- .defaultExt(object)
    con <- file.path(dir, paste0(name, ".", ext))
    args <- list(
        "object" = object,
        "con" = con,
        "format" = format
    )
    args <- append(x = args, values = dots)
    do.call(what = export, args = args)
}



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "ANY",
        con = "missing",
        format = "missing"
    ),
    definition = .exportNSE
)



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "ANY",
        con = "missing",
        format = "character"
    ),
    definition = .exportFormat
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DataFrame",
        con = "missing",
        format = "character"
    ),
    definition = .exportFormat
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GenomicRanges",
        con = "missing",
        format = "character"
    ),
    definition = .exportFormat
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Matrix",
        con = "missing",
        format = "character"
    ),
    definition = .exportFormat
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "character",
        con = "missing",
        format = "character"
    ),
    definition = .exportFormat
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "data.frame",
        con = "missing",
        format = "character"
    ),
    definition = .exportFormat
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "matrix",
        con = "missing",
        format = "character"
    ),
    definition = .exportFormat
)
