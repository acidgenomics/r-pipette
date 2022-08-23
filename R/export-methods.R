#' Export
#'
#' @name export
#' @note Updated 2022-08-23.
#'
#' @section Output file format extension:
#'
#' `matrix` supported arguments:
#'
#' - Comma separated values (CSV):
#' `"csv"`, `"csv.bz2"`, `"csv.gz"`, `"csv.xz"`, `"csv.zip"`.
#' - Tab separated values (TSV):
#' `"tsv"`, `"tsv.bz2"`, `"tsv.gz"`, `"tsv.xz"`, `"tsv.zip"`.
#'
#' `Matrix` (`sparseMatrix`) supported arguments:
#'
#' - MatrixMarket exchange (MTX):
#' `"mtx"`, `"mtx.bz2"`, `"mtx.gz"`, `"mtx.xz"`, `"mtx.zip"`.
#'
#' @section Row names:
#'
#' Some export utilities in R have a tendency to drop row names when writing to
#' disk in CSV format. For example, the [readr][] family of functions never
#' write row names by design. This is a *really poor* default setting for
#' handling genomic data, which often contain gene identifiers in the row names.
#' Here we're performing any internal tibble coercion step to ensure row names
#' are always moved to a `"rowname"` column in the CSV export.
#'
#' [readr]: https://readr.tidyverse.org/
#'
#' @section Debugging:
#'
#' Note that this function currently wraps `readr::write_delim()` by default
#' for exporting `DataFrame`, `data.frame`, and `matrix` class objects.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams import
#' @param ... Additional arguments.
#'
#' @param object Object.
#' An object supporting `dim()`, or a supported class capable of being coerced
#' to `data.frame`, to be written to disk.
#'
#' @param append `logical(1)`.
#' Append to output file.
#' When enabled, automatically sets `overwrite` argument to `FALSE`.
#' Requires readr package to be installed.
#'
#' @param con `character(1)`, `missing`, or `NULL`.
#' File path.
#' Alternatively, can leave unset and use `ext` and `dir` arguments instead.
#'
#' @param engine `character(1)`.
#' Engine (package) to use for export.
#'
#' Currently supported:
#' - base
#' - data.table
#' - readr
#'
#' @param format `character(1)`, `missing`, or `NULL`.
#' Output file format extension.
#'
#' @return Invisible `character`.
#' File path(s).
#'
#' @seealso
#' Packages:
#'
#' - [readr](https://readr.tidyverse.org).
#' - [data.table](https://r-datatable.com/).
#' - [rio](https://cran.r-project.org/package=rio).
#' - [rtracklayer](https://bioconductor.org/packages/rtracklayer/).
#' - [vroom](https://vroom.r-lib.org).
#'
#' Export functions:
#'
#' - `BiocIO::export()`.
#' - `data.table::fwrite()`.
#' - `readr::write_delim()`.
#' - `rio::export()`.
#' - `rtracklayer::export()`.
#' - `vroom::vroom_write()`.
#'
#' @examples
#' counts <- matrix(data = seq_len(100L), nrow = 10)
#' export(counts, ext = "csv")
#'
#' ## Clean up.
#' file.remove("counts.csv")
NULL



## Updated 2021-09-27.
.alertExport <- function(whatFile, whatPkg, whatFun) {
    assert(
        isString(whatFile),
        isString(whatPkg),
        isString(whatFun)
    )
    alert(sprintf(
        "Exporting {.file %s} using {.pkg %s}::{.fun %s}.",
        whatFile, whatPkg, whatFun
    ))
}



## Updated 2021-10-21.
.exportFormatChoices <- list(
    "Matrix" = c(
        "mtx.gz",
        "mtx.bz2",
        "mtx.xz",
        "mtx.zip",
        "mtx"
    ),
    "character" = c(
        "txt",
        "txt.bz2",
        "txt.gz",
        "txt.xz",
        "txt.zip"
    ),
    "delim" = c(
        "csv",
        "csv.bz2",
        "csv.gz",
        "csv.xz",
        "csv.zip",
        "tsv",
        "tsv.bz2",
        "tsv.gz",
        "tsv.xz",
        "tsv.zip"
    )
)



## Updated 2022-05-02.
`export,character` <- # nolint
    function(object,
             con,
             format,
             append = FALSE,
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             engine = getOption(
                 x = "acid.export.engine",
                 default = "base"
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             ),
             verbose = getOption(
                 x = "acid.verbose",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        assert(
            isString(con),
            is.null(format),
            isFlag(overwrite),
            isFlag(append),
            isString(engine),
            isFlag(quiet),
            isFlag(verbose)
        )
        if (isTRUE(verbose)) {
            assert(isFALSE(quiet))
        }
        whatPkg <- match.arg(arg = engine, choices = .engines)
        assert(requireNamespaces(whatPkg))
        if (isTRUE(append)) {
            assert(
                !identical(whatPkg, "base"),
                msg = sprintf(
                    "'%s' engine not supported when '%s' is enabled.",
                    "base", "append"
                )
            )
            overwrite <- FALSE
        }
        if (isTRUE(overwrite)) {
            assert(isFALSE(append))
        }
        whatFile <- con
        compressExt <- fileExt(path = con, pattern = compressExtPattern)
        compress <- !is.na(compressExt)
        if (isAFile(con)) {
            con <- realpath(con)
            if (isTRUE(append) && isFALSE(quiet)) {
                alertInfo(sprintf(
                    "Appending content in {.file %s}.",
                    basename(con)
                ))
            } else if (isTRUE(overwrite) && isFALSE(quiet)) {
                alertWarning(sprintf("Overwriting {.file %s}.", con))
            } else {
                abort(sprintf("File exists: {.file %s}.", con))
            }
        }
        if (isTRUE(compress)) {
            con <- sub(
                pattern = paste0("\\.", compressExt, "$"),
                replacement = "",
                x = con
            )
        }
        switch(
            EXPR = whatPkg,
            "base" = {
                whatFun <- "writeLines"
                args <- list(
                    "text" = object,
                    "con" = con
                )
            },
            "data.table" = {
                whatFun <- "fwrite"
                args <- list(
                    "x" = as.list(object),
                    "file" = con,
                    "append" = append,
                    "na" = "NA",
                    "quote" = FALSE,
                    "sep" = "\n",
                    "verbose" = verbose
                )
            },
            "readr" = {
                whatFun <- "write_lines"
                con <- normalizePath(
                    path = con,
                    winslash = "\\",
                    mustWork = FALSE
                )
                args <- list(
                    "x" = object,
                    "file" = con,
                    "append" = append,
                    "na" = "NA",
                    "sep" = "\n"
                )
            }
        )
        if (isFALSE(quiet)) {
            .alertExport(
                whatFile = whatFile,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        initDir(dirname(con))
        what <- get(x = whatFun, envir = asNamespace(whatPkg), inherits = TRUE)
        assert(is.function(what))
        do.call(what = what, args = args)
        if (isTRUE(compress)) {
            con <- compress(
                file = con,
                ext = compressExt,
                remove = TRUE,
                overwrite = TRUE
            )
        }
        con <- realpath(con)
        invisible(con)
    }



#' Export `matrix` method
#'
#' @note Updated 2022-05-31.
#' @noRd
#'
#' @details
#' This method covers standard `matrix` but is also intended to work for
#' `data.table`, `tbl_df`, and `DataFrame` classes. Note that `rio::export()`
#' doesn't preserve row names by default, so we're ensuring row names get
#' coerced to "rowname" column consistently here.
`export,data.frame` <- # nolint
    function(object,
             con,
             format,
             rownames = TRUE,
             colnames = TRUE,
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             engine = getOption(
                 x = "acid.export.engine",
                 default = "base"
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        validObject(object)
        object <- as.data.frame(object)
        verbose <- getOption(x = "acid.verbose", default = FALSE)
        ## Allowing export of empty objects, so don't check for length,
        ## rows, or columns here.
        assert(
            hasNoDuplicates(colnames(object)),
            isString(con),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(overwrite),
            isFlag(verbose),
            isFlag(quiet)
        )
        formatChoices <- .exportFormatChoices[["delim"]]
        if (missing(format)) {
            format <- fileExt(con) # nocov
        }
        format <- match.arg(arg = format, choices = formatChoices)
        whatPkg <- match.arg(arg = engine, choices = .engines)
        assert(requireNamespaces(whatPkg))
        file <- con
        whatFile <- con
        compressExt <- fileExt(path = file, pattern = compressExtPattern)
        compress <- !is.na(compressExt)
        ## Handle non-atomic columns (i.e. nested list columns).
        nonatomicCols <- which(!bapply(
            X = object,
            FUN = is.atomic,
            USE.NAMES = TRUE
        ))
        if (hasLength(nonatomicCols)) {
            ## Attempt to keep simple list columns and return reformatted as
            ## delimited character strings.
            listCols <- which(bapply(
                X = object,
                FUN = is.list,
                USE.NAMES = TRUE
            ))
            if (hasLength(listCols)) {
                for (listCol in listCols) {
                    x <- tryCatch(
                        expr = {
                            unlist(
                                x = lapply(
                                    X = object[[listCol]],
                                    FUN = toString
                                ),
                                recursive = FALSE,
                                use.names = FALSE
                            )
                        },
                        error = function(e) {
                            NULL # nocov
                        }
                    )
                    if (
                        is.vector(x) &&
                            identical(length(x), nrow(object))
                    ) {
                        object[[listCol]] <- x
                    }
                }
            }
            ## Discard any remaining non-atomic columns we can't coerce.
            keep <- bapply(X = object, FUN = is.atomic, USE.NAMES = TRUE)
            if (!all(keep)) {
                ## nocov start
                alertWarning(sprintf(
                    "Dropping non-atomic columns: %s.",
                    toInlineString(names(keep)[!keep], n = 10L)
                ))
                object <- object[, keep, drop = FALSE]
                ## nocov end
            }
        }
        if (isFALSE(rownames)) {
            rownames(object) <- NULL # nocov
        }
        if (hasRownames(object)) {
            assert(areDisjointSets("rowname", colnames(object)))
            object[["rowname"]] <- rownames(object)
            rownames(object) <- NULL
            object <- object[
                ,
                c("rowname", setdiff(colnames(object), "rowname")),
                drop = FALSE
            ]
        }
        if (isAFile(file)) {
            file <- realpath(file)
            if (isTRUE(overwrite)) {
                if (isFALSE(quiet)) {
                    alertWarning(sprintf("Overwriting {.file %s}.", file))
                }
                file.remove(file)
            } else {
                abort(sprintf("File exists: {.file %s}.", file))
            }
        }
        if (isTRUE(compress)) {
            file <- sub(
                pattern = paste0("\\.", compressExt, "$"),
                replacement = "",
                x = file
            )
            format <- match.arg(
                arg = fileExt(file),
                choices = c("csv", "tsv")
            )
        }
        switch(
            EXPR = whatPkg,
            "base" = {
                whatFun <- "write.table"
                args <- list(
                    "x" = object,
                    "file" = file,
                    "append" = FALSE,
                    "col.names" = colnames,
                    "dec" = ".",
                    "eol" = "\n",
                    "na" = "NA",
                    "qmethod" = "double",
                    "quote" = TRUE,
                    "row.names" = FALSE,
                    "sep" = switch(
                        EXPR = format,
                        "csv" = ",",
                        "tsv" = "\t"
                    )
                )
            },
            "data.table" = {
                whatFun <- "fwrite"
                args <- list(
                    "x" = object,
                    "file" = file,
                    "append" = FALSE,
                    "col.names" = colnames,
                    "compress" = "none",
                    "dateTimeAs" = "ISO",
                    "eol" = "\n",
                    "na" = "NA",
                    "qmethod" = "double",
                    "quote" = TRUE,
                    "row.names" = FALSE,
                    "sep" = switch(
                        EXPR = format,
                        "csv" = ",",
                        "tsv" = "\t"
                    ),
                    "showProgress" = FALSE,
                    "verbose" = verbose
                )
            },
            "readr" = {
                whatFun <- "write_delim"
                file <- normalizePath(
                    path = file,
                    winslash = "\\",
                    mustWork = FALSE
                )
                args <- list(
                    "x" = object,
                    "file" = file,
                    "append" = FALSE,
                    "col_names" = colnames,
                    "delim" = switch(
                        EXPR = format,
                        "csv" = ",",
                        "tsv" = "\t"
                    ),
                    "eol" = "\n",
                    "escape" = "double",
                    "na" = "NA",
                    "quote" = "all"
                )
            }
        )
        if (isFALSE(quiet)) {
            .alertExport(
                whatFile = whatFile,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        initDir(dirname(con))
        what <- get(x = whatFun, envir = asNamespace(whatPkg), inherits = TRUE)
        assert(is.function(what))
        do.call(what = what, args = args)
        if (isTRUE(compress)) {
            file <- compress(
                file = file,
                ext = compressExt,
                remove = TRUE,
                overwrite = TRUE
            )
        }
        file <- realpath(file)
        invisible(file)
    }



#' Export `Matrix` (e.g. `sparseMatrix`) method
#'
#' @note Updated 2022-06-07.
#' @noRd
#'
#' @details
#' Note that "file" is referring to the matrix file.
#' The correponding column and row sidecar files are generated automatically.
#' Consider adding HDF5 support in a future update.
`export,Matrix` <- # nolint
    function(object,
             con,
             format,
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        validObject(object)
        assert(
            hasLength(object),
            isString(con),
            isFlag(overwrite),
            isFlag(quiet)
        )
        formatChoices <- .exportFormatChoices[["Matrix"]]
        if (missing(format)) {
            format <- fileExt(con) # nocov
        }
        format <- match.arg(arg = format, choices = formatChoices)
        file <- con
        whatFile <- con
        whatPkg <- "Matrix"
        whatFun <- "writeMM"
        assert(requireNamespaces(whatPkg))
        compressExt <- fileExt(path = file, pattern = compressExtPattern)
        compress <- !is.na(compressExt)
        if (isAFile(file)) {
            file <- realpath(file)
            if (isTRUE(overwrite) && isFALSE(quiet)) {
                alertWarning(sprintf(
                    fmt = "Overwriting {.file %s} at {.path %s}.",
                    basename(file), realpath(dirname(file))
                ))
            } else {
                abort(sprintf("File exists: {.file %s}.", file))
            }
        }
        if (isTRUE(compress)) {
            file <- sub(
                pattern = paste0("\\.", compressExt, "$"),
                replacement = "",
                x = file
            )
        }
        if (isFALSE(quiet)) {
            .alertExport(
                whatFile = whatFile,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        initDir(dirname(con))
        args <- list("obj" = object, "file" = file)
        what <- get(x = whatFun, envir = asNamespace(whatPkg), inherits = TRUE)
        assert(is.function(what))
        do.call(what = what, args = args)
        if (isTRUE(compress)) {
            file <- compress(
                file = file,
                ext = compressExt,
                remove = TRUE,
                overwrite = TRUE
            )
        }
        file <- realpath(file)
        ## Write features (row names).
        if (!is.null(rownames(object))) {
            rownamesFile <- paste0(file, ".rownames")
            export(
                object = rownames(object),
                con = rownamesFile,
                overwrite = TRUE,
                quiet = quiet
            )
        } else {
            rownamesFile <- NULL
        }
        ## Write barcodes (column names).
        if (!is.null(colnames(object))) {
            colnamesFile <- paste0(file, ".colnames")
            export(
                object = colnames(object),
                con = colnamesFile,
                overwrite = TRUE,
                quiet = quiet
            )
        } else {
            colnamesFile <- NULL
        }
        files <- c(
            "matrix" = file,
            "rownames" = rownamesFile,
            "colnames" = colnamesFile
        )
        assert(allAreFiles(files))
        invisible(files)
    }



`export,DataFrame` <- # nolint
    `export,data.frame`

`export,GenomicRanges` <- # nolint
    `export,data.frame`

`export,matrix` <- # nolint
    `export,data.frame`



## S4 method exports ===========================================================

## FIXME What about when "format" is defined but "con" isn't?
## Don't we need to define these methods??

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DataFrame",
        con = "character",
        format = "character"
    ),
    definition = `export,DataFrame`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DataFrame",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,DataFrame`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GenomicRanges",
        con = "character",
        format = "character"
    ),
    definition = `export,GenomicRanges`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GenomicRanges",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,GenomicRanges`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Matrix",
        con = "character",
        format = "character"
    ),
    definition = `export,Matrix`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Matrix",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,Matrix`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "character",
        con = "character",
        format = "character"
    ),
    definition = `export,character`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "character",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,character`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "data.frame",
        con = "character",
        format = "character"
    ),
    definition = `export,data.frame`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "data.frame",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,data.frame`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "matrix",
        con = "character",
        format = "character"
    ),
    definition = `export,matrix`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "matrix",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,matrix`
)




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
             ...
    ) {
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



## Deprecated S4 method exports ================================================

`export,deprecated` <- function(object, con, format, ...) {
    stop("FIXME")
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
