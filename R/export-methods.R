#' @name export
#' @inherit AcidGenerics::export
#' @note Updated 2023-11-08.
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
#' for exporting `DFrame`, `data.frame`, and `matrix` class objects.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param append `logical(1)`.
#' Append to output file.
#' When enabled, automatically sets `overwrite` argument to `FALSE`.
#' Requires readr package to be installed.
#'
#' @param engine `character(1)`.
#' Engine (package) to use for export.
#'
#' Currently supported:
#' - base
#' - data.table
#' - readr
#'
#' @param quote `logical(1)`.
#' Surround any `character` or `factor` columns by double quotes.
#' Recommended by default.
#'
#' @param ... Additional arguments.
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
#' export(object = counts, con = "counts.csv")
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



#' Return default extension for object
#'
#' @note Updated 2023-11-08.
#' @noRd
#'
#' @details
#' Can consider exporting this as a generic and making it object-oriented in a
#' future package update, to better support custom methods defined outside of
#' the package.
.defaultExt <- function(object) {
    if (isAny(
        x = object,
        classes = c(
            "matrix",
            "data.frame",
            "DFrame",
            "GRanges",
            "GRangesList"
        )
    )) {
        key <- "delim"
    } else if (is(object, "Matrix")) {
        key <- "Matrix"
    } else if (is.atomic(object)) {
        key <- "character"
    } else {
        return(character())
    }
    choices <- .exportFormatChoices
    ext <- choices[[key]][[1L]]
    assert(isString(ext))
    ext
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



#' Easy export of an object to working directory
#'
#' @note Updated 2023-11-08.
#' @noRd
`export,ANY,missing` <- # nolint
    function(object, con, ...) {
        dots <- list(...)
        if (isSubset("ext", names(dots))) {
            abort(sprintf(
                "Use {.arg %s} instead of {.arg %s}.",
                "con", "ext"
            ))
        }
        if (isSubset("file", names(dots))) {
            abort(sprintf(
                "Use {.arg %s} instead of {.arg %s}.",
                "con", "file"
            ))
        }
        if (isSubset("format", names(dots))) {
            abort(sprintf(
                "Use {.arg %s} instead of {.arg %s}.",
                "con", "format"
            ))
        }
        if (missing(con)) {
            con <- NULL
        }
        dir <- getOption(x = "acid.export.dir", default = getwd())
        assert(
            is.null(con),
            isString(dir)
        )
        call <- standardizeCall()
        sym <- call[["object"]]
        assert(is.symbol(sym), msg = .symError)
        name <- as.character(sym)
        ext <- .defaultExt(object)
        if (!hasLength(ext)) {
            abort(sprintf(
                "{.cls %s} is not supported. Use {.var %s} to define output.",
                simpleClass(object), "con"
            ))
        }
        con <- file.path(dir, paste0(name, ".", ext))
        export(object = object, con = con, ...)
    }



## Updated 2023-11-08.
`export,atomic` <- # nolint
    function(object,
             con,
             append = FALSE,
             overwrite = TRUE,
             engine = c("base", "data.table", "readr"),
             quiet = FALSE) {
        whatPkg <- match.arg(engine)
        assert(
            requireNamespaces(whatPkg),
            validObject(object),
            isString(con),
            isFlag(overwrite),
            isFlag(append),
            isFlag(quiet)
        )
        object <- as.character(object)
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
                    "verbose" = FALSE
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



#' Export `data.frame` method
#'
#' @note Updated 2023-11-08.
#' @noRd
#'
#' @details
#' This method covers standard `matrix` but is also intended to work for
#' `data.table`, `tbl_df`, and `DFrame` classes. Note that `rio::export()`
#' doesn't preserve row names by default, so we're ensuring row names get
#' coerced to "rowname" column consistently here.
`export,data.frame` <- # nolint
    function(object,
             con,
             rownames = TRUE,
             colnames = TRUE,
             quote = TRUE,
             overwrite = TRUE,
             engine = c("base", "data.table", "readr"),
             quiet = FALSE) {
        whatPkg <- match.arg(engine)
        assert(
            requireNamespaces(whatPkg),
            validObject(object),
            hasNoDuplicates(colnames(object)),
            isString(con),
            isSubset(fileExt(con), .exportFormatChoices[["delim"]]),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(quote),
            isFlag(overwrite),
            isFlag(quiet)
        )
        object <- as.data.frame(object, optional = TRUE)
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
                            NULL
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
                alertWarning(sprintf(
                    "Dropping non-atomic columns: %s.",
                    toInlineString(names(keep)[!keep], n = 10L)
                ))
                object <- object[, keep, drop = FALSE]
            }
        }
        if (isFALSE(rownames)) {
            rownames(object) <- NULL
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
            assert(isSubset(fileExt(file), c("csv", "tsv")))
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
                    "quote" = quote,
                    "row.names" = FALSE,
                    "sep" = switch(
                        EXPR = fileExt(file),
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
                    "quote" = quote,
                    "row.names" = FALSE,
                    "sep" = switch(
                        EXPR = fileExt(file),
                        "csv" = ",",
                        "tsv" = "\t"
                    ),
                    "showProgress" = FALSE,
                    "verbose" = FALSE
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
                        EXPR = fileExt(file),
                        "csv" = ",",
                        "tsv" = "\t"
                    ),
                    "eol" = "\n",
                    "escape" = "double",
                    "na" = "NA",
                    "quote" = ifelse(
                        test = isTRUE(quote),
                        yes = "all",
                        no = "none"
                    )
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



#' Export `list` method
#'
#' @note Updated 2023-11-09.
#' @noRd
`export,list` <- # nolint
    function(object,
             con,
             overwrite = TRUE,
             quiet = FALSE) {
        assert(
            validObject(object),
            hasLength(object),
            isString(con),
            isFlag(overwrite),
            isFlag(quiet)
        )
        con <- initDir(con)
        names <- names(object)
        if (is.null(names)) {
            names <- autopadZeros(seq_along(object))
        }
        out <- Map(
            name = names,
            object = object,
            MoreArgs = list(
                "con" = con,
                "overwrite" = overwrite,
                "quiet" = quiet
            ),
            f = function(object, name, con, overwrite, quiet) {
                con <- file.path(con, name)
                ext <- .defaultExt(object)
                if (isString(ext)) {
                    con <- paste0(con, ".", ext)
                }
                export(
                    object = object,
                    con = con,
                    overwrite = overwrite,
                    quiet = quiet
                )
            },
            USE.NAMES = TRUE
        )
        invisible(out)
    }



#' Export `Matrix` (e.g. `sparseMatrix`) method
#'
#' @note Updated 2023-09-20.
#' @noRd
#'
#' @details
#' Note that "file" is referring to the matrix file.
#' The correponding column and row sidecar files are generated automatically.
#' Consider adding HDF5 support in a future update.
`export,Matrix` <- # nolint
    function(object,
             con,
             overwrite = TRUE,
             quiet = FALSE) {
        whatPkg <- "Matrix"
        whatFun <- "writeMM"
        assert(
            requireNamespaces(whatPkg),
            validObject(object),
            hasLength(object),
            isString(con),
            isSubset(fileExt(con), .exportFormatChoices[["Matrix"]]),
            isFlag(overwrite),
            isFlag(quiet)
        )
        file <- con
        whatFile <- con
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



`export,DFrame` <- # nolint
    `export,data.frame`

`export,GRanges` <- # nolint
    `export,data.frame`

`export,List` <- # nolint
    `export,list`

`export,matrix` <- # nolint
    `export,data.frame`



## S4 method exports ===========================================================

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DFrame",
        con = "character"
    ),
    definition = `export,DFrame`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GRanges",
        con = "character"
    ),
    definition = `export,GRanges`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "List",
        con = "character"
    ),
    definition = `export,List`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "Matrix",
        con = "character"
    ),
    definition = `export,Matrix`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "atomic",
        con = "character"
    ),
    definition = `export,atomic`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "data.frame",
        con = "character"
    ),
    definition = `export,data.frame`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "list",
        con = "character"
    ),
    definition = `export,list`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "matrix",
        con = "character"
    ),
    definition = `export,matrix`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "ANY",
        con = "missing"
    ),
    definition = `export,ANY,missing`
)
