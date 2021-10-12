#' Export
#'
#' @name export
#' @note Updated 2021-09-28.
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
#' Note that this function currently wraps `data.table::fwrite()` by default
#' for exporting `data.frame` and `matrix` class objects.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams import
#' @param object Object.
#'   An object supporting `dim()`, or a supported class capable of being coerced
#'   to `data.frame`, to be written to disk.
#' @param append `logical(1)`.
#'   Append to output file.
#'   When enabled, automatically sets `overwrite` argument to `FALSE`.
#'   Requires readr package to be installed.
#' @param con `character(1)`.
#'   File path.
#'   Alternatively, can leave unset and use `ext` and `dir` arguments instead.
#' @param engine `character(1)`.
#'   Engine (package) to use for export.
#'   Currently supported:
#'   - base
#'   - data.table
#'   - readr
#'   - vroom
#' @param ext `character(1)`.
#'   *Deprecated in favor of `format` argument.*
#' @param file `character(1)`.
#'   *Deprecated in favor of `con` argument.*
#' @param format `character(1)`.
#'   Output file format extension.
#'
#'   `matrix` supported arguments:
#'   - Comma separated values (CSV):
#'     `"csv"`, `"csv.bz2"`, `"csv.gz"`, `"csv.xz"`, `"csv.zip"`.
#'   - Tab separated values (TSV):
#'     `"tsv"`, `"tsv.bz2"`, `"tsv.gz"`, `"tsv.xz"`, `"tsv.zip"`.
#'
#'   `Matrix` (`sparseMatrix`) supported arguments:
#'   - MatrixMarket exchange (MTX):
#'     `"mtx"`, `"mtx.bz2"`, `"mtx.gz"`, `"mtx.xz"`, `"mtx.zip"`.
#' @param ... Additional arguments.
#'
#' @return Invisible `character`.
#' File path(s).
#'
#' @seealso
#' Packages:
#'
#' - [data.table](http://r-datatable.com/).
#' - [readr](http://readr.tidyverse.org).
#' - [rio](https://cran.r-project.org/package=rio).
#' - [rtracklayer](http://bioconductor.org/packages/rtracklayer/).
#' - [vroom](https://vroom.r-lib.org).
#'
#' Export functions:
#'
#' - `BiocIO::export()`.
#' - `data.table::fwrite()`.
#' - `readr::write_csv()`.
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



## Updated 2021-09-27.
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
    "matrix" = c(
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



## This method sets "con" argument automatically from "format" and "dir".
## Updated 2021-10-12.
`export,character,format` <-
    function(
        object,
        con,  # NULL
        format,
        dir,
        ...,
        ext,  # deprecated in favor of "format"
        file  # deprecated in favor of "con"

    ) {
        if (!missing(file)) {
            ## > .Deprecated(sprintf(
            ## >     "Use '%s' instead of '%s'.",
            ## >     "con", "file"
            ## > ))
            con <- file
        }
        if (missing(con)) {
            con <- NULL
        }
        if (!missing(ext)) {
            ## > .Deprecated(sprintf(
            ## >     "Use '%s' instead of '%s'.",
            ## >     "format", "ext"
            ## > ))
            format <- ext
        }
        assert(isString(dir))
        formatChoices <- .exportFormatChoices[["character"]]
        if (missing(format)) {
            format <- formatChoices[[1L]]
        }
        format <- match.arg(arg = format, choices = formatChoices)
        if (is.null(con)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            assert(is.symbol(sym), msg = .symError)
            name <- as.character(sym)
            con <- file.path(dir, paste0(name, ".", format))
        }
        export(
            object = object,
            con = con,
            format = format,
            ...
        )
    }

formals(`export,character,format`)[["dir"]] <-
    .formalsList[["export.dir"]]

## This is the primary character method that uses "con" for file path.
## Updated 2021-10-12.
`export,character,con` <-  # nolint
    function(
        object,
        con,
        format,
        append = FALSE,
        overwrite,
        engine = getOption(x = "acid.export.engine", default = "base"),
        quiet
    ) {
        assert(
            isString(con),
            isFlag(overwrite),
            isFlag(append),
            isString(engine),
            isFlag(quiet)
        )
        formatChoices <- .exportFormatChoices[["character"]]
        if (missing(format)) {
            format <- fileExt(con)
        }
        format <- match.arg(arg = format, choices = formatChoices)
        whatPkg <- match.arg(arg = engine, choices = .engines)
        requireNamespaces(whatPkg)
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
        file <- con; whatFile <- con
        match <- str_match(string = file, pattern = extPattern)
        compressExt <- match[1L, 4L]
        compress <- !is.na(compressExt)
        if (isAFile(file)) {
            file <- realpath(file)
            if (isTRUE(append) && isFALSE(quiet)) {
                alertInfo(sprintf(
                    "Appending content in {.file %s}.",
                    basename(file)
                ))
            } else if (isTRUE(overwrite) && isFALSE(quiet)) {
                alertWarning(sprintf("Overwriting {.file %s}.", file))
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
        switch(
            EXPR = whatPkg,
            "base" = {
                whatFun <- "writeLines"
                args <- list(
                    "text" = object,
                    "con" = file
                )
            },
            "data.table" = {
                whatFun <- "fwrite"
                args <- list(
                    "x" = as.list(object),
                    "file" = file,
                    "append" = append,
                    "sep" = "\n"
                )
            },
            "readr" = {
                whatFun <- "write_lines"
                args <- list(
                    "x" = object,
                    "file" = file,
                    "append" = append
                )
            },
            "vroom" = {
                whatFun <- "vroom_write_lines"
                args <- list(
                    "x" = object,
                    "file" = file,
                    "append" = append
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

formals(`export,character,con`)[c("overwrite", "quiet")] <-
    .formalsList[c("overwrite", "quiet")]



## Updated 2021-10-12.
`export,matrix,format` <-  # nolint
    function(
        object,
        con,  # NULL
        format,
        dir,
        ...,
        ext,  # deprecated in favor of "format"
        file  # deprecated in favor of "con"
    ) {
        if (!missing(file)) {
            ## > .Deprecated(sprintf(
            ## >     "Use '%s' instead of '%s'.",
            ## >     "con", "file"
            ## > ))
            con <- file
        }
        if (missing(con)) {
            con <- NULL
        }
        if (!missing(ext)) {
            ## > .Deprecated(sprintf(
            ## >     "Use '%s' instead of '%s'.",
            ## >     "format", "ext"
            ## > ))
            format <- ext
        }
        assert(isString(dir))
        formatChoices <- .exportFormatChoices[["matrix"]]
        if (missing(format)) {
            format <- formatChoices[[1L]]
        }
        format <- match.arg(arg = format, choices = formatChoices)
        if (is.null(con)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            assert(is.symbol(sym), msg = .symError)
            name <- as.character(sym)
            con <- file.path(dir, paste0(name, ".", format))
        }
        export(
            object = object,
            con = con,
            format = format,
            ...
        )
    }

formals(`export,matrix,format`)[["dir"]] <-
    .formalsList[["export.dir"]]



#' Export `matrix` method
#'
#' @note Updated 2021-09-27.
#' @noRd
#'
#' @details
#' This method covers standard `matrix` but is also intended to work for
#' `data.table`, `tbl_df`, and `DataFrame` classes. Note that `rio::export()`
#' does not preserve row names by default, so we're ensuring row names get
#' coerced to "rowname" column consistently here.
`export,matrix,con` <-  # nolint
    function(
        object,
        con,
        format,
        rownames = TRUE,
        colnames = TRUE,
        overwrite,
        engine = getOption(x = "acid.export.engine", "data.table"),
        quiet
    ) {
        validObject(object)
        object <- as.data.frame(object)
        verbose <- getOption("acid.verbose", default = FALSE)
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
        formatChoices <- .exportFormatChoices[["matrix"]]
        if (missing(format)) {
            format <- fileExt(con)
        }
        format <- match.arg(arg = format, choices = formatChoices)
        whatPkg <- match.arg(arg = engine, choices = .engines)
        requireNamespaces(whatPkg)
        file <- con; whatFile <- con
        match <- str_match(string = file, pattern = extPattern)
        compressExt <- match[1L, 4L]
        compress <- !is.na(compressExt)
        ## Drop non-atomic columns automatically, if necessary.
        keep <- bapply(X = object, FUN = is.atomic)
        if (!all(keep)) {
            ## nocov start
            ## This is used to handle rowData with nested entrez identifiers.
            fail <- names(keep)[!keep]
            alertWarning(sprintf(
                "Dropping non-atomic columns: %s.",
                toInlineString(fail, n = 10L)
            ))
            object <- object[, keep, drop = FALSE]
            ## nocov end
        }
        if (isFALSE(rownames)) {
            rownames(object) <- NULL  # nocov
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
                    "na" = "NA",
                    "quote_escape" = "double"
                )
            },
            "vroom" = {
                whatFun <- "vroom_write"
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
                    "progress" = FALSE,
                    "quote" = "needed"
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

formals(`export,matrix,con`)[c("overwrite", "quiet")] <-
    .formalsList[c("overwrite", "quiet")]



`export,data.frame,con` <-  # nolint
    `export,matrix,con`

`export,data.frame,format` <-  # nolint
    `export,matrix,format`



`export,DataFrame,con` <-
    `export,data.frame,con`

`export,DataFrame,format` <-
    `export,data.frame,format`



`export,GRanges,con` <-   # nolint
    `export,DataFrame,con`

`export,GRanges,format` <-   # nolint
    `export,DataFrame,format`



## Updated 2021-10-12.
`export,Matrix,format` <-  # nolint
    function(
        object,
        con,  # NULL
        format,
        dir,
        ...,
        ext,  # deprecated in favor of "format"
        file  # deprecated in favor of "con"
    ) {
        if (!missing(file)) {
            ## > .Deprecated(sprintf(
            ## >     "Use '%s' instead of '%s'.",
            ## >     "con", "file"
            ## > ))
            con <- file
        }
        if (missing(con)) {
            con <- NULL
        }
        if (!missing(ext)) {
            ## > .Deprecated(sprintf(
            ## >     "Use '%s' instead of '%s'.",
            ## >     "format", "ext"
            ## > ))
            format <- ext
        }
        assert(isString(dir))
        formatChoices <- .exportFormatChoices[["Matrix"]]
        if (missing(format)) {
            format <- formatChoices[[1L]]
        }
        format <- match.arg(arg = format, choices = formatChoices)
        if (is.null(con)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            assert(is.symbol(sym), msg = .symError)
            name <- as.character(sym)
            con <- file.path(dir, paste0(name, ".", format))
        }
        export(
            object = object,
            con = con,
            format = format,
            ...
        )
    }

formals(`export,Matrix,format`)[["dir"]] <-
    .formalsList[["export.dir"]]



#' Export `Matrix` (e.g. `sparseMatrix`) method
#'
#' @note Updated 2021-09-28.
#' @noRd
#'
#' @details
#' Note that "file" is referring to the matrix file.
#' The correponding column and row sidecar files are generated automatically.
#' Consider adding HDF5 support in a future update.
`export,Matrix,con` <-  # nolint
    function(
        object,
        con,
        format,
        overwrite,
        quiet
    ) {
        validObject(object)
        assert(
            hasLength(object),
            isString(con),
            isFlag(overwrite),
            isFlag(quiet)
        )
        formatChoices <- .exportFormatChoices[["Matrix"]]
        if (missing(format)) {
            format <- fileExt(con)
        }
        format <- match.arg(arg = format, choices = formatChoices)
        file <- con; whatFile <- con
        whatPkg <- "Matrix"
        whatFun <- "writeMM"
        requireNamespaces(whatPkg)
        match <- str_match(string = file, pattern = extPattern)
        compressExt <- match[1L, 4L]
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
        ## Write barcodes (column names).
        barcodes <- colnames(object)
        barcodesFile <- paste0(file, ".colnames")
        export(
            object = barcodes,
            file = barcodesFile,
            overwrite = TRUE,
            quiet = quiet
        )
        ## Write features (row names).
        features <- rownames(object)
        featuresFile <- paste0(file, ".rownames")
        export(
            object = features,
            file = featuresFile,
            overwrite = TRUE,
            quiet = quiet
        )
        files <- c(
            "matrix" = file,
            "barcodes" = barcodesFile,
            "genes" = featuresFile
        )
        assert(allAreFiles(files))
        invisible(files)
    }

formals(`export,Matrix,con`)[
    c("overwrite", "quiet")] <-
    .formalsList[c("overwrite", "quiet")]



## S4 method exports ===========================================================

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DataFrame",
        con = "character",
        format = "character"
    ),
    definition = `export,DataFrame,con`
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
    definition = `export,DataFrame,con`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "DataFrame",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,DataFrame,format`
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
    definition = `export,DataFrame,format`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GRanges",
        con = "character",
        format = "character"
    ),
    definition = `export,GRanges,con`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GRanges",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,GRanges,con`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GRanges",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,GRanges,format`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "GRanges",
        con = "missingOrNULL",
        format = "character"
    ),
    definition = `export,GRanges,format`
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
    definition = `export,Matrix,con`
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
    definition = `export,Matrix,con`
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
    definition = `export,Matrix,format`
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
    definition = `export,Matrix,format`
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
    definition = `export,character,con`
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
    definition = `export,character,con`
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
    definition = `export,character,format`
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
    definition = `export,character,format`
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
    definition = `export,data.frame,con`
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
    definition = `export,data.frame,con`
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
    definition = `export,data.frame,format`
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
    definition = `export,data.frame,format`
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
    definition = `export,matrix,con`
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
    definition = `export,matrix,con`
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
    definition = `export,matrix,format`
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
    definition = `export,matrix,format`
)
