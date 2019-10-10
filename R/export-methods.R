#' @name export
#' @inherit bioverbs::export
#' @note Updated 2019-10-10.
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
#' @section Exporting vectors:
#'
#' Use [`writeLines()`][base::writeLines] instead of [export()] to write vectors
#' to disk. An S4 character method may be defined in a future update, but it is
#' intentionally unsupported in the current release.
#'
#' @section Debugging:
#'
#' Note that this function currently wraps [data.table::fwrite()] by default.
#' If you encounter any stack imbalance or segfault warnings during export,
#' these are errors from data.table.
#'
#' See related:
#' - https://github.com/Rdatatable/data.table/issues/2457
#'
#' @inheritParams acidroxygen::params
#' @param object Object.
#'   An object supporting [`dim()`][base::dim], or a supported class capable
#'   of being coerced to `data.frame`, to be written to disk.
#' @param ext `character(1)`.
#'   Output file format extension.
#'   Uses [`match.arg()`][base::match.arg], where applicable.
#' @param file `character(1)`.
#'   File path. When left unset (default), the `ext` and `dir` arguments will
#'   be used instead.
#' @param compress `logical(1)`.
#'   Apply gzip compression to all files.
#' @param name `character(1)`.
#'   Name to use on disk. If `NULL`, will use the name of the object instead.
#' @param ... Additional arguments.
#'
#' @return Invisible `character`.
#' File path(s).
#'
#' @seealso
#' - [rio::export()].
#' - [rtracklayer::export()].
#' - [data.table::fwrite()].
#' - [readr::write_csv()].
#'
#' @examples
#' counts <- matrix(data = seq_len(100L), nrow = 10)
#' export(counts, ext = "csv")
#'
#' ## Clean up.
#' file.remove("counts.csv")
NULL



#' @rdname export
#' @name export
#' @importFrom bioverbs export
#' @usage export(object, ...)
#' @export
NULL



## This method covers standard `matrix` but is also intended to work for
## `data.table`, `tbl_df`, and `DataFrame` classes. Note that `rio::export()`
## does not preserve row names by default, so we're ensuring row names get
## coerced to "rowname" column consistently here.
## Updated 2019-10-10.
`export,matrix` <-  # nolint
    function(
        object,
        ext,
        dir,
        file = NULL,
        overwrite
    ) {
        validObject(object)
        engine <- match.arg(
            arg = getOption("acid.export.engine", default = "data.table"),
            choices = c("data.table", "readr")
        )
        whatPkg <- engine
        verbose <- getOption("acid.verbose", default = FALSE)
        assert(
            hasLength(object),
            isString(dir),
            isString(file, nullOK = TRUE),
            isFlag(overwrite),
            isFlag(verbose)
        )
        ext <- match.arg(
            arg = ext,
            choices = c(
                "csv", "csv.gz", "csv.bz2",
                "tsv", "tsv.gz", "tsv.bz2"
            )
        )
        ## Match the file extension and compression.
        if (is.null(file)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            if (!is.symbol(sym)) {
                ## nocov start
                stop(sprintf(
                    "'export()' object argument is not a symbol: %s.",
                    deparse(sym)
                ))
                ## nocov end
            }
            name <- as.character(sym)
            assert(isString(ext))
            file <- file.path(dir, paste0(name, ".", ext))
            string <- paste0(".", ext)
        } else {
            string <- basename(file)
        }
        match <- str_match(string = string, pattern = extPattern)
        ext <- match[1L, 2L]
        ext <- match.arg(arg = ext, choices = c("csv", "tsv"))
        compress <- match[1L, 4L]
        if (!is.na(compress)) {
            compress <- match.arg(arg = compress, choices = c("gz", "bz2"))
        }
        ## Keep the `as.data.frame()` call here, so we can inherit S4 methods.
        object <- as.data.frame(object)
        ## Ensure row names are automatically moved to `rowname` column.
        if (hasRownames(object)) {
            rownames <- "rowname"
        } else {
            rownames <- NULL
        }
        ## Now we're ready to coerce to tibble internally, which helps us move
        ## the row names into a column.
        object <- as_tibble(object, rownames = rownames)
        assert(hasRows(object), hasCols(object))
        ## Inform the user regarding overwrite.
        if (isAFile(file)) {
            if (isTRUE(overwrite)) {
                message(sprintf("Overwriting '%s'.", basename(file)))
            } else {
                stop(sprintf("File exists: %s", realpath(file)))
            }
        }
        ## Ensure directory is created automatically.
        initDir(dir = dirname(file))
        ## Remove compression extension from output file.
        if (!is.na(compress)) {
            file <- sub(
                pattern = paste0("\\.", compress, "$"),
                replacement = "",
                x = file
            )
        }
        if (identical(engine, "data.table")) {
            ## data.table ------------------------------------------------------
            ## Current default in rio package.
            whatFun <- "fwrite"
            what <- fwrite
            args <- list(
                x = as.data.table(object),
                file = file,
                row.names = FALSE,
                verbose = verbose
            )
            args[["sep"]] <- switch(
                EXPR = ext,
                "csv" = ",",
                "tsv" = "\t"
            )
        } else if (identical(engine, "readr")) {
            ## readr -----------------------------------------------------------
            assert(requireNamespace(whatPkg, quietly = TRUE))
            whatFun <- switch(
                EXPR = ext,
                "csv" = "write_csv",
                "tsv" = "write_tsv"
            )
            what <- get(
                x = whatFun,
                envir = asNamespace(whatPkg),
                inherits = TRUE
            )
            assert(is.function(what))
            args <- list(
                x = as_tibble(object),
                path = file
            )
        }
        message(sprintf(
            "Exporting '%s' using '%s::%s()'.",
            basename(file), whatPkg, whatFun
        ))
        do.call(what = what, args = args)
        ## Compress file, if necessary.
        if (!is.na(compress)) {
            compressFun <- switch(
                EXPR = compress,
                "gz" = gzip,
                "bz2" = bzip2
            )
            assert(is.function(compressFun))
            file <- compressFun(file, overwrite = TRUE)
        }
        file <- realpath(file)
        message(sprintf("Exported '%s'.", basename(file)))
        invisible(file)
    }

formals(`export,matrix`)[["dir"]] <- formalsList[["export.dir"]]
formals(`export,matrix`)[["ext"]] <- formalsList[["export.ext"]]
formals(`export,matrix`)[["overwrite"]] <- formalsList[["overwrite"]]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("matrix"),
    definition = `export,matrix`
)



`export,data.frame` <- `export,matrix`  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("data.frame"),
    definition = `export,data.frame`
)



`export,DataFrame` <- `export,data.frame`  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("DataFrame"),
    definition = `export,DataFrame`
)



## Note that "file" is referring to the matrix file.
## The correponding column and row sidecar files are generated automatically.
## Consider adding HDF5 support in a future update.
## Updated 2019-08-27.
`export,sparseMatrix` <-  # nolint
    function(
        object,
        ext,
        dir,
        file = NULL,
        overwrite
    ) {
        validObject(object)
        assert(
            hasLength(object),
            isString(dir),
            isString(file, nullOK = TRUE),
            isFlag(overwrite)
        )
        ext <- match.arg(
            arg = ext,
            choices = c("mtx", "mtx.gz", "mtx.bz2")
        )
        ## Match the file extension and compression.
        if (is.null(file)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            if (!is.symbol(sym)) {
                ## nocov start
                stop(sprintf(
                    "'export()' object argument is not a symbol: %s.",
                    deparse(sym)
                ))
                ## nocov end
            }
            name <- as.character(sym)
            assert(isString(ext))
            file <- file.path(dir, paste0(name, ".", ext))
            string <- paste0(".", ext)
        } else {
            string <- basename(file)
        }
        match <- str_match(string = string, pattern = extPattern)
        ext <- match[1L, 2L]
        ext <- match.arg(arg = ext, choices = "mtx")
        compress <- match[1L, 4L]
        if (!is.na(compress)) {
            compress <- match.arg(arg = compress, choices = c("gz", "bz2"))
        }
        ## Inform the user regarding overwrite.
        if (isAFile(file)) {
            if (isTRUE(overwrite)) {
                message(sprintf("Overwriting '%s'.", basename(file)))
            } else {
                stop(sprintf("File exists: %s", realpath(file)))
            }
        }
        ## Ensure directory is created automatically.
        initDir(dir = dirname(file))
        ## Remove compression extension from output file.
        if (!is.na(compress)) {
            file <- sub(
                pattern = paste0("\\.", compress, "$"),
                replacement = "",
                x = file
            )
        }
        ## Export MatrixMarket file.
        message(sprintf(
            "Exporting '%s' using '%s::%s()'.",
            basename(file), "Matrix", "writeMM"
        ))
        writeMM(obj = object, file = file)
        ## Compress file, if necessary.
        if (!is.na(compress)) {
            compressFun <- switch(
                EXPR = compress,
                "gz" = gzip,
                "bz2" = bzip2
            )
            assert(is.function(compressFun))
            file <- compressFun(file, overwrite = TRUE)
        }
        ## Normalize the path.
        file <- realpath(file)
        ## Write barcodes (column names).
        barcodes <- colnames(object)
        barcodesFile <- paste0(file, ".colnames")
        writeLines(text = barcodes, con = barcodesFile)
        ## Write features (row names).
        features <- rownames(object)
        featuresFile <- paste0(file, ".rownames")
        writeLines(text = features, con = featuresFile)
        files <- c(
            matrix = file,
            barcodes = barcodesFile,
            genes = featuresFile
        )
        assert(allAreFiles(files))
        message(sprintf(
            "Exported '%s' and sidecar files to '%s'.",
            basename(file), dirname(file)
        ))
        ## Return named character of file paths.
        invisible(files)
    }

formals(`export,sparseMatrix`)[["dir"]] <-
    formalsList[["export.dir"]]
formals(`export,sparseMatrix`)[["ext"]] <-
    formalsList[["export.sparse.ext"]]
formals(`export,sparseMatrix`)[["overwrite"]] <-
    formalsList[["overwrite"]]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("sparseMatrix"),
    definition = `export,sparseMatrix`
)



`export,GRanges` <- `export,DataFrame`  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("GRanges"),
    definition = `export,GRanges`
)



## Updated 2019-07-19.
.exportAssays <-  # nolint
    function(object, name, dir, compress) {
        assayNames <- assayNames(object)
        assert(isCharacter(assayNames))
        message(sprintf("Exporting assays: %s.", toString(assayNames)))
        out <- lapply(
            X = assayNames,
            FUN = function(name, dir) {
                file <- file.path(dir, name)
                assay <- assays(object)[[name]]
                ## Dynamically set the file name based on the assay type.
                if (is(assay, "sparseMatrix")) {
                    ext <- "mtx"
                } else {
                    ext <- "csv"
                }
                if (isTRUE(compress)) {
                    ext <- paste0(ext, ".gz")
                }
                file <- paste0(file, ".", ext)
                export(assay, file = file)
            },
            dir = initDir(file.path(dir, "assays"))
        )
        names(out) <- assayNames
        out
    }



## Updated 2019-07-19.
.exportColData <-  # nolint
    function(object, ext, dir) {
        export(
            object = atomize(colData(object)),
            file = file.path(dir, paste0("colData", ext))
        )
    }



## NOTE: The standard `rowData()` output is okay but doesn't include genomic
## ranges coordinates. That's why we're coercing from `rowRanges()` for RSE.
## Updated 2019-07-19.
.exportRowData <-  # nolint
    function(object, ext, dir) {
        data <- rowData(object)
        ## Note that SummarizedExperiment in BioC 3.6/R 3.4 release doesn't
        ## set row names properly, so keep this step here for compatibility.
        if (!hasRownames(data)) {
            rownames(data) <- rownames(object)  # nocov
        }
        data <- atomize(data)
        data <- as.data.frame(data)
        assert(identical(rownames(data), rownames(object)))
        export(object = data, file = file.path(dir, paste0("rowData", ext)))
    }



## Updated 2019-08-27.
`export,SummarizedExperiment` <-  # nolint
    function(
        object,
        name = NULL,
        dir,
        compress
    ) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isString(dir),
            isFlag(compress)
        )
        call <- standardizeCall()
        ## Get the name and create directory substructure.
        if (is.null(name)) {
            sym <- call[["object"]]
            if (!is.symbol(sym)) {
                stop(sprintf(
                    "'export()' object argument is not a symbol: %s.",
                    deparse(sym)
                ))
            }
            name <- as.character(sym)
        }
        dir <- initDir(file.path(dir, name))
        ## Return the file paths back to the user as a named list.
        files <- list()
        ## Set the desired output extension, depending on whether we need to
        ## enable compression. Always output consistently in CSV format.
        ext <- "csv"
        if (isTRUE(compress)) {
            ext <- paste0(ext, ".gz")
        }
        ext <- paste0(".", ext)
        ## Ensure the assays list is always named. Note that valid SE objects
        ## don't have to contain named assays (e.g. DESeqTransform). In the
        ## event that an SE object contains a single, unnamed assay, we make
        ## sure to rename it internally to "assay" before exporting.
        if (is.null(assayNames(object))) {
            assayNames(object) <- "assay"
        }
        ## Assays.
        files[["assays"]] <-
            .exportAssays(
                object = object,
                name = name,
                dir = dir,
                compress = compress
            )
        ## Column data.
        files[["colData"]] <-
            .exportColData(
                object = object,
                ext = ext,
                dir = dir
            )
        ## Row data.
        files[["rowData"]] <-
            .exportRowData(
                object = object,
                ext = ext,
                dir = dir
            )
        message(sprintf("Exported '%s' to '%s'.", name, dir))
        ## Return named character of file paths.
        files <- Filter(Negate(is.null), files)
        assert(hasNames(files))
        invisible(files)
    }

formals(`export,SummarizedExperiment`)[["compress"]] <-
    formalsList[["export.compress"]]
formals(`export,SummarizedExperiment`)[["dir"]] <-
    formalsList[["export.dir"]]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SummarizedExperiment"),
    definition = `export,SummarizedExperiment`
)



## Updated 2019-08-27.
`export,SingleCellExperiment` <-  # nolint
    function(
        object,
        name = NULL,
        dir,
        compress
    ) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isString(dir),
            isFlag(compress)
        )
        call <- standardizeCall()
        ## Get the name and create directory substructure.
        if (is.null(name)) {
            sym <- call[["object"]]
            if (!is.symbol(sym)) {
                stop(sprintf(
                    "'export()' object argument is not a symbol: %s.",
                    deparse(sym)
                ))
            }
            name <- as.character(sym)
        }
        ## Export SummarizedExperiment-compatible slots.
        files <- export(
            object = as(object, "RangedSummarizedExperiment"),
            name = name,
            dir = dir,
            compress = compress
        )
        dir <- initDir(file.path(dir, name))
        ## Export dimensionality reduction data.
        reducedDimNames <- reducedDimNames(object)
        if (hasLength(reducedDimNames)) {
            message(sprintf(
                "Exporting reducedDims: %s",
                toString(reducedDimNames)
            ))
            files[["reducedDims"]] <- lapply(
                X = reducedDimNames,
                FUN = function(name, dir) {
                    file <- file.path(dir, name)
                    reducedDim <- reducedDim(
                        x = object,
                        type = name,
                        withDimnames = TRUE
                    )
                    if (is(reducedDim, "matrix")) {
                        ext <- "csv"
                    } else if (is(reducedDim, "sparseMatrix")) {
                        ext <- "mtx"
                    }
                    if (isTRUE(compress)) {
                        ext <- paste0(ext, ".gz")
                    }
                    file <- paste0(file, ".", ext)
                    export(reducedDim, file = file)
                },
                dir = initDir(file.path(dir, "reducedDims"))
            )
            names(files[["reducedDims"]]) <- reducedDimNames
        }
        assert(hasNames(files))
        invisible(files)
    }

formals(`export,SingleCellExperiment`)[["compress"]] <-
    formalsList[["export.compress"]]
formals(`export,SingleCellExperiment`)[["dir"]] <-
    formalsList[["export.dir"]]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SingleCellExperiment"),
    definition = `export,SingleCellExperiment`
)
