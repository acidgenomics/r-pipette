#' @name export
#' @inherit acidgenerics::export
#' @note Updated 2020-01-19.
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
#' Note that this function currently wraps [data.table::fwrite()] by default
#' for exporting `data.frame` and `matrix` class objects. If you encounter any
#' stack imbalance or segfault warnings during export, these are errors from
#' data.table.
#'
#' @inheritParams acidroxygen::params
#' @param object Object.
#'   An object supporting [`dim()`][base::dim], or a supported class capable
#'   of being coerced to `data.frame`, to be written to disk.
#' @param ext `character(1)`.
#'   Output file format extension.
#'
#'   `matrix` supported arguments:
#'   - CSV "csv", "csv.bz2", "csv.gz", "csv.xz", "csv.zip"
#'   - TSV "tsv", "tsv.bz2", "tsv.gz", "tsv.xz", "tsv.zip"
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
#' @importFrom acidgenerics export
#' @usage export(object, ...)
#' @export
NULL



## Updated 2020-01-19.
`export,character` <-  # nolint
    function(
        object,
        ext = "txt",
        dir,
        file = NULL,
        overwrite,
        quiet
    ) {
        assert(
            isCharacter(object),
            isString(ext),
            isString(dir),
            isString(file, nullOK = TRUE),
            isFlag(overwrite),
            isFlag(quiet)
        )
        if (is.null(file)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
            ext <- match.arg(
                arg = ext,
                choices = c(
                    "txt",
                    "txt.bz2",
                    "txt.gz",
                    "txt.xz",
                    "txt.zip"
                )
            )
            file <- file.path(dir, paste0(name, ".", ext))
        }
        match <- str_match(string = file, pattern = extPattern)
        compressExt <- match[1L, 4L]
        compress <- !is.na(compressExt)
        ## Inform the user regarding overwrite.
        if (isAFile(file)) {
            if (isTRUE(overwrite) && !isTRUE(quiet)) {
                cli_alert_warning(
                    sprintf("Overwriting {.file %s}.", basename(file))
                )
            } else {
                stop(sprintf("File exists: '%s'", realpath(file)))
            }
        }
        if (!isTRUE(quiet)) {
            cli_alert(sprintf(
                "Exporting {.file %s} using {.pkg %s}::{.fun %s}.",
                basename(file), "base", "writeLines"
            ))
        }
        con <- file(description = file)
        writeLines(text = object, con = con)
        close(con)
        ## Compress file, if necessary.
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

formals(`export,character`)[
    c(
        "dir",
        "ext",
        "overwrite",
        "quiet"
    )] <-
    formalsList[c(
        "export.dir",
        "export.ext",
        "overwrite",
        "quiet"
    )]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("character"),
    definition = `export,character`
)



## This method covers standard `matrix` but is also intended to work for
## `data.table`, `tbl_df`, and `DataFrame` classes. Note that `rio::export()`
## does not preserve row names by default, so we're ensuring row names get
## coerced to "rowname" column consistently here.
## Updated 2020-01-19.
`export,matrix` <-  # nolint
    function(
        object,
        ext,
        dir,
        file = NULL,
        overwrite,
        quiet
    ) {
        validObject(object)
        object <- as.data.frame(object)
        whatPkg <- match.arg(
            arg = getOption("acid.export.engine", default = "data.table"),
            choices = c("data.table", "readr", "vroom")
        )
        verbose <- getOption("acid.verbose", default = FALSE)
        assert(
            requireNamespace(whatPkg, quietly = TRUE),
            hasLength(object),
            hasRows(object),
            hasCols(object),
            hasNoDuplicates(colnames(object)),
            allAreAtomic(object),
            isString(ext),
            isString(dir),
            isString(file, nullOK = TRUE),
            isFlag(overwrite),
            isFlag(verbose)
        )
        if (is.null(file)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
            file <- file.path(dir, paste0(name, ".", ext))
        } else {
            ext <- fileExt(file)
        }
        ext <- match.arg(
            arg = ext,
            choices = c(
                "csv", "csv.bz2", "csv.gz", "csv.xz", "csv.zip",
                "tsv", "tsv.bz2", "tsv.gz", "tsv.xz", "tsv.zip"
            )
        )
        match <- str_match(string = file, pattern = extPattern)
        compressExt <- match[1L, 4L]
        compress <- !is.na(compressExt)
        ## Ensure row names are automatically moved to `rowname` column.
        if (hasRownames(object)) {
            assert(areDisjointSets("rowname", colnames(object)))
            object[["rowname"]] <- rownames(object)
            rownames(object) <- NULL
            ## Ensure the "rowname" column appears first.
            object <- object[
                ,
                c("rowname", setdiff(colnames(object), "rowname")),
                drop = FALSE
            ]
        }
        ## Inform the user regarding overwrite.
        if (isAFile(file)) {
            if (isTRUE(overwrite) && !isTRUE(quiet)) {
                cli_alert_warning(
                    sprintf("Overwriting {.file %s}.", basename(file))
                )
            } else {
                stop(sprintf("File exists: '%s'", realpath(file)))
            }
        }
        ## Ensure directory is created automatically.
        initDir(dir = dirname(file))
        ## Remove compression extension from output file path, prior to running
        ## `compress()` step below.
        if (isTRUE(compress)) {
            file <- sub(
                pattern = paste0("\\.", compressExt, "$"),
                replacement = "",
                x = file
            )
        }
        if (identical(whatPkg, "data.table")) {
            ## data.table ------------------------------------------------------
            whatFun <- "fwrite"
            what <- fwrite
            args <- list(
                x = object,
                file = file,
                row.names = FALSE,
                verbose = verbose
            )
            args[["sep"]] <- switch(
                EXPR = ext,
                "csv" = ",",
                "tsv" = "\t"
            )
        } else if (identical(whatPkg, "readr")) {
            ## readr -----------------------------------------------------------
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
                x = object,
                path = file
            )
        }  else if (identical(whatPkg, "vroom")) {
            ## vroom -----------------------------------------------------------
            whatFun <- "vroom_write"
            what <- get(
                x = whatFun,
                envir = asNamespace(whatPkg),
                inherits = TRUE
            )
            assert(is.function(what))
            args <- list(
                x = object,
                path = file,
                progress = FALSE
            )
            args[["delim"]] <- switch(
                EXPR = ext,
                "csv" = ",",
                "tsv" = "\t"
            )
        }
        if (!isTRUE(quiet)) {
            cli_alert(sprintf(
                "Exporting {.file %s} using {.pkg %s}::{.fun %s}.",
                basename(file), whatPkg, whatFun
            ))
        }
        do.call(what = what, args = args)
        ## Compress file, if necessary.
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

formals(`export,matrix`)[
    c(
        "dir",
        "ext",
        "overwrite",
        "quiet"
    )] <-
    formalsList[c(
        "export.dir",
        "export.ext",
        "overwrite",
        "quiet"
    )]



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
## Updated 2020-01-19.
`export,sparseMatrix` <-  # nolint
    function(
        object,
        ext,
        dir,
        file = NULL,
        overwrite,
        quiet
    ) {
        validObject(object)
        assert(
            hasLength(object),
            isString(ext),
            isString(dir),
            isString(file, nullOK = TRUE),
            isFlag(overwrite),
            isFlag(quiet)
        )
        if (is.null(file)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
            file <- file.path(dir, paste0(name, ".", ext))
        } else {
            ext <- fileExt(file)
        }
        ext <- match.arg(
            arg = ext,
            choices = c(
                "mtx",
                "mtx.bz2",
                "mtx.gz",
                "mtx.xz",
                "mtx.zip"
            )
        )
        match <- str_match(string = file, pattern = extPattern)
        compressExt <- match[1L, 4L]
        compress <- !is.na(compressExt)
        ## Inform the user regarding overwrite.
        if (isAFile(file)) {
            if (isTRUE(overwrite) && !isTRUE(quiet)) {
                cli_alert_warning(
                    sprintf("Overwriting {.file %s}.", basename(file))
                )
            } else {
                stop(sprintf("File exists: %s", realpath(file)))
            }
        }
        ## Ensure directory is created automatically.
        initDir(dir = dirname(file))
        ## Remove compression extension from output file.
        if (isTRUE(compress)) {
            file <- sub(
                pattern = paste0("\\.", compressExt, "$"),
                replacement = "",
                x = file
            )
        }
        ## Export MatrixMarket file.
        if (!isTRUE(quiet)) {
            cli_alert(sprintf(
                "Exporting {.file %s} using {.pkg %s}::{.fun %s}.",
                basename(file), "Matrix", "writeMM"
            ))
        }
        writeMM(obj = object, file = file)
        ## Compress file, if necessary.
        if (isTRUE(compress)) {
            file <- compress(
                file = file,
                ext = compressExt,
                remove = TRUE,
                overwrite = TRUE
            )
        }
        ## Normalize the path.
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
            matrix = file,
            barcodes = barcodesFile,
            genes = featuresFile
        )
        assert(allAreFiles(files))
        ## Return named character of file paths.
        invisible(files)
    }

formals(`export,sparseMatrix`)[
    c(
        "dir",
        "ext",
        "overwrite",
        "quiet"
    )] <- formalsList[c(
        "export.dir",
        "export.sparse.ext",
        "overwrite",
        "quiet"
    )]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("sparseMatrix"),
    definition = `export,sparseMatrix`
)



`export,GenomicRanges` <- `export,DataFrame`  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("GenomicRanges"),
    definition = `export,GenomicRanges`
)



## Updated 2020-01-19.
.exportAssays <-  # nolint
    function(object, name, dir, compress, overwrite, quiet) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            isString(name),
            isString(dir),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        assayNames <- assayNames(object)
        assert(isCharacter(assayNames))
        cli_alert(sprintf("Exporting assays: {.var %s}.", toString(assayNames)))
        out <- lapply(
            X = assayNames,
            FUN = function(name, dir) {
                file <- file.path(dir, name)
                assay <- assay(x = object, i = name)
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
                export(
                    object = assay,
                    file = file,
                    overwrite = overwrite,
                    quiet = quiet
                )
            },
            dir = initDir(file.path(dir, "assays"))
        )
        names(out) <- assayNames
        out
    }



## Updated 2020-01-19.
.exportColData <-  # nolint
    function(object, ext, dir, overwrite, quiet) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            isString(ext),
            isString(dir),
            isFlag(overwrite),
            isFlag(quiet)
        )
        export(
            object = atomize(colData(object)),
            file = file.path(dir, paste0("colData", ext)),
            overwrite = overwrite,
            quiet = quiet
        )
    }



## NOTE: The standard `rowData()` output is okay but doesn't include genomic
## ranges coordinates. That's why we're coercing from `rowRanges()` for RSE.
## Updated 2020-01-19.
.exportRowData <-  # nolint
    function(object, ext, dir, overwrite, quiet) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            isString(ext),
            isString(dir),
            isFlag(overwrite),
            isFlag(quiet)
        )
        data <- rowData(object)
        ## Note that SummarizedExperiment in BioC 3.6/R 3.4 release doesn't
        ## set row names properly, so keep this step here for compatibility.
        if (!hasRownames(data)) {
            rownames(data) <- rownames(object)  # nocov
        }
        data <- atomize(data)
        data <- as.data.frame(data)
        assert(identical(rownames(data), rownames(object)))
        export(
            object = data,
            file = file.path(dir, paste0("rowData", ext)),
            overwrite = overwrite,
            quiet = quiet
        )
    }



## Updated 2020-01-19.
`export,SummarizedExperiment` <-  # nolint
    function(
        object,
        name = NULL,
        dir,
        compress,
        overwrite,
        quiet
    ) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isString(dir),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        call <- standardizeCall()
        ## Get the name and create directory substructure.
        if (is.null(name)) {
            sym <- call[["object"]]
            assert(is.symbol(sym))
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
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        ## Column data.
        files[["colData"]] <-
            .exportColData(
                object = object,
                ext = ext,
                dir = dir,
                overwrite = overwrite,
                quiet = quiet
            )
        ## Row data.
        files[["rowData"]] <-
            .exportRowData(
                object = object,
                ext = ext,
                dir = dir,
                overwrite = overwrite,
                quiet = quiet
            )
        cli_alert(sprintf("Exported {.envvar %s} to {.path %s}.", name, dir))
        ## Return named character of file paths.
        files <- Filter(Negate(is.null), files)
        assert(hasNames(files))
        invisible(files)
    }

formals(`export,SummarizedExperiment`)[
    c(
        "compress",
        "dir",
        "overwrite",
        "quiet"
    )] <- formalsList[c(
        "export.compress",
        "export.dir",
        "overwrite",
        "quiet"
    )]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SummarizedExperiment"),
    definition = `export,SummarizedExperiment`
)



## Updated 2020-01-19.
`export,SingleCellExperiment` <-  # nolint
    function(
        object,
        name = NULL,
        dir,
        compress,
        overwrite,
        quiet
    ) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isString(dir),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        call <- standardizeCall()
        ## Get the name and create directory substructure.
        if (is.null(name)) {
            sym <- call[["object"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
        }
        ## Export SummarizedExperiment-compatible slots.
        files <- export(
            object = as(object, "RangedSummarizedExperiment"),
            name = name,
            dir = dir,
            compress = compress,
            overwrite = overwrite,
            quiet = quiet
        )
        dir <- initDir(file.path(dir, name))
        ## Export dimensionality reduction data.
        reducedDimNames <- reducedDimNames(object)
        if (hasLength(reducedDimNames)) {
            if (!isTRUE(quiet)) {
                cli_alert(sprintf(
                    "Exporting {.var reducedDims}: {.var %s}",
                    toString(reducedDimNames)
                ))
            }
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
                    export(
                        object = reducedDim,
                        file = file,
                        overwrite = overwrite,
                        quiet = quiet
                    )
                },
                dir = initDir(file.path(dir, "reducedDims"))
            )
            names(files[["reducedDims"]]) <- reducedDimNames
        }
        assert(hasNames(files))
        invisible(files)
    }

formals(`export,SingleCellExperiment`)[
    c(
        "compress",
        "dir",
        "overwrite",
        "quiet"
    )] <- formalsList[c(
        "export.compress",
        "export.dir",
        "overwrite",
        "quiet"
    )]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SingleCellExperiment"),
    definition = `export,SingleCellExperiment`
)
