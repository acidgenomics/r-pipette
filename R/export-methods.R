#' @name export
#' @inherit AcidGenerics::export
#' @note Updated 2020-10-06.
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
#' Note that this function currently wraps `vroom::voom_write()` by default
#' for exporting `data.frame` and `matrix` class objects.
#'
#' @inheritParams AcidRoxygen::params
#' @param object Object.
#'   An object supporting [`dim()`][base::dim], or a supported class capable
#'   of being coerced to `data.frame`, to be written to disk.
#' @param ext `character(1)`.
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



#' @rdname export
#' @name export
#' @importFrom AcidGenerics export
#' @usage export(object, ...)
#' @export
NULL



## Updated 2020-08-12.
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
                choices = c("txt", "txt.bz2", "txt.gz", "txt.xz", "txt.zip")
            )
            dir <- initDir(dir)
            file <- file.path(dir, paste0(name, ".", ext))
        } else {
            initDir(dirname(file))
        }
        match <- str_match(string = file, pattern = extPattern)
        compressExt <- match[1L, 4L]
        compress <- !is.na(compressExt)
        if (isAFile(file)) {
            file <- realpath(file)
            if (isTRUE(overwrite) && !isTRUE(quiet)) {
                cli_alert_warning(sprintf(
                    fmt = "Overwriting {.file %s} at {.path %s}.",
                    basename(file), dirname(file)
                ))
            } else {
                stop(sprintf("File exists: '%s'", file))
            }
        }
        if (!isTRUE(quiet)) {
            cli_alert(sprintf(
                fmt = paste(
                    "Exporting {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), dirname(file),
                "readr", "write_lines"
            ))
        }
        write_lines(x = object, path = file, append = FALSE)
        if (isTRUE(compress)) {
            ## nocov start
            file <- compress(
                file = file,
                ext = compressExt,
                remove = TRUE,
                overwrite = TRUE
            )
            ## nocov end
        }
        file <- realpath(file)
        invisible(file)
    }

formals(`export,character`)[c("dir", "overwrite", "quiet")] <-
    formalsList[c("export.dir", "overwrite", "quiet")]



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
## Updated 2020-08-18.
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
            arg = getOption("acid.export.engine", default = "vroom"),
            choices = c("data.table", "readr", "vroom")
        )
        requireNamespaces(whatPkg)
        verbose <- getOption("acid.verbose", default = FALSE)
        assert(
            hasLength(object),
            hasRows(object),
            hasCols(object),
            hasNoDuplicates(colnames(object)),
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
            dir <- initDir(dir)
            file <- file.path(dir, paste0(name, ".", ext))
        } else {
            ext <- fileExt(file)
            initDir(dirname(file))
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
        ## Drop non-atomic columns automatically, if necessary.
        keep <- bapply(X = object, FUN = is.atomic)
        if (!all(keep)) {
            ## nocov start
            ## This is used to handle rowData with nested entrezIDs.
            fail <- names(keep)[!keep]
            cli_alert_warning(sprintf(
                "Dropping non-atomic columns: {.var %s}.",
                toString(fail, width = 200L)
            ))
            object <- object[, keep, drop = FALSE]
            ## nocov end
        }
        assert(allAreAtomic(object))
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
            if (isTRUE(overwrite) && !isTRUE(quiet)) {
                cli_alert_warning(sprintf(
                    fmt = "Overwriting {.file %s} at {.path %s}.",
                    basename(file), dirname(file)
                ))
            } else {
                stop(sprintf("File exists: '%s'", file))
            }
        }
        if (isTRUE(compress)) {
            file <- sub(
                pattern = paste0("\\.", compressExt, "$"),
                replacement = "",
                x = file
            )
        }
        if (identical(whatPkg, "data.table")) {
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
                fmt = paste(
                    "Exporting {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), dirname(file),
                whatPkg, whatFun
            ))
        }
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

formals(`export,matrix`)[
    c("dir", "ext", "overwrite", "quiet")] <-
    formalsList[c("export.dir", "export.ext", "overwrite", "quiet")]



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
## Updated 2020-08-11.
`export,sparseMatrix` <-  # nolint
    function(
        object,
        ext = "mtx",
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
            dir <- initDir(dir)
            file <- file.path(dir, paste0(name, ".", ext))
        } else {
            ext <- fileExt(file)
            initDir(dirname(file))
        }
        ext <- match.arg(
            arg = ext,
            choices = c("mtx", "mtx.bz2", "mtx.gz", "mtx.xz", "mtx.zip")
        )
        match <- str_match(string = file, pattern = extPattern)
        compressExt <- match[1L, 4L]
        compress <- !is.na(compressExt)
        if (isAFile(file)) {
            file <- realpath(file)
            if (isTRUE(overwrite) && !isTRUE(quiet)) {
                cli_alert_warning(sprintf(
                    fmt = "Overwriting {.file %s} at {.path %s}.",
                    basename(file), dirname(file)
                ))
            } else {
                stop(sprintf("File exists: %s", file))
            }
        }
        if (isTRUE(compress)) {
            file <- sub(
                pattern = paste0("\\.", compressExt, "$"),
                replacement = "",
                x = file
            )
        }
        if (!isTRUE(quiet)) {
            cli_alert(sprintf(
                fmt = paste(
                    "Exporting {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), dirname(file),
                "Matrix", "writeMM"
            ))
        }
        writeMM(obj = object, file = file)
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
            matrix = file,
            barcodes = barcodesFile,
            genes = featuresFile
        )
        assert(allAreFiles(files))
        invisible(files)
    }

formals(`export,sparseMatrix`)[
    c("dir", "overwrite", "quiet")] <-
    formalsList[c("export.dir", "overwrite", "quiet")]



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



## Updated 2020-08-11.
.exportAssays <-
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
        dir <- realpath(initDir(dir))
        cli_alert(sprintf(
            fmt = "Exporting assays {.var %s} to {.path %s}.",
            toString(assayNames), dir
        ))
        out <- lapply(
            X = assayNames,
            FUN = function(name, dir) {
                file <- file.path(dir, name)
                assay <- assay(x = object, i = name)
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



## Updated 2020-08-11.
.exportColData <-
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
## Updated 2020-08-11.
.exportRowData <-
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



## Ensure the assays list is always named. Note that valid SE objects don't have
## to contain named assays (e.g. DESeqTransform). In the event that an SE object
## contains a single, unnamed assay, we make sure to rename it internally to
## "assay" before exporting.
##
## Updated 2020-08-11.
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
        if (is.null(name)) {
            sym <- call[["object"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
        }
        dir <- initDir(file.path(dir, name))
        if (!isTRUE(quiet)) {
            cli_alert(sprintf(
                fmt = "Exporting {.envvar %s} to {.path %s}.",
                name, dir
            ))
        }
        files <- list()
        ext <- "csv"
        if (isTRUE(compress)) ext <- paste0(ext, ".gz")
        ext <- paste0(".", ext)
        if (is.null(assayNames(object))) {
            assayNames(object) <- "assay"
        }
        files[["assays"]] <-
            .exportAssays(
                object = object,
                name = name,
                dir = dir,
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        files[["colData"]] <-
            .exportColData(
                object = object,
                ext = ext,
                dir = dir,
                overwrite = overwrite,
                quiet = quiet
            )
        files[["rowData"]] <-
            .exportRowData(
                object = object,
                ext = ext,
                dir = dir,
                overwrite = overwrite,
                quiet = quiet
            )
        files <- Filter(Negate(is.null), files)
        assert(hasNames(files))
        invisible(files)
    }

formals(`export,SummarizedExperiment`)[
    c("compress", "dir", "overwrite", "quiet")] <-
    formalsList[c("export.compress", "export.dir", "overwrite", "quiet")]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SummarizedExperiment"),
    definition = `export,SummarizedExperiment`
)



## Updated 2020-08-11.
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
        ## Export dimensionality reduction data.
        dir <- initDir(file.path(dir, name))
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
    c("compress", "dir", "overwrite", "quiet")] <-
    formalsList[c("export.compress", "export.dir", "overwrite", "quiet")]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SingleCellExperiment"),
    definition = `export,SingleCellExperiment`
)
