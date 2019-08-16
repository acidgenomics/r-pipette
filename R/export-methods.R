#' @name export
#' @inherit bioverbs::export
#' @note Updated 2019-08-16.
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
#' @param slotNames `character`.
#'   Names of slots to include when writing to disk.
#' @param ... Additional arguments.
#'
#' @return Invisible `character`.
#' File path(s).
#'
#' @seealso
#' - [rtracklayer::export()].
#' - [rio::export()].
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



## matrix ======================================================================
## This method covers standard `matrix` but is also intended to work for
## `data.table`, `tbl_df`, and `DataFrame` classes. Note that `rio::export()`
## does not preserve row names by default, so we're ensuring row names get
## coerced to "rowname" column consistently here.
## Updated 2019-07-16.
`export,matrix` <-  # nolint
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
            choices = c("csv", "csv.gz", "tsv", "tsv.gz")
        )

        ## Keep the `as.data.frame()` call here, so we can inherit the
        ## `data.frame` method in other S4 methods.
        object <- as.data.frame(object)

        ## Ensure row names are automatically moved to `rowname` column.
        if (hasRownames(object)) {
            rownames <- "rowname"
        } else {
            rownames <- NULL
        }

        ## Now we're ready to coerce to tibble internally, which helps us
        ## move the row names into a column.
        object <- as_tibble(object, rownames = rownames)
        assert(hasRows(object), hasCols(object))

        ## Define the file name using the object name by default.
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
        }
        assert(isString(file))

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

        ## Now attach rio package and call `export()` on our data frame.
        requireNamespace("rio", quietly = TRUE)
        suppressMessages(
            file <- do.call(
                what = rio::export,
                args = list(x = object, file = file)
            )
        )

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



## data.frame ==================================================================
`export,data.frame` <- `export,matrix`  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("data.frame"),
    definition = `export,data.frame`
)



## DataFrame ===================================================================
`export,DataFrame` <- `export,data.frame`  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("DataFrame"),
    definition = `export,DataFrame`
)



## sparseMatrix ================================================================
## Note that "file" is referring to the matrix file.
## The correponding column and row sidecar files are generated automatically.
## Consider adding HDF5 support in a future update.
## Updated 2019-08-15.
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
        ext <- match.arg(arg = ext, choices = c("mtx", "mtx.gz"))

        ## Define the file name using the object name by default.
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
        }
        assert(isString(file))

        ## Inform the user regarding overwrite.
        if (isAFile(file)) {
            if (isTRUE(overwrite)) {
                message(sprintf("Overwriting '%s'.", basename(file)))
            } else {
                stop(sprintf("File exists: %s", realpath(file)))
            }
        }

        ## Determine whether we want to gzip compress.
        gzip <- grepl(pattern = "\\.gz$", x = file)

        ## Now ensure ".gz" is stripped from the working file variable.
        file <- sub(pattern = "\\.gz", replacement = "", x = file)

        ## Create the recursive directory structure, if necessary.
        initDir(dirname(file))

        ## MatrixMarket file.
        writeMM(obj = object, file = file)

        if (isTRUE(gzip)) {
            file <- gzip(file, overwrite = TRUE)
        }

        ## Normalize the path.
        file <- realpath(file)

        ## Write barcodes (colnames).
        barcodes <- colnames(object)
        barcodesFile <- paste0(file, ".colnames")
        writeLines(text = barcodes, con = barcodesFile)

        ## Write features (rownames).
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



## GRanges =====================================================================
`export,GRanges` <- `export,DataFrame`  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("GRanges"),
    definition = `export,GRanges`
)



## SummarizedExperiment ========================================================
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



## Require at least 1 of the slotNames to be defined for export.
## `rowData` is a supported slot but is actually defined in `rowRanges`.
## Note that we're not using `match.arg()` here for `slotNames`.
## Updated 2019-07-19.
`export,SummarizedExperiment` <-  # nolint
    function(
        object,
        name = NULL,
        dir,
        compress,
        slotNames = c("assays", "colData", "rowData")
    ) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isString(dir),
            isFlag(compress),
            isCharacter(slotNames),
            isSubset(
                x = slotNames,
                y = c(slotNames(object), "rowData")
            )
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

        ## Assays (count matrices).
        if ("assays" %in% slotNames) {
            files[["assays"]] <-
                .exportAssays(
                    object = object,
                    name = name,
                    dir = dir,
                    compress = compress
                )
        }

        ## Column annotations.
        if ("colData" %in% slotNames) {
            files[["colData"]] <-
                .exportColData(
                    object = object,
                    ext = ext,
                    dir = dir
                )
        }

        ## Row annotations.
        if ("rowData" %in% slotNames) {
            files[["rowData"]] <-
                .exportRowData(
                    object = object,
                    ext = ext,
                    dir = dir
                )
        }

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



## Updated 2019-07-19.
`export,SingleCellExperiment` <-  # nolint
    function(object) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isString(dir),
            isFlag(compress),
            isCharacter(slotNames),
            isSubset(
                x = slotNames,
                y = c(slotNames(object), "rowData")
            )
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

        ## Primarily use SE method to export.
        se <- as(object, "RangedSummarizedExperiment")
        assign(x = name, value = se)
        args <- matchArgsToDoCall()
        args[["object"]] <- as.name(name)
        ## We're handling `reducedDims` specially below.
        args[["slotNames"]] <- setdiff(args[["slotNames"]], "reducedDims")
        files <- do.call(what = export, args = args)

        reducedDimNames <- reducedDimNames(object)
        if (
            isSubset("reducedDims", slotNames) &&
            hasLength(reducedDimNames)
        ) {
            message(sprintf(
                "Exporting reducedDims: %s",
                toString(reducedDimNames)
            ))
            files[["reducedDims"]] <- lapply(
                X = reducedDimNames,
                FUN = function(name, dir) {
                    file <- file.path(dir, name)
                    reducedDim <- reducedDims(object)[[name]]
                    if (is(reducedDim, "matrix")) {
                        ext <- "csv"
                    } else if (is(reducedDim, "sparseMatrix")) {         # nocov
                        ext <- "mtx"                                     # nocov
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

f <- formals(`export,SummarizedExperiment`)
f[["slotNames"]] <- c(eval(f[["slotNames"]]), "reducedDims")
formals(`export,SingleCellExperiment`) <- f



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SingleCellExperiment"),
    definition = `export,SingleCellExperiment`
)
