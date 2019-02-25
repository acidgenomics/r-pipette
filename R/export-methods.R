#' @name export
#' @inherit bioverbs::export
#' @inheritParams params
#'
#' @section Row names:
#'
#' Some export utilities in R have a tendency to drop row names when writing to
#' disk in CSV format. For example, the [readr][] family of functions never
#' write row names by design. This is a *really poor* default setting for
#' handling genomic data, which often contain gene identifiers in the row names.
#' Here we're performing any internal tibble coercion step to ensure row names
#' are always moved to a "`rowname`" column in the CSV export.
#'
#' [readr]: https://readr.tidyverse.org/
#'
#' @section Exporting vectors:
#'
#' Use [`writeLines()`][base::writeLines] instead of [export()] to write vectors
#' to disk. An S4 character method may be defined in a future update, but it is
#' intentionally unsupported in the current release.
#'
#' @param x `ANY`.
#'   An object supporting [`dim()`][base::dim], to be written to disk.
#' @param file `character(1)`.
#'   File path. Specify `file` or `format` but not both.
#' @param format `character(1)`.
#'   An optional character string containing the
#'   file format, which can be used to override the format inferred from `file`,
#'   or in lieu of specifying `file`.
#' @param compress `logical(1)`.
#'   Apply gzip compression to all files.
#' @param name `character(1)`.
#'   Name to use on disk. If `NULL`, will use the name of the object instead.
#' @param slotNames `character`.
#'   Names of slots to include when writing to disk.
#'
#' @return Invisible `character`.
#' File path(s).
#'
#' @seealso [rio::export()].
#'
#' @examples
#' counts <- matrix(data = seq_len(100L), nrow = 10)
#' export(counts, format = "csv")
#'
#' ## Clean up.
#' file.remove("counts.csv")
NULL



#' @importFrom bioverbs export
#' @aliases NULL
#' @export
bioverbs::export



# Don't export an ANY method here. We want to keep the method support tight
# and working consistently for only expected data classes.

# Future support:
# - DataFrameList
# - GRangesList



# data.frame ===================================================================
# This method covers standard data.frame but is also intended to work for
# data.table and tbl_df classes. Note that `rio::export()` does not preserve
# rownames by default, so we're ensuring rownames get coerced to "rowname"
# column consistently.
export.data.frame <-  # nolint
    function(x, file, format, ...) {
        # Keep the `as.data.frame()` call here, so we can inherit the
        # `data.frame` method in other S4 methods.
        x <- as.data.frame(x)

        # Ensure row names are automatically moved to `rowname` column.
        if (hasRownames(x)) {
            rownames <- "rowname"
        } else {
            rownames <- NULL
        }

        # Now we're ready to coerce to tibble internally, which helps us
        # move the row names into a column.
        x <- as_tibble(x, rownames = rownames)
        assert(hasRows(x), hasCols(x))

        # Juggle the file and format arguments, like rio package.
        # Specify one but not both.
        if (missing(file) && missing(format)) {
            stop("Must specify `file` and/or `format`.", call. = FALSE)
        } else if (missing(file)) {
            call <- standardizeCall()
            sym <- call[["x"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
            assert(isString(format))
            file <- paste0(name, ".", format)
        } else if (missing(format)) {
            assert(isString(file))
        }

        # Ensure directory is created automatically.
        initDir(dir = dirname(file))

        # Now attach rio package and call `export()` on our data frame.
        requireNamespace("rio", quietly = TRUE)
        suppressMessages(
            file <- do.call(
                what = rio::export,
                args = list(x = x, file = file, ...)
            )
        )

        file <- realpath(file)
        message(paste0("Exported ", basename(file), "."))
        invisible(file)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("data.frame"),
    definition = export.data.frame
)



# DataFrame ====================================================================
export.DataFrame <- export.data.frame  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("DataFrame"),
    definition = export.DataFrame
)



# matrix =======================================================================
export.matrix <- export.DataFrame  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("matrix"),
    definition = export.matrix
)



# sparseMatrix =================================================================
# Note that "file" is referring to the matrix file.
# The correponding column and row sidecar files are generated automatically.
# Consider adding HDF5 support in a future update.
export.sparseMatrix <-  # nolint
    function(x, file, format) {
        assert(hasLength(x))
        choices <- c("mtx", "mtx.gz")

        if (missing(file) && missing(format)) {
            stop("Must specify `file` and/or `format`.", call. = FALSE)
        } else if (missing(file)) {
            call <- standardizeCall()
            sym <- call[["x"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
            format <- match.arg(format, choices)
            file <- paste0(name, ".", format)
        } else if (missing(format)) {
            assert(isString(file))
            # Require a valid extension.
            grepChoices <- paste0("\\.", choices, "$")
            assert(any(vapply(
                X = grepChoices,
                FUN = grepl,
                FUN.VALUE = logical(1L),
                x = file
            )))
        }

        # Determine whether we want to gzip compress.
        gzip <- grepl("\\.gz$", file)

        # Now ensure ".gz" is stripped from the working file variable.
        file <- sub("\\.gz", "", file)

        # Create the recursive directory structure, if necessary.
        initDir(dirname(file))

        # MatrixMarket file.
        writeMM(obj = x, file = file)

        if (isTRUE(gzip)) {
            file <- gzip(file, overwrite = TRUE)
        }

        # Normalize the path.
        file <- realpath(file)

        # Write barcodes (colnames).
        barcodes <- colnames(x)
        barcodesFile <- paste0(file, ".colnames")
        write_lines(x = barcodes, path = barcodesFile)

        # Write gene names (rownames).
        genes <- rownames(x)
        genesFile <- paste0(file, ".rownames")
        write_lines(x = genes, path = genesFile)

        files <- c(
            matrix = file,
            barcodes = barcodesFile,
            genes = genesFile
        )

        message(paste0("Exported ", toString(basename(files)), "."))

        # Return named character of file paths.
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("sparseMatrix"),
    definition = export.sparseMatrix
)



# GRanges ======================================================================
export.GRanges <- export.DataFrame  # nolint



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("matrix"),
    definition = export.GRanges
)



# SummarizedExperiment =========================================================
.export.assays <-  # nolint
    function(x, name, dir, compress) {
        assayNames <- assayNames(x)
        assert(isCharacter(assayNames))
        message(paste("Exporting assays:", toString(assayNames)))
        out <- lapply(
            X = assayNames,
            FUN = function(name, dir) {
                file <- file.path(dir, name)
                assay <- assays(x)[[name]]

                # Dynamically set the file name based on the assay type.
                if (is(assay, "sparseMatrix")) {
                    format <- "mtx"
                } else {
                    format <- "csv"
                }
                if (isTRUE(compress)) {
                    format <- paste0(format, ".gz")
                }
                file <- paste0(file, ".", format)

                export(assay, file = file)
            },
            dir = initDir(file.path(dir, "assays"))
        )
        names(out) <- assayNames
        out
    }



.export.colData <-  # nolint
    function(x, ext, dir) {
        export(
            x = atomize(colData(x)),
            file = file.path(dir, paste0("colData", ext))
        )
    }



# NOTE: The standard `rowData` output is okay but doesn't include genomic
# ranges coordinates. That's why we're coercing from `rowRanges` for RSE.
.export.rowData <-  # nolint
    function(x, ext, dir) {
        if (is(x, "RangedSummarizedExperiment")) {
            data <- rowRanges(x)
        } else {
            data <- rowData(x)
        }
        data <- atomize(data)
        # Coerce the GRanges to a standard data.frame.
        data <- as.data.frame(data)
        assert(identical(rownames(data), rownames(x)))
        export(x = data, file = file.path(dir, paste0("rowData", ext)))
    }



export.SummarizedExperiment <-  # nolint
    function(
        x,
        name = NULL,
        dir = ".",
        compress = FALSE,
        slotNames = c("assays", "colData", "rowData")
    ) {
        call <- standardizeCall()
        assert(isString(name, nullOK = TRUE))
        if (is.null(name)) {
            name <- as.character(call[["x"]])
        }
        dir <- initDir(file.path(dir, name))
        assert(
            isFlag(compress),
            isCharacter(slotNames),
            # Require at least 1 of the slotNames to be defined for export.
            # Note that we're not using `match.arg` here.
            isSubset(x = slotNames, y = eval(formals()[["slotNames"]]))
        )

        # Return the file paths back to the user as a named list.
        files <- list()

        # Set the desired output extension, depending on whether we need to
        # enable compression. Always output consistently in CSV format.
        format <- "csv"
        if (isTRUE(compress)) {
            format <- paste0(format, ".gz")
        }
        ext <- paste0(".", format)

        # Ensure the assays list is always named. Note that valid SE objects
        # don't have to contain named assays (e.g. DESeqTransform). In the event
        # that an SE object contains a single, unnamed assay, we make sure to
        # rename it internally to "assay" before exporting.
        if (is.null(assayNames(x))) {
            assayNames(x) <- "assay"
        }

        # Assays (count matrices).
        if ("assays" %in% slotNames) {
            files[["assays"]] <-
                .export.assays(
                    x = x, name = name, dir = dir, compress = compress
                )
        }

        # Column annotations.
        if ("colData" %in% slotNames) {
            files[["colData"]] <-
                .export.colData(x = x, ext = ext, dir = dir)
        }

        # Row annotations.
        if ("rowData" %in% slotNames) {
            files[["rowData"]] <-
                .export.rowData(x = x, ext = ext, dir = dir)
        }

        message(paste0("Exported ", name, " to ", dir, "."))

        # Return named character of file paths.
        files <- Filter(Negate(is.null), files)
        assert(hasNames(files))
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SummarizedExperiment"),
    definition = export.SummarizedExperiment
)



export.SingleCellExperiment <-  # nolint
    function(x) {
        assert(isFlag(compress))
        call <- standardizeCall()
        name <- as.character(call[["x"]])

        # Primarily use SE method to export.
        se <- as(x, "RangedSummarizedExperiment")

        assign(x = name, value = se)
        args <- matchArgsToDoCall()
        args[["x"]] <- as.name(name)
        files <- do.call(what = export, args = args)
        print(files)

        reducedDimNames <- reducedDimNames(x)
        if (length(reducedDimNames) == 0L) {
            message(paste("Exporting reducedDims:", toString(reducedDimNames)))
            files[["reducedDims"]] <- lapply(
                X = reducedDimNames,
                FUN = function(name, dir) {
                    file <- file.path(dir, name)
                    reducedDim <- reducedDims(x)[[name]]
                    if (is(reducedDim, "matrix")) {
                        format <- "csv"
                    } else if (is(reducedDim, "sparseMatrix")) {
                        format <- "mtx"
                    }
                    if (isTRUE(compress)) {
                        format <- paste0(format, ".gz")
                    }
                    file <- paste0(file, ".", format)
                    export(reducedDim, file = file)
                },
                dir = initDir(file.path(dir, name, "reducedDims"))
            )
            names(files[["reducedDims"]]) <- reducedDimNames
        }

        assert(hasNames(files))
        invisible(files)
    }

formals(export.SingleCellExperiment) <- formals(export.SummarizedExperiment)



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SingleCellExperiment"),
    definition = export.SingleCellExperiment
)
