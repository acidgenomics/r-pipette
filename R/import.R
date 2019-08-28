#' Import
#'
#' Read file by extension into R.
#'
#' [import()] supports automatic loading of common file types, by wrapping
#' popular importer functions. The function is intentionally designed to be
#' simple. Remote URLs and compressed files are supported. If you need more
#' complex import settings, just call the wrapped importer directly instead.
#'
#' @note Updated 2019-08-22.
#' @export
#'
#' @section Row and column names:
#'
#' **Row names.** Row name handling has become an inconsistent mess in R because
#' of differential support in base R, tidyverse, data.table, and Bioconductor.
#' To maintain sanity, [import()] attempts to handle row names automatically.
#' The function checks for a `rowname` column in delimited data, and moves these
#' values into the object's row names, if supported by the return type (e.g.
#' `data.frame`, `DataFrame`). Note that `tbl_df` (tibble) and `data.table`
#' intentionally do not support row names. When returning in this format, no
#' attempt to assign the `rowname` column into the return object's row names is
#' made. Note that [import()] is strict about this matching and only checks for
#' a `rowname` column, similar to the default syntax recommended in
#' [tibble::rownames_to_column()]. To disable this behavior, set
#' `rownames = FALSE`, and no attempt will be made to set the row names.
#'
#' **Column names.** [import()] assumes that delimited files always contain
#' column names. If you are working with a file that doesn't contain column
#' names, either set `colnames = FALSE` or pass the names in as a `character`
#' vector. It's strongly recommended to always define column names in a
#' supported file type.
#'
#' @section Data frame return:
#'
#' By default, [import()] returns a standard `data.frame` for delimited/column
#' formatted data. However, any of these desired output formats can be set
#' globally using `options(acid.data.frame = "data.frame")`.
#'
#' Supported return types:
#'
#' - `data.frame`: Base R default. Generally recommended.
#'   - S3 class.
#'   - Allows rownames, but they're required and can't be set `NULL`.
#'   - See `help(topic = "data.frame", package = "base")` for details.
#' - `DataFrame`: Recommended when working with Bioconductor packages.
#'   - S4 class; inherits `DataTable` virtual class.
#'   - Allows rownames, but they're optional and can be set `NULL`.
#'   - See `help(topic = "DataFrame", package = "S4Vectors")` for details.
#' - `tbl_df` (`tibble`): Recommended when working with tidyverse packages.
#'   - S3 class; inherits `data.frame`.
#'   - Does not allow rownames.
#'   - See `help(topic = "tibble", package = "tibble")` for details.
#' - `data.table`: Recommended when working with the data.table package.
#'   - S3 class; inherits `data.frame`.
#'   - Does not allow rownames.
#'   - See `help(topic = "data.table", package = "data.table")` for details.
#'
#' Note that `stringsAsFactors` is always disabled for import.
#'
#' @section Matrix Market Exchange (MTX):
#'
#' Reading a Matrix Market Exchange file requires `ROWNAMES` and `COLNAMES`
#' sidecar files containing the corresponding row and column names of the sparse
#' matrix.
#'
#' @section General feature format (GFF, GTF):
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' [basejump][] exports the specialized `makeGRangesFromGFF()` function that
#' makes GFF loading simple.
#'
#' See also:
#'
#' - [Ensembl spec](http://www.ensembl.org/info/website/upload/gff.html)
#' - [GENCODE spec](http://www.gencodegenes.org/gencodeformat.html)
#'
#' [basejump]: https://steinbaugh.com/basejump/
#'
#' @section Gene sets (GMT, GMX):
#'
#' Refer to the Broad Institute [GSEA wiki][] for details.
#'
#' [GSEA wiki]: https://goo.gl/3ZkDPb
#'
#' @section bcbio count matrix:
#'
#' [bcbio][] count matrix and related sidecar files are natively supported.
#'
#' - `COUNTS`: Counts table (e.g. RNA-seq aligned counts).
#' - `COLNAMES`: Sidecar file containing column names.
#' - `ROWNAMES`: Sidecar file containing row names.
#'
#' [bcbio]: https://bcbio-nextgen.readthedocs.io/
#'
#' @section Blacklisted extensions:
#'
#' These file formats are blacklisted, and intentionally not supported:
#' `DOC`, `DOCX`, `PDF`, `PPT`, `PPTX`.
#'
#' @inheritParams acidroxygen::params
#' @param rownames `logical(1)`.
#'   Automatically assign row names, if `rowname` column is defined.
#'   Applies to file types that return `data.frame` only.
#' @param colnames `logical(1)` or `character`.
#'   Automatically assign column names, using the first header row.
#'   Applies to file types that return `data.frame` only.
#'   Pass in a `character` vector to define the column names manually.
#' @param format `character(1)`.
#'   An optional file format code, which can be used to override the format
#'   inferred from `file`. *Not recommended by default.*
#' @param sheet `character(1)` or `integer(1)`.
#'   *Applies to Excel Workbook, Google Sheet, or GraphPad Prism file.*
#'   Sheet to read. Either a string (the name of a sheet), or an integer (the
#'   position of the sheet). Defaults to the first sheet.
#' @param setclass `character(1)`.
#'   Class to set on data frame return.
#'   Options: `data.frame` (default), `DataFrame`, `tbl_df`, `data.table`.
#'
#' @return Varies, depending on the file type.
#'
#' - **Plain text delimited** (`CSV`, `TSV`, `TXT`): `data.frame`.\cr
#'   Data separated by commas, tabs, or visual spaces.\cr
#'   Note that TXT structure is amgibuous and actively discouraged.\cr
#'   Refer to `Data frame return` section for details on how to change the
#'   default return type to `DataFrame`, `tbl_df` or `data.table`.\cr
#'   Imported by [data.table::fread()].
#' - **Excel workbook** (`XLSB`, `XLSX`): `data.frame`.\cr
#'   Resave in plain text delimited format instead, if possible.\cr
#'   Imported by [readxl::read_excel()].
#' - **Legacy Excel workbook (pre-2007)** (`XLS`): `data.frame`.\cr
#'   Resave in plain text delimited format instead, if possible.\cr
#'   Note that import of files in this format is slow.\cr
#'   Imported by [gdata::read.xls()].
#' - **GraphPad Prism project** (`PZFX`): `data.frame`.\cr
#'   Experimental. Consider resaving in CSV format instead.\cr
#'   Imported by [pzfx::read_pzfx()].
#' - **General feature format** (`GFF`, `GFF1`, `GFF2`, `GFF3`, `GTF`):
#'   `GRanges`.\cr
#'   Imported by [rtracklayer::import()].
#' - **MatrixMarket exchange sparse matrix** (`MTX`): `sparseMatrix`.\cr
#'   Imported by [Matrix::readMM()].
#' - **Gene sets (for GSEA)** (`GMT`, `GMX`): `character`.
#' - **Browser extensible data** (`BED`, `BED15`, `BEDGRAPH`, `BEDPE`):
#'   `GRanges`.\cr
#'   Imported by [rtracklayer::import()].
#' - **ChIP-seq peaks** (`BROADPEAK`, `NARROWPEAK`): `GRanges`.\cr
#'   Imported by [rtracklayer::import()].
#' - **Wiggle track format** (`BIGWIG`, `BW`, `WIG`): `GRanges`.\cr
#'   Imported by [rtracklayer::import()].
#' - **JSON serialization data** (`JSON`): `list`.\cr
#'   Imported by [jsonlite::read_json()].
#' - **YAML serialization data** (`YAML`, `YML`): `list`.\cr
#'   Imported by [yaml::yaml.load_file()].
#' - **Lines** (`LOG`, `MD`, `PY`, `R`, `RMD`, `SH`): `character`.
#'   Source code or log files.\cr
#'   Imported by [`readLines()`][base::readLines].
#' - **R data serialized** (`RDS`): *variable*.\cr
#'   Currently recommend over RDA, if possible.\cr
#'   Imported by [`readRDS()`][base::readRDS].
#' - **R data** (`RDA`, `RDATA`): *variable*.\cr
#'   Must contain a single object.
#'   Doesn't require internal object name to match, unlike [loadData()].\cr
#'   Imported by [`load()`][base::load].
#' - **Infrequently used rio-compatible formats** (`ARFF`, `DBF`, `DIF`, `DTA`,
#'   `MAT`, `MTP`, `ODS`, `POR`, `SAS7BDAT`, `SAV`, `SYD`, `REC`, `XPT`):
#'   *variable*.\cr
#'   Imported by [rio::import()].
#'
#' @seealso
#' Packages:
#'
#' - [rio](https://cran.r-project.org/package=rio).
#' - [rtracklayer](http://bioconductor.org/packages/rtracklayer/).
#' - [data.table](http://r-datatable.com/).
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#'
#' Importer functions:
#'
#' - `rtracklayer::import()`.
#' - `rio::import()`.
#' - `data.table::fread()`.
#' - `readr::read_csv()`.
#' - `utils::read.table()`.
#'
#' @examples
#' file <- system.file("extdata/example.csv", package = "brio")
#'
#' ## Row and column names enabled.
#' x <- import(file)
#' print(head(x))
#'
#' ## Row and column names disabled.
#' x <- import(file, rownames = FALSE, colnames = FALSE)
#' print(head(x))
import <- function(
    file,
    rownames = TRUE,
    colnames = TRUE,
    format = "none",
    sheet = 1L,
    setclass = getOption("acid.data.frame", default = "data.frame")
) {
    ## We're supporting remote files, so don't check using `isAFile()` here.
    assert(
        isAFile(file) || isAURL(file),
        isScalar(sheet),
        isFlag(rownames),
        isFlag(colnames) || isCharacter(colnames)
    )
    format <- match.arg(
        arg = format,
        choices = c("none", "csv", "tsv", "txt", "lines")
    )
    setclass <- match.arg(
        arg = setclass,
        choices = c(
            "DataFrame",
            "data.frame",
            "data.table",
            "tbl_df"
        )
    )
    ## Allow Google Sheets import using rio, by matching the URL.
    ## Otherwise, coerce the file extension to uppercase, for easy matching.
    if (identical(format, "none")) {
        ext <- str_match(basename(file), extPattern)[1L, 2L]
    } else {
        ext <- format
    }
    ext <- toupper(ext)
    if (isSubset(ext, c("CSV", "FWF", "PSV", "TSV", "TXT"))) {
        object <- .importDelim(file, colnames = colnames)
    } else if (identical(ext, "XLS")) {
        object <- .importXLS(file, sheet = sheet, colnames = colnames)
    } else if (isSubset(ext, c("XLSB", "XLSX"))) {
        object <- .importXLSX(file, sheet = sheet, colnames = colnames)
    } else if (identical(ext, "PZFX")) {
        ## GraphPad Prism project.
        ## Note that Prism files always contain column names.
        object <- .importPZFX(file, sheet = sheet)
    } else if (identical(ext, "RDS")) {
        object <- .importRDS(file)
    } else if (isSubset(ext, c("RDA", "RDATA"))) {
        object <- .importRDA(file)
    } else if (identical(ext, "GMT")) {
        object <- .importGMT(file)
    } else if (identical(ext, "GMX")) {
        object <- .importGMX(file)
    } else if (identical(ext, "GRP")) {
        object <- .importGRP(file)
    } else if (identical(ext, "JSON")) {
        object <- .importJSON(file)
    } else if (isSubset(ext, c("YAML", "YML"))) {
        object <- .importYAML(file)
    } else if (identical(ext, "MTX")) {
        ## We're always requiring row and column sidecar files for MTX.
        object <- .importMTX(file)
    } else if (identical(ext, "COUNTS")) {
        ## bcbio counts format always contains row and column names.
        object <- .importBCBCounts(file)
    } else if (isSubset(ext, c("LINES", "LOG", "MD", "PY", "R", "RMD", "SH"))) {
        object <- .importLines(file)
    } else if (isSubset(ext, c(
        "BED", "BED15", "BEDGRAPH", "BEDPE",
        "BROADPEAK", "NARROWPEAK",
        "GFF", "GFF1", "GFF2", "GFF3", "GTF",
        "BIGWIG", "BW", "WIG"
    ))) {
        object <- .rtracklayerImport(file)
    } else if (isSubset(ext, c(
        "ARFF",      # Weka Attribute-Relation File Format
        "DBF",       # dBase Database File
        "DIF",       # Data Interchange Format
        "DTA",       # Stata
        "MAT",       # Matlab
        "MTP",       # Minitab
        "ODS",       # OpenDocument (LibreOffice)
        "POR",       # SPSS
        "SAS7BDAT",  # SASS
        "SAV",       # SPSS
        "SYD",       # Systat
        "REC",       # Epi Info
        "XPT"        # SASS
    ))) {
        object <- .rioImport(file)
    } else {
        stop(sprintf(
            "Import of '%s' failed. '%s' extension is not supported.",
            basename(file), ext
        ))
    }
    if (is.data.frame(object)) {
        ## Coerce data frame to desired global output, if necessary.
        object <- switch(
            EXPR = setclass,
            "data.frame" = object,
            "DataFrame" = as(object, "DataFrame"),
            "tbl_df" = as_tibble(
                x = object,
                .name_repair = "minimal",
                rownames = NULL
            ),
            "data.table" = as.data.table(
                x = object,
                keep.rownames = FALSE
            )
        )
        ## Set row names automatically, if supported.
        if (
            isAny(object, c("data.frame", "DataFrame")) &&
            !isAny(object, c("data.table", "tbl_df")) &&
            isSubset("rowname", colnames(object)) &&
            isTRUE(rownames)
        ) {
            message("Setting row names from 'rowname' column.")
            rownames(object) <- object[["rowname"]]
            object[["rowname"]] <- NULL
        }
    }
    ## Slot data provenance metadata into object.
    ## Skipping this step for vectors (i.e. source code lines).
    if (!is.atomic(object)) {
        newMeta <- list(
            brio = packageVersion("brio"),
            file = if (isAFile(file)) {
                realpath(file)
            } else {
                file
            },
            date = Sys.Date(),
            call = standardizeCall()
        )
        if (isS4(object) && isSubset("metadata", slotNames(object))) {
            meta <- metadata(object)[["brio"]]
            meta <- c(meta, newMeta)
            meta <- meta[sort(names(meta))]
            metadata(object)[["brio"]] <- meta
        } else {
            meta <- attr(object, "brio")
            meta <- c(meta, newMeta)
            meta <- meta[sort(names(meta))]
            attr(object, "brio") <- meta
        }
    }
    ## Check for syntactically valid names and inform the user, if necessary.
    if (
        (hasNames(object) && !hasValidNames(object)) ||
        (hasDimnames(object) && !hasValidDimnames(object))
    ) {
        ## nocov start
        message(sprintf(
            "'%s' does not contain syntactically valid names.",
            basename(file)
        ))
        ## nocov end
    }
    ## Inform the user when encountering duplicate names. This `tryCatch()` step
    ## here helps suppress `validObject()` error for invalid
    ## SummarizedExperiment objects.
    names <- tryCatch(
        expr = names(object),
        error = function(e) e
    )
    if (isCharacter(names)) {
        dupes <- duplicated(names)
        if (any(dupes)) {
            ## nocov start
            dupes <- sort(unique(names[dupes]))
            warning(sprintf(
                "%d duplicate %s: %s",
                length(dupes),
                ngettext(
                    n = length(dupes),
                    msg1 = "name",
                    msg2 = "names"
                ),
                toString(dupes, width = 200L)
            ))
            ## nocov end
        }
    }
    object
}



## Slot data provenance metadata.
.slotMetadata <- function(object, pkg, fun) {
    assert(isString(pkg), isString(fun))
    importer <- paste0(pkg, "::", fun)
    version <- packageVersion(pkg)
    if (isS4(object) && "metadata" %in% slotNames(object)) {
        metadata(object)[["brio"]][["importer"]] <- importer
        metadata(object)[["brio"]][[pkg]] <- version
    } else {
        ## Use `attr()` instead of `attributes()` here. It doesn't error on
        ## assignment when the object doesn't already have attributes.
        attr(object, "brio")[["importer"]] <- importer
        attr(object, "brio")[[pkg]] <- version
    }
    object
}



## Basic =======================================================================
## Internal importer for a delimited file (e.g. `.csv`, `.tsv`).
## Calls [data.table::fread()] internally.
.importDelim <- function(file, colnames = TRUE) {
    assert(isFlag(colnames) || isCharacter(colnames))
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "data.table::fread"
    ))
    args <- list(
        file = file,
        blank.lines.skip = TRUE,
        ## Don't attempt to adjust names using `make.names()`.
        ## We check for this later downstream.
        check.names = FALSE,
        ## Return as `data.frame`.
        data.table = FALSE,
        ## This handles mismatched columns more gracefully.
        ## Example: c_elegans.PRJNA13758.WS269.pcr_product2gene.txt.gz
        fill = TRUE,
        ## Sanitize NA columns, with our improved defaults.
        na.strings = naStrings,
        ## Always import starting from first line.
        skip = 0L,
        showProgress = FALSE,
        ## Never set factors on import automatically.
        stringsAsFactors = FALSE,
        strip.white = TRUE,
        verbose = FALSE
    )
    if (isCharacter(colnames)) {
        args[["header"]] <- FALSE
        args[["col.names"]] <- colnames
    } else {
        args[["header"]] <- colnames
    }
    object <- do.call(what = fread, args = args)
    assert(is.data.frame(object))
    object <- .slotMetadata(object, pkg = "data.table", fun = "fread")
    object
}



## Internal importer for (source code) lines.
.importLines <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "base::readLines"
    ))
    con <- file(file)
    object <- readLines(con = con)
    close(con)
    object
}



## R data ======================================================================
## Internal importer for an R data serialized file (`.rds`).
.importRDS <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file),
        "base::readRDS"
    ))
    object <- readRDS(file)
    object
}



## Internal importer for an R data file (`.rda`).
.importRDA <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "base::load"
    ))
    safe <- new.env()
    object <- load(file, envir = safe)
    if (length(safe) != 1L) {
        stop("File does not contain a single object.")
    }
    object <- get(object, envir = safe, inherits = FALSE)
    object
}



## Sparse matrix ===============================================================
## Internal importer for a sparse matrix file (`.mtx`).
.importMTX <- function(file) {
    assert(isString(file))
    ## Add the rownames automatically using `.rownames` sidecar file.
    rownamesFile <- paste(file, "rownames", sep = ".")
    rownamesFile <- tryCatch(
        expr = localOrRemoteFile(rownamesFile),
        error = function(e) {
            NULL  # nocov
        }
    )
    ## Add the colnames automatically using `.colnames` sidecar file.
    colnamesFile <- paste(file, "colnames", sep = ".")
    colnamesFile <- tryCatch(
        expr = localOrRemoteFile(colnamesFile),
        error = function(e) {
            NULL  # nocov
        }
    )
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "Matrix::readMM"
    ))
    object <- readMM(file = file)
    if (!is.null(rownamesFile)) {
        rownames(object) <- .importMTXSidecar(rownamesFile)
    }
    if (!is.null(colnamesFile)) {
        colnames(object) <- .importMTXSidecar(colnamesFile)
    }
    object <- .slotMetadata(object, pkg = "Matrix", fun = "readMM")
    object
}



## Internal importer for a sparse matrix sidecar file (e.g. `.rownames`).
.importMTXSidecar <- function(file) {
    message(sprintf("Importing sidecar '%s'.", basename(file)))
    .importLines(file)
}



## List ========================================================================
## Internal importer for a JSON file (`.json`).
.importJSON <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "jsonlite::read_json"
    ))
    assert(requireNamespace("jsonlite", quietly = TRUE))
    object <- jsonlite::read_json(path = file)
    object <- .slotMetadata(object, pkg = "jsonlite", fun = "read_json")
    object
}



## Internal importer for a YAML file (`.yaml`, `.yml`).
.importYAML <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "yaml::yaml.load_file"
    ))
    assert(requireNamespace("yaml", quietly = TRUE))
    object <- yaml::yaml.load_file(input = file)
    object <- .slotMetadata(object, pkg = "yaml", fun = "yaml.load_file")
    object
}



## GSEA ========================================================================
## Internal importer for a gene matrix transposed file (`.gmt`).
## See also `fgsea::gmtPathways()`.
.importGMT <- function(file) {
    message(sprintf("Importing '%s'.", basename(file)))
    lines <- .importLines(file)
    lines <- strsplit(lines, split = "\t")
    pathways <- lapply(lines, tail, n = -2L)
    names(pathways) <- vapply(
        X = lines,
        FUN = head,
        FUN.VALUE = character(1L),
        n = 1L
    )
    pathways
}



## Internal importer for a gene matrix file (`.gmx`).
.importGMX <- function(file) {
    message(sprintf("Importing '%s'.", basename(file)))
    lines <- .importLines(file)
    pathways <- list(tail(lines, n = -2L))
    names(pathways) <- lines[[1L]]
    pathways
}



## Internal importer for a gene set file (`.grp`).
.importGRP <- .importGMX



## Microsoft Excel =============================================================
## Note that `readxl::read_excel()` doesn't currently support automatic blank
## lines removal, so ensure that is fixed downstream.

## Internal importer for a Microsoft Excel worksheet (`.xlsx`).
.importXLSX <- function(file, sheet = 1L, colnames = TRUE) {
    assert(isFlag(colnames) || isCharacter(colnames))
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "readxl::read_excel"
    ))
    assert(requireNamespace("readxl", quietly = TRUE))
    object <- readxl::read_excel(
        path = file,
        sheet = sheet,
        col_names = colnames,
        na = naStrings,
        trim_ws = TRUE,
        progress = FALSE,
        .name_repair = "minimal"
    )
    ## Always return as data.frame instead of tibble at this step.
    object <- as.data.frame(
        x = object,
        make.names = FALSE,
        stringsAsFactors = FALSE
    )
    object <- .slotMetadata(object, pkg = "readxl", fun = "read_excel")
    object
}



## readxl does support XLS format also but it's buggy for many files.
## Make some minimal repex examples and file issue on GitHub.
##
## See also:
## - https://github.com/tidyverse/readxl/issues/466
## - https://github.com/tidyverse/readxl/issues/472
##
## In the meantime, load using gdata, which is slower but does work.

## Internal importer for a legacy Microsoft Excel worksheet (`.xls`).
.importXLS <- function(file, sheet = 1L, colnames = TRUE) {
    assert(isFlag(colnames) || isCharacter(colnames))
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "gdata::read.xls"
    ))
    assert(requireNamespace("gdata", quietly = TRUE))
    if (isCharacter(colnames)) {
        header <- FALSE
    } else {
        header <- colnames
    }
    ## gdata currently has an OS.type partial match issue.
    ## `read.xls()` passes `...` to `utils::read.table()`.
    object <- withCallingHandlers(
        expr = gdata::read.xls(
            xls = file,
            sheet = sheet,
            verbose = FALSE,
            na.strings = naStrings,
            header = header
        ),
        warning = function(w) {
            ## nocov start
            if (isTRUE(grepl(
                pattern = "partial match of 'OS' to 'OS.type'",
                x = as.character(w)
            ))) {
                invokeRestart("muffleWarning")
            } else {
                w
            }
            ## nocov end
        }
    )
    if (isCharacter(colnames)) {
        assert(hasLength(colnames, n = ncol(object)))
        colnames(object) <- colnames
    }
    object <- .slotMetadata(object, pkg = "gdata", fun = "read.xls")
    object
}



## GraphPad Prism ==============================================================
## Internal importer for a GraphPad Prism file (`.pzfx`).
## Note that this function doesn't support optional column names.
.importPZFX <- function(file, sheet = 1L) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "pzfx::read_pzfx"
    ))
    assert(requireNamespace("pzfx", quietly = TRUE))
    object <- pzfx::read_pzfx(
        path = file,
        table = sheet
    )
    object <- .slotMetadata(object, pkg = "pzfx", fun = "read_pzfx")
    object
}



## bcbio =======================================================================
## Internal importer for a bcbio count matrix file (`.counts`).
## These files contain an `"id"` column that we need to coerce to row names.
.importBCBCounts <- function(file) {
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "data.table::fread"
    ))
    file <- localOrRemoteFile(file)
    object <- fread(
        file = file,
        na.strings = naStrings
    )
    assert(
        isSubset("id", colnames(object)),
        hasNoDuplicates(object[["id"]])
    )
    object <- as.data.frame(object)
    rownames(object) <- object[["id"]]
    object[["id"]] <- NULL
    object <- as.matrix(object)
    mode(object) <- "integer"
    object <- .slotMetadata(object, pkg = "data.table", fun = "fread")
    object
}



## Handoff =====================================================================
.rioImport <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf("Importing '%s' using '%s()'.",
        basename(file), "rio::import"
    ))
    assert(requireNamespace("rio", quietly = TRUE))
    object <- rio::import(file)
    object <- .slotMetadata(object, pkg = "rio", fun = "import")
    object
}



## Using `tryCatch()` here to error if there are any warnings.
.rtracklayerImport <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "rtracklayer::import"
    ))
    assert(requireNamespace("rtracklayer", quietly = TRUE))
    object <- tryCatch(
        expr = rtracklayer::import(file),
        error = function(e) {
            stop("File failed to load.")  # nocov
        },
        warning = function(w) {
            stop("File failed to load.")  # nocov
        }
    )
    object <- .slotMetadata(object, pkg = "rtracklayer", fun = "import")
    object
}
