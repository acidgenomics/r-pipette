#' Import
#'
#' Read file by extension into R.
#'
#' Remote URLs and compressed files are supported. All extensions are case
#' insensitive.
#'
#' This function supports automatic loading of common file types:
#'
#' - `CSV`: Comma Separated Values.\cr
#'   Imported by [data.table::fread()].
#' - `TSV` Tab Separated Values.\cr
#'   Imported by [data.table::fread()].
#' - `TXT`: Text file. *Ambiguous, not recommended*.\cr
#'   Imported by [data.table::fread()].
#' - `XLSX`/`XLS`: Excel workbook.\cr
#'   Imported by [readxl::read_excel()].
#' - `MTX`: MatrixMarket sparse matrix.\cr
#'   Imported by [Matrix::readMM()].
#' - `GTF`/`GFF`/`GFF3`: General Feature Format.\cr
#'   Imported by [rtracklayer::import()].
#' - `JSON`: JSON.
#'   Imported by [jsonlite::read_json()].
#' - `YAML`/`YML`: YAML.
#'   Imported by [yaml::yaml.load_file()].
#' - `RDA`/`RDATA`: R Data.
#'     - Imported by `load`.
#'     - Must contain a single object.
#'     - Doesn't require internal object name to match, unlike [loadData()].
#' - `RDS`: R Data Serialized.\cr
#'   Imported by [`readRDS()`][base::readRDS].
#'
#' These file formats will be imported as source code lines by
#' [readr::read_lines()]: `LOG`, `MD`, `PY`, `R`, `RMD`, `SH`.
#'
#' These file formats are blacklisted, and intentionally not supported:
#' `DOC`, `DOCX`, `PDF`, `PPT`, `PPTX`.
#'
#' If a file format isn't supported natively (or blacklisted), the
#' [rio](https://cran.r-project.org/web/packages/rio/index.html) package will
#' be used as a fallback attempt. See [rio::import()] for details.
#'
#' @section Delimited Files (CSV/TSV):
#'
#' [import()] uses the [`fread()`][data.table::fread] function of the
#' [data.table][] package to import standard CSV and TSV files. This should work
#' automatically for most files without issue.
#'
#' Here are some notable exceptions:
#'
#' - Columns headers should be `character`. In the event that a column name
#'   is `numeric`, set `header = TRUE` here to force the column name.
#'
#' See `help(topic = "fread", package = "data.table")` for details.
#'
#' The [`read_csv()`][readr::read_csv] and [`read_tsv()`][readr::read_tsv]
#' functions of the [readr][] package are good alternatives, which return
#' `tibble` data frames (`tbl_df`).
#'
#' [data.table]: https://cran.r-project.org/package=data.table
#' [readr]: https://readr.tidyverse.org
#'
#' @section Google Sheets:
#'
#' Public Google Sheets are now supported. Simply paste in the URL.
#'
#' @section Matrix Market Exchange (MTX/MEX):
#'
#' Reading a Matrix Market Exchange (`MTX`) file now requires `COLNAMES` and
#' `ROWNAMES` sidecar files containing the `colnames` and `rownames` of
#' the sparse matrix. Legacy support for manual loading of these sidecar files
#' is provided.
#'
#' @section General Feature Format (GFF/GTF):
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' Column names follow the [Ensembl conventions](https://bit.ly/2K6EBla).
#
#' Additional information:
#'
#' - [Ensembl](http://www.ensembl.org/info/website/upload/gff.html)
#' - [Gencode](http://www.gencodegenes.org/gencodeformat.html)
#'
#' @section GMT/GMX files:
#'
#' Refer to the Broad Institute [GSEA wiki][] for details.
#'
#' [GSEA wiki]: https://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats
#'
#' @section bcbio files:
#'
#' Also supports some additional extensions commonly used with the
#' [bcbio](https://bcbio-nextgen.readthedocs.io) pipeline:
#'
#' - `COUNTS`: Counts table (e.g. RNA-seq aligned counts).
#' - `COLNAMES`: Sidecar file containing column names.
#' - `ROWNAMES`: Sidecar file containing row names.
#'
#' @section Data frame return:
#'
#' By default, [import()] returns a standard `data.frame`. However, any of these
#' desired output formats can be set globally using `brio.data.frame`:
#'
#' - `data.frame`
#' - `DataFrame` (S4Vectors).
#' - `data.table` (data.table).
#' - `tbl_df` (tibble).
#'
#' @export
#' @inheritParams params
#
#' @param ... Additional arguments, passed to the respective internal loading
#'   function.
#'
#' Current recommendations (by priority):
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
#' @return Varies, depending on the file extension:
#'
#' - Default: `DataFrame`.
#' - `MTX`: `sparseMatrix`.
#' - `GTF`/`GFF`: `GRanges`.
#' - `JSON`/`YAML`: `list`.
#' - Source code or log: `character`.
#'
#' @seealso
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [Matrix](https://cran.r-project.org/web/packages/Matrix/index.html).
#'
#' @examples
#' file <- system.file("extdata/example.csv", package = "brio")
#' x <- import(file)
#' print(x)
import <- function(file, ...) {
    # Note that we're not using a `localOrRemoteFile()` call for `file` here,
    # so we can support Google Sheets.
    assert(isString(file))

    # Allow Google Sheets import using rio.
    if (grepl("docs\\.google\\.com/spreadsheets/", file)) {
        ext <- "GSHEET"
    } else {
        ext <- toupper(str_match(basename(file), extPattern)[1L, 2L])
    }

    # How we set NA strings depends on the file extension.
    if (ext %in% c("CSV", "FWF", "PSV", "TSV", "TXT")) {
        object <- importDelim(file, ...)
    } else if (ext %in% c("XLS", "XLSB", "XLSX")) {
        object <- importXLSX(file, ...)
    } else if (ext == "RDS") {
        object <- importRDS(file, ...)
    } else if (ext %in% c("RDA", "RDATA")) {
        object <- importRDA(file, ...)
    } else if (ext == "GMT") {
        object <- importGMT(file, ...)
    } else if (ext == "GMX") {
        object <- importGMX(file, ...)
    } else if (ext == "JSON") {
        object <- importJSON(file, ...)
    } else if (ext %in% c("YAML", "YML")) {
        object <- importYAML(file, ...)
    } else if (ext == "MTX") {
        object <- importMTX(file, ...)
    } else if (ext == "COUNTS") {
        object <- importCounts(file, ...)
    } else if (ext %in% c("LOG", "MD", "PY", "R", "RMD", "SH")) {
        object <- importLines(file, ...)
    } else if (ext %in% c(
        "BED", "BED15", "BEDGRAPH", "BEDPE",
        "BROADPEAK", "NARROWPEAK",
        "GFF", "GFF1", "GFF2", "GFF3", "GTF",
        "BIGWIG", "BW", "WIG"
    )) {
        object <- .rtracklayerImport(file, ...)
    } else if (ext %in% c(
        "ARFF",      # Weka Attribute-Relation File Format
        "DBF",       # dBase Database File
        "DIF",       # Data Interchange Format
        "DTA",       # Stata
        "GSHEET",    # Google Sheets, matched by URL (see above)
        "MAT",       # Matlab
        "MTP",       # Minitab
        "ODS",       # OpenDocument (LibreOffice)
        "POR",       # SPSS
        "SAS7BDAT",  # SASS
        "SAV",       # SPSS
        "SYD",       # Systat
        "REC",       # Epi Info
        "XPT"        # SASS
    )) {
        object <- .rioImport(file, ...)
    } else {
        stop(paste0(
            "Import of ", basename(file), " failed.\n",
            ext, " extension is not supported."
        ))
    }

    if (is.data.frame(object)) {
        # Coerce data frame to desired global output, if necessary.
        pref <- getOption("basejump.data.frame")
        if (isString(pref)) {
            object <- switch(
                data.frame = as.data.frame(object),
                DataFrame = as(object, "DataFrame"),
                tbl_df = as_tibble(object),
                data.table = as.data.table(object)
            )
        }

        # Set rownames automatically, if supported.
        if (
            isAny(object, c("data.frame", "DataFrame")) &&
            "rowname" %in% colnames(object)
        ) {
            message("Setting rownames from `rowname` column.")
            rownames(object) <- object[["rowname"]]
            object[["rowname"]] <- NULL
        }
    }

    # Slot data provenance metadata into object.
    newMeta <- list(
        brio = packageVersion("brio"),
        file = if (isAFile(file)) { realpath(file) } else { file },
        date = Sys.Date(),
        call = match.call()
    )
    if (isS4(object) && "metadata" %in% slotNames(object)) {
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

    validObject(object)
    object
}



.rioImport <- function(file, ...) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using rio::import()."))
    requireNamespace("rio", quietly = TRUE)
    object <- rio::import(file, ...)
    object <- .slotMetadata(object, pkg = "rio", fun = "import")
    object
}



# Using `tryCatch()` here to error if there are any warnings.
.rtracklayerImport <- function(file, ...) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using rtracklayer::import()."))
    requireNamespace("rtracklayer", quietly = TRUE)
    object <- tryCatch(
        expr = rtracklayer::import(file, ...),
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



# Slot data provenance metadata.
.slotMetadata <- function(object, pkg, fun) {
    assert(isString(pkg), isString(fun))
    importer <- paste0(pkg, "::", fun)
    version <- packageVersion(pkg)
    if (isS4(object) && "metadata" %in% slotNames(object)) {
        metadata(object)[["brio"]][["importer"]] <- importer
        metadata(object)[["brio"]][[pkg]] <- version
    } else {
        # Use `attr()` instead of `attributes()` here. It doesn't error on
        # assignment when the object doesn't already have attributes.
        attr(object, "brio")[["importer"]] <- importer
        attr(object, "brio")[[pkg]] <- version
    }
    object
}
