#' Import
#'
#' Read file by extension into R.
#'
#' [import()] supports automatic loading of common file types, by wrapping
#' popular importer functions. The function is intentionally designed to be
#' simple. Remote URLs and compressed files are supported. If you need more
#' complex import settings, just call the wrapped importer directly instead.
#'
#' @note Updated 2019-07-30.
#' @export
#'
#' @inheritParams acidroxygen::params
#' @param sheet
#'   *Applies to Excel Workbook, Google Sheet, or GraphPad Prism file.*\cr
#'   `character(1)` or `integer(1)`.
#'   Sheet to read. Either a string (the name of a sheet), or an integer (the
#'   position of the sheet). Defaults to the first sheet.
#' @param rownames `logical(1)`.
#'   Automatically assign row names, if `rowname` column is defined.
#'   Applies to file types that return `data.frame` only.
#' @param colnames `logical(1)`.
#'   Automatically assign column names, using the first header row.
#'   Applies to file types that return `data.frame` only.
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
#' - **Gene sets (for GSEA)** (`GMT`, `GMX`): `character`.\cr
#'   Imported by [readr::read_lines()], with additional processing.
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
#'   Imported by [readr::read_lines()].
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
#' column names. If you are working with a file that doens't contain column
#' names, either set `colnames = FALSE` or call the wrapped importer function
#' directly instead. It's strongly recommended to always define column names in
#' a supported file type.
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
#' @seealso
#'
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
    sheet = 1L,
    rownames = TRUE,
    colnames = TRUE
) {
    ## We're supporting remote files, so don't check using `isAFile()` here.
    assert(
        isString(file),
        isScalar(sheet),
        isFlag(rownames),
        isFlag(colnames)
    )

    ## Allow Google Sheets import using rio, by matching the URL.
    ## Otherwise, coerce the file extension to uppercase, for easy matching.
    ext <- toupper(str_match(basename(file), extPattern)[1L, 2L])

    if (ext %in% c("CSV", "FWF", "PSV", "TSV", "TXT")) {
        object <- importDelim(file, colnames = colnames)
    } else if (ext == "XLS") {
        object <- importXLS(file, sheet = sheet, colnames = colnames)
    } else if (ext %in% c("XLSB", "XLSX")) {
        object <- importXLSX(file, sheet = sheet, colnames = colnames)
    } else if (ext == "PZFX") {
        ## GraphPad Prism project.
        ## Note that Prism files always contain column names.
        object <- importPZFX(file, sheet = sheet)
    } else if (ext == "RDS") {
        object <- importRDS(file)
    } else if (ext %in% c("RDA", "RDATA")) {
        object <- importRDA(file)
    } else if (ext == "GMT") {
        object <- importGMT(file)
    } else if (ext == "GMX") {
        object <- importGMX(file)
    } else if (ext == "GRP") {
        object <- importGRP(file)
    } else if (ext == "JSON") {
        object <- importJSON(file)
    } else if (ext %in% c("YAML", "YML")) {
        object <- importYAML(file)
    } else if (ext == "MTX") {
        ## We're always requiring row and column sidecar files for MTX.
        object <- importMTX(file)
    } else if (ext == "COUNTS") {
        ## bcbio counts format always contains row and column names.
        object <- importCounts(file)
    } else if (ext %in% c("LOG", "MD", "PY", "R", "RMD", "SH")) {
        object <- importLines(file)
    } else if (ext %in% c(
        "BED", "BED15", "BEDGRAPH", "BEDPE",
        "BROADPEAK", "NARROWPEAK",
        "GFF", "GFF1", "GFF2", "GFF3", "GTF",
        "BIGWIG", "BW", "WIG"
    )) {
        object <- .rtracklayerImport(file)
    } else if (ext %in% c(
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
    )) {
        object <- .rioImport(file)
    } else {
        stop(sprintf(
            "Import of '%s' failed. '%s' extension is not supported.",
            basename(file), ext
        ))
    }

    if (is.data.frame(object)) {
        ## Coerce data frame to desired global output, if necessary.
        pref <- getOption("acid.data.frame")
        if (isString(pref)) {
            object <- switch(
                EXPR = pref,
                data.frame = object,
                DataFrame = as(object, "DataFrame"),
                tbl_df = as_tibble(
                    x = object,
                    .name_repair = "minimal",
                    rownames = NULL
                ),
                data.table = as.data.table(
                    x = object,
                    keep.rownames = FALSE
                )
            )
        }

        ## Set rownames automatically, if supported.
        if (
            isAny(object, c("data.frame", "DataFrame")) &&
            !isAny(object, c("data.table", "tbl_df")) &&
            "rowname" %in% colnames(object) &&
            isTRUE(rownames)
        ) {
            message("Setting row names from 'rowname' column.")
            rownames(object) <- object[["rowname"]]
            object[["rowname"]] <- NULL
        }
    }

    ## Slot data provenance metadata into object.
    newMeta <- list(
        brio = packageVersion("brio"),
        file = if (isAFile(file)) {
            realpath(file)
        } else {
            file
        },
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

    ## Check for syntactically valid names and inform the user, if necessary.
    if (
        (hasNames(object) && !hasValidNames(object)) ||
        (hasDimnames(object) && !hasValidDimnames(object))
    ) {
        ## nocov start
        message(paste(
            basename(file),
            "does not return syntactically valid names."
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
            warning(paste(
                length(dupes), "duplicate names:",
                toString(dupes, width = 200L)
            ))
            ## nocov end
        }
    }

    ## Don't run object validity check with `validObject()` here.
    object
}



.rioImport <- function(file) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using rio::import()."))
    requireNamespace("rio", quietly = TRUE)
    object <- rio::import(file)
    object <- .slotMetadata(object, pkg = "rio", fun = "import")
    object
}



## Using `tryCatch()` here to error if there are any warnings.
.rtracklayerImport <- function(file) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using rtracklayer::import()."))
    requireNamespace("rtracklayer", quietly = TRUE)
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
