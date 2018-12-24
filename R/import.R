# GMT/GMX:
# https://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats



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
#' @section bcbio files:
#'
#' Also supports some additional extensions commonly used with the
#' [bcbio](https://bcbio-nextgen.readthedocs.io) pipeline:
#'
#' - `COUNTS`: Counts table (e.g. RNA-seq aligned counts).
#' - `COLNAMES`: Sidecar file containing column names.
#' - `ROWNAMES`: Sidecar file containing row names.
#'
#' @export
#' @inheritParams params
#'
#' @param dataFrame `character(1)`.
#'   Data frame class to return. Can be set globally using the `brio.data.frame`
#'   option.
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
import <- function(
    file,
    ...,
    dataFrame = c("data.frame", "DataFrame", "tbl_df", "data.table")
) {
    assert(isString(file))
    file <- localOrRemoteFile(file)
    dataFrame <- match.arg(dataFrame)

    ext <- str_match(basename(file), extPattern)[1L, 2L]
    # Simplify the extension matching by converting to uppercase.
    ext <- toupper(ext)

    # Error on blacklisted extension.
    if (ext %in% c("DOC", "DOCX", "PDF", "PPT", "PPTX")) {
        stop(paste0(
            "Import of ", basename(file), " failed.\n",
            ext, " extension is not supported."
        ))
    }

    # How we set NA strings depends on the file extension.
    if (ext %in% c("CSV", "TSV", "TXT")) {
        data <- .importDelim(file, ...)
    } else if (ext %in% c("XLS", "XLSX")) {
        data <- .importExcel(file, ...)
    } else if (ext == "RDS") {
        data <- .importRDS(file, ...)
    } else if (ext %in% c("RDA", "RDATA")) {
        data <- .importRDA(file, ...)
    } else if (ext %in% c("GFF", "GFF3", "GTF")) {
        data <- .importGFF(file, ...)
    } else if (ext == "JSON") {
        data <- .importJSON(file, ...)
    } else if (ext %in% c("YAML", "YML")) {
        data <- .importYAML(file, ...)
    } else if (ext %in% c("LOG", "MD", "PY", "R", "RMD", "SH")) {
        data <- .importLines(file, ...)
    } else if (ext == "MTX") {
        data <- .importMTX(file, ...)
    } else if (ext %in% c("COLNAMES", "ROWNAMES")) {
        data <- .importSidecar(file, ...)
    } else if (ext == "COUNTS") {
        data <- .importCounts(file, ...)
    } else {
        data <- .importRio(file, ...)
    }

    if (is.data.frame(data)) {
        # Coerce data frame return, if necessary.
        if (dataFrame == "data.frame") {
            data <- as.data.frame(data)
        } else if (dataFrame == "DataFrame") {
            data <- as(data, "DataFrame")
        } else if (dataFrame == "tbl_df") {
            data <- as_tibble(data)
        } else if (dataFrame == "data.table") {
            data <- as.data.table(data)
        }

        # Set rownames automatically, if supported.
        if (
            dataFrame %in% c("data.frame", "DataFrame") &&
            "rowname" %in% colnames(data)
        ) {
            message("Setting rownames from `rowname` column.")
            rownames(data) <- data[["rowname"]]
            data[["rowname"]] <- NULL
        }
    }

    data
}



.importRio <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using rio::import()."
    ))
    requireNamespace("rio", quietly = TRUE)
    rio::import(file, ...)
}



# CSV, TSV, TXT
.importDelim <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using data.table::fread()."
    ))
    data <- fread(file = file, na.strings = naStrings, ...)
    # Coerce tibble to data.frame.
    data <- as.data.frame(data)
}



# Microsoft Excel
.importExcel <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using readxl::read_excel()."
    ))
    data <- read_excel(path = file, na = naStrings, ...)
    # Coerce data.table to data.frame.
    data <- as.data.frame(data)
    data
}



.importRDS <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using base::readRDS()."
    ))
    readRDS(file, ...)
}



.importRDA <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using base::load()."
    ))
    safe <- new.env()
    object <- load(file, envir = safe, ...)
    if (length(safe) != 1L) {
        stop("File does not contain a single object.")
    }
    get(object, envir = safe, inherits = FALSE)
}



# GFF, GTF
.importGFF <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using rtracklayer::import()."
    ))
    requireNamespace("rtracklayer", quietly = TRUE)
    tryCatch(
        expr = rtracklayer::import(con = file, ...),
        error = function(e) {
            stop("GFF file failed to load.")  # nocov
        },
        warning = function(w) {
            stop("GFF file failed to load.")  # nocov
        }
    )
}



.importGMT <- function(file, ...) {
    # FIXME
    stop("Not added yet")
}



.importGMX <- function(file, ...) {
    # FIXME
    stop("Not added yet")
}



.importJSON <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using jsonlite::read_json()."
    ))
    read_json(path = file, ...)
}



.importYAML <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using yaml::yaml.load_file()."
    ))
    yaml.load_file(input = file, ...)
}



# Source code lines
.importLines <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using readr::read_lines()."
    ))
    read_lines(file, ...)
}



# Sparse matrix. Note that we're warning the user if row and column
# name sidecar files don't exist.
.importMTX <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using Matrix::readMM()."
    ))
    data <- readMM(file = file, ...)

    # Add the rownames automatically using `.rownames` sidecar file.
    rownamesFile <- paste(file, "rownames", sep = ".")
    rownamesFile <- tryCatch(
        expr = localOrRemoteFile(rownamesFile),
        error = function(e) {
            warning(paste0(
                basename(rownamesFile), " does not exist.\n",
                "  Row names will not be added to sparse matrix."
            ), call. = FALSE)
            NULL
        }
    )
    if (!is.null(rownamesFile)) {
        rownames(data) <- .importSidecar(rownamesFile)
    }

    # Add the colnames automatically using `.colnames` sidecar file.
    colnamesFile <- paste(file, "colnames", sep = ".")
    colnamesFile <- tryCatch(
        expr = localOrRemoteFile(colnamesFile),
        error = function(e) {
            warning(paste0(
                basename(colnamesFile), " does not exist.\n",
                "  Column names will not be added to sparse matrix."
            ), call. = FALSE)
            NULL
        }
    )
    if (!is.null(colnamesFile)) {
        colnames(data) <- .importSidecar(colnamesFile)
    }

    data
}



# Sparse matrix sidecar files (.rownames, .colnames)
.importSidecar <- function(file, ...) {
    message(paste(
        "Importing", basename(file),
        "using readr::read_lines()."
    ))
    read_lines(file = file, na = naStrings, ...)
}



# bcbio count matrix.
.importCounts <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using readr::read_tsv()."
    ))
    data <- read_tsv(file = file, na = naStrings, ...)
    assert(
        isSubset("id", colnames(data)),
        hasNoDuplicates(data[["id"]])
    )
    # Coerce tibble to data frame.
    data <- as.data.frame(data)
    # Need to move the "id" column to rownames.
    data <- column_to_rownames(data, var = "id")
    # Coerce data.frame to matrix.
    data <- as.matrix(data)
    data
}
