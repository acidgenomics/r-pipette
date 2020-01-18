#' Import
#'
#' Read file by extension into R.
#'
#' [import()] supports automatic loading of common file types, by wrapping
#' popular importer functions. It intentionally designed to be simple, with few
#' arguments. Remote URLs and compressed files are supported. If you need more
#' complex import settings, just call the wrapped importer directly instead.
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
#' [basejump]: https://basejump.acidgenomics.com/
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
#' @export
#' @note Updated 2020-01-18.
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
#' file <- system.file("extdata/example.csv", package = "pipette")
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
    format = "auto",
    sheet = 1L,
    metadata,
    quiet
) {
    ## We're supporting remote files, so don't check using `isAFile()` here.
    assert(
        isAFile(file) || isAURL(file),
        isFlag(rownames),
        isFlag(colnames) || isCharacter(colnames),
        isScalar(sheet),
        isFlag(metadata),
        isFlag(quiet)
    )
    ## 2019-10-18: Default renamed from "none" to "auto".
    format <- match.arg(
        arg = format,
        choices = c("auto", "csv", "tsv", "txt", "lines", "none")
    )
    ## Allow Google Sheets import using rio, by matching the URL.
    ## Otherwise, coerce the file extension to uppercase, for easy matching.
    if (identical(format, "auto") || identical(format, "none")) {
        ext <- str_match(basename(file), extPattern)[1L, 2L]
        if (is.na(ext)) {
            if (!isTRUE(quiet)) {
                cli_alert_warning(paste(
                    "No file extension detected.",
                    "Importing as {.strong lines}."
                ))
            }
            ext <- "lines"
        }
    } else {
        ext <- format
    }
    ext <- toupper(ext)
    if (isSubset(ext, c("CSV", "FWF", "PSV", "TSV", "TXT"))) {
        object <- .importDelim(
            file = file,
            colnames = colnames,
            ext = ext,
            metadata = metadata,
            quiet = quiet
        )
    } else if (identical(ext, "XLS")) {
        object <- .importXLS(
            file = file,
            sheet = sheet,
            colnames = colnames,
            metadata = metadata,
            quiet = quiet
        )
    } else if (isSubset(ext, c("XLSB", "XLSX"))) {
        object <- .importXLSX(
            file = file,
            sheet = sheet,
            colnames = colnames,
            metadata = metadata,
            quiet = quiet
        )
    } else if (identical(ext, "PZFX")) {
        ## GraphPad Prism project.
        ## Note that Prism files always contain column names.
        object <- .importPZFX(
            file = file,
            sheet = sheet,
            metadata = metadata,
            quiet = quiet
        )
    } else if (identical(ext, "RDS")) {
        object <- .importRDS(file = file, quiet = quiet)
    } else if (isSubset(ext, c("RDA", "RDATA"))) {
        object <- .importRDA(file = file, quiet = quiet)
    } else if (identical(ext, "GMT")) {
        object <- .importGMT(file = file, quiet = quiet)
    } else if (identical(ext, "GMX")) {
        object <- .importGMX(file = file, quiet = quiet)
    } else if (identical(ext, "GRP")) {
        object <- .importGRP(file = file, quiet = quiet)
    } else if (identical(ext, "JSON")) {
        object <- .importJSON(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else if (isSubset(ext, c("YAML", "YML"))) {
        object <- .importYAML(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else if (identical(ext, "MTX")) {
        ## We're always requiring row and column sidecar files for MTX.
        object <- .importMTX(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else if (identical(ext, "COUNTS")) {
        ## bcbio counts format always contains row and column names.
        object <- .importBcbioCounts(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else if (isSubset(ext, c("LINES", "LOG", "MD", "PY", "R", "RMD", "SH"))) {
        object <- .importLines(file = file, quiet = quiet)
    } else if (isSubset(ext, c(
        "BED", "BED15", "BEDGRAPH", "BEDPE",
        "BROADPEAK", "NARROWPEAK",
        "GFF", "GFF1", "GFF2", "GFF3", "GTF",
        "BIGWIG", "BW", "WIG"
    ))) {
        object <- .rtracklayerImport(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
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
        object <- .rioImport(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else {
        stop(sprintf(
            "Import of '%s' failed. '%s' extension is not supported.",
            basename(file), ext
        ))
    }
    ## Data frame-specific operations.
    if (is.data.frame(object)) {
        ## Set row names automatically.
        if (isSubset("rowname", colnames(object))) {
            if (!isTRUE(quiet)) {
                cli_alert_info("Setting row names from {.var rowname} column.")
            }
            rownames(object) <- object[["rowname"]]
            object[["rowname"]] <- NULL
        }
    }
    ## Check for syntactically valid names and inform the user, if necessary.
    if (
        (
            (hasNames(object) && !hasValidNames(object)) ||
            (hasDimnames(object) && !hasValidDimnames(object))
        ) &&
        !isTRUE(quiet)
    ) {
        ## nocov start
        cli_alert_warning(sprintf(
            "{.file %s} does not contain syntactically valid names.",
            basename(file)
        ))
        ## nocov end
    }
    ## Inform the user when encountering duplicate names. This `tryCatch()` step
    ## here helps suppress `validObject()` error for invalid SE objects.
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
    if (isTRUE(metadata)) {
        if (!is.null(metadata2(object, which = "import"))) {
            ## Add the call to metadata.
            m <- metadata2(object, which = "import")
            call <- tryCatch(
                expr = standardizeCall(),
                error = function(e) NULL
            )
            m[["call"]] <- call
            metadata2(object, which = "import") <- m
        }
    }
    object
}

formals(import)[c("metadata", "quiet")] <-
    .formalsList[c("import.metadata", "quiet")]



## Add data provenance metadata.
## Previously, "which" was defined as "brio", until v0.3.8.
## Updated 2019-10-24.
.slotImportMetadata <- function(object, file, pkg, fun) {
    assert(
        isString(file),
        isString(pkg),
        isString(fun)
    )
    metadata2(object, which = "import") <- list(
        package = "brio",
        packageVersion = packageVersion("brio"),
        importer = paste0(pkg, "::", fun),
        importerVersion = packageVersion(pkg),
        file = if (isAFile(file)) {
            realpath(file)
        } else {
            file
        },
        date = Sys.Date()
    )
    object
}



## Basic =======================================================================
## Internal importer for a delimited file (e.g. `.csv`, `.tsv`).
## Calls `data.table::fread()` internally by default.
## Can override using `acid.import.engine` option, which also supports readr.
## Updated 2020-01-18.
.importDelim <- function(
    file,
    colnames,
    ext,
    metadata,
    quiet
) {
    assert(
        isFlag(colnames) || isCharacter(colnames),
        isFlag(verbose),
        isFlag(metadata),
        isFlag(quiet)
    )
    whatPkg <- match.arg(
        arg = getOption("acid.import.engine", default = "data.table"),
        choices = c("data.table", "readr", "vroom")
    )
    verbose <- getOption("acid.verbose", default = FALSE)
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (identical(whatPkg, "data.table")) {
        ## data.table ----------------------------------------------------------
        assert(requireNamespace(whatPkg, quietly = TRUE))
        whatFun <- "fread"
        what <- get(
            x = whatFun,
            envir = asNamespace(whatPkg),
            inherits = TRUE
        )
        args <- list(
            file = tmpfile,
            blank.lines.skip = TRUE,
            check.names = FALSE,
            data.table = FALSE,
            fill = FALSE,
            na.strings = naStrings,
            skip = 0L,
            showProgress = FALSE,
            stringsAsFactors = FALSE,
            strip.white = TRUE,
            verbose = verbose
        )
        if (isCharacter(colnames)) {
            args[["header"]] <- FALSE
            args[["col.names"]] <- colnames
        } else {
            args[["header"]] <- colnames
        }
    } else if (identical(whatPkg, "readr")) {
        ## readr ---------------------------------------------------------------
        assert(requireNamespace(whatPkg, quietly = TRUE))
        whatFun <- switch(
            EXPR = ext,
            "CSV" = "read_csv",
            "TSV" = "read_tsv",
            "TXT" = "read_delim"
        )
        what <- get(
            x = whatFun,
            envir = asNamespace(whatPkg),
            inherits = TRUE
        )
        assert(is.function(what))
        args <- list(
            file = tmpfile,
            col_names = colnames,
            col_types = readr::cols(),
            na = naStrings,
            progress = FALSE,
            trim_ws = TRUE,
            skip = 0L,
            skip_empty_rows = TRUE
        )
    } else if (identical(whatPkg, "vroom")) {
        ## vroom ---------------------------------------------------------------
        assert(requireNamespace(whatPkg, quietly = TRUE))
        whatFun <- "vroom"
        what <- get(
            x = whatFun,
            envir = asNamespace(whatPkg),
            inherits = TRUE
        )
        args <- list(
            file = tmpfile,
            col_names = colnames,
            col_types = vroom::cols(),
            na = naStrings,
            progress = FALSE,
            skip = 0L,
            trim_ws = TRUE,
            .name_repair = make.names  # or "universal"
        )
    }
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), whatPkg, whatFun
        ))
    }
    object <- do.call(what = what, args = args)
    assert(is.data.frame(object))
    if (!identical(class(object), "data.frame")) {
        object <- as.data.frame(object, stringsAsFactors = FALSE)
    }
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = whatPkg,
            fun = whatFun
        )
    }
    object
}



## Internal importer for (source code) lines.
## Updated 2020-01-17.
.importLines <- function(file, quiet) {
    assert(isFlag(quiet))
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "base", "readLines"
        ))
    }
    con <- file(tmpfile)
    object <- readLines(con = con)
    close(con)
    object
}



## R data ======================================================================
## Internal importer for an R data serialized file (`.rds`).
## Updated 2020-01-17.
.importRDS <- function(file, quiet) {
    assert(isFlag(quiet))
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "base", "readRDS"
        ))
    }
    object <- readRDS(file = tmpfile)
    object
}



## Internal importer for an R data file (`.rda`).
## Updated 2020-01-17.
.importRDA <- function(file, quiet) {
    assert(isFlag(quiet))
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "base", "load"
        ))
    }
    safe <- new.env()
    object <- load(file = tmpfile, envir = safe)
    if (length(safe) != 1L) {
        stop(sprintf("'%s' does not contain a single object.", basename(file)))
    }
    object <- get(object, envir = safe, inherits = FALSE)
    object
}



## Sparse matrix ===============================================================
## Internal importer for a sparse matrix file (`.mtx`).
## Updated 2020-01-17.
.importMTX <- function(file, metadata, quiet) {
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "Matrix", "readMM"
        ))
    }
    object <- readMM(file = tmpfile)
    ## Add the rownames automatically using `.rownames` sidecar file.
    rownamesFile <- paste(file, "rownames", sep = ".")
    rownamesFile <- tryCatch(
        expr = localOrRemoteFile(file = rownamesFile, quiet = quiet),
        error = function(e) {
            NULL  # nocov
        }
    )
    if (!is.null(rownamesFile)) {
        rownames(object) <-
            .importMTXSidecar(file = rownamesFile, quiet = quiet)
    }
    ## Add the colnames automatically using `.colnames` sidecar file.
    colnamesFile <- paste(file, "colnames", sep = ".")
    colnamesFile <- tryCatch(
        expr = localOrRemoteFile(file = colnamesFile, quiet = quiet),
        error = function(e) {
            NULL  # nocov
        }
    )
    if (!is.null(colnamesFile)) {
        colnames(object) <-
            .importMTXSidecar(file = colnamesFile, quiet = quiet)
    }
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = "Matrix",
            fun = "readMM"
        )
    }
    object
}



## Internal importer for a sparse matrix sidecar file (e.g. `.rownames`).
## Updated 2020-01-17.
.importMTXSidecar <- function(file, quiet) {
    assert(isFlag(quiet))
    if (!isTRUE(quiet)) {
        cli_alert(sprintf("Importing sidecar {.file %s}.", basename(file)))
    }
    .importLines(file = file, quiet = quiet)
}



## List ========================================================================
## Internal importer for a JSON file (`.json`).
## Updated 2020-01-17.
.importJSON <- function(file, metadata, quiet) {
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "jsonlite", "read_json"
        ))
    }
    assert(requireNamespace("jsonlite", quietly = TRUE))
    object <- jsonlite::read_json(path = tmpfile)
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = "jsonlite",
            fun = "read_json"
        )
    }
    object
}



## Internal importer for a YAML file (`.yaml`, `.yml`).
## Updated 2020-01-17.
.importYAML <- function(file, metadata, quiet) {
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "yaml", "yaml.load_file"
        ))
    }
    assert(requireNamespace("yaml", quietly = TRUE))
    object <- yaml::yaml.load_file(input = tmpfile)
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = "yaml",
            fun = "yaml.load_file"
        )
    }
    object
}



## GSEA ========================================================================
## Internal importer for a gene matrix transposed file (`.gmt`).
## See also `fgsea::gmtPathways()`.
.importGMT <- function(file, quiet) {
    assert(isFlag(quiet))
    if (!isTRUE(quiet)) {
        cli_alert(sprintf("Importing {.file %s}.", basename(file)))
    }
    lines <- .importLines(file = file, quiet = quiet)
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
.importGMX <- function(file, quiet) {
    assert(isFlag(quiet))
    if (!isTRUE(quiet)) {
        cli_alert(sprintf("Importing {.file %s}.", basename(file)))
    }
    lines <- .importLines(file = file, quiet = quiet)
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
## Updated 2020-01-17.
.importXLSX <- function(
    file,
    sheet,
    colnames,
    metadata,
    quiet
) {
    assert(
        isScalar(sheet),
        isFlag(colnames) || isCharacter(colnames),
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "readxl", "read_excel"
        ))
    }
    assert(requireNamespace("readxl", quietly = TRUE))
    object <- readxl::read_excel(
        path = tmpfile,
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
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = "readxl",
            fun = "read_excel"
        )
    }
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
## Updated 2020-01-17.
.importXLS <- function(
    file,
    sheet,
    colnames,
    metadata,
    quiet
) {
    assert(
        isScalar(sheet),
        isFlag(colnames) || isCharacter(colnames),
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "gdata", "read.xls"
        ))
    }
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
            xls = tmpfile,
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
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = "gdata",
            fun = "read.xls"
        )
    }
    object
}



## GraphPad Prism ==============================================================
## Internal importer for a GraphPad Prism file (`.pzfx`).
## Note that this function doesn't support optional column names.
## Updated 2020-01-17.
.importPZFX <- function(
    file,
    sheet,
    metadata,
    quiet
) {
    assert(
        isScalar(sheet),
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "pzfx", "read_pzfx"
        ))
    }
    assert(requireNamespace("pzfx", quietly = TRUE))
    object <- pzfx::read_pzfx(
        path = tmpfile,
        table = sheet
    )
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = "pzfx",
            fun = "read_pzfx"
        )
    }
    object
}



## bcbio =======================================================================
## Internal importer for a bcbio count matrix file (`.counts`).
## These files contain an `"id"` column that we need to coerce to row names.
## Updated 2020-01-17.
.importBcbioCounts <- function(file, metadata, quiet) {
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "data.table", "fread"
        ))
    }
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    object <- fread(file = tmpfile, na.strings = naStrings)
    assert(
        isSubset("id", colnames(object)),
        hasNoDuplicates(object[["id"]])
    )
    object <- as.data.frame(object)
    rownames(object) <- object[["id"]]
    object[["id"]] <- NULL
    object <- as.matrix(object)
    mode(object) <- "integer"
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = "data.table",
            fun = "fread"
        )
    }
    object
}



## Handoff =====================================================================
## Updated 2020-01-17.
.rioImport <- function(file, metadata, quiet, ...) {
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "rio", "import"
        ))
    }
    assert(requireNamespace("rio", quietly = TRUE))
    object <- rio::import(file = tmpfile, ...)
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = "rio",
            fun = "import"
        )
    }
    object
}



## Using `tryCatch()` here to error if there are any warnings.
## Updated 2020-01-17.
.rtracklayerImport <- function(file, metadata, quiet, ...) {
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} using {.pkg %s}::{.fun %s}.",
            basename(file), "rtracklayer", "import"
        ))
    }
    assert(requireNamespace("rtracklayer", quietly = TRUE))
    object <- tryCatch(
        expr = rtracklayer::import(con = tmpfile, ...),
        error = function(e) {
            stop("File failed to load.")  # nocov
        },
        warning = function(w) {
            stop("File failed to load.")  # nocov
        }
    )
    if (isTRUE(metadata)) {
        object <- .slotImportMetadata(
            object = object,
            file = file,
            pkg = "rtracklayer",
            fun = "import"
        )
    }
    object
}
