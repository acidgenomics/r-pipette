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
#'   - S4 class.
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
#' @note Updated 2020-08-13.
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
#'   An optional file format type, which can be used to override the file format
#'   inferred from `file`. Only recommended for file and URL paths that don't
#'   contain an extension.
#' @param sheet `character(1)` or `integer(1)`.
#'   *Applies to Excel Workbook, Google Sheet, or GraphPad Prism file.*
#'   Sheet to read. Either a string (the name of a sheet), or an integer (the
#'   position of the sheet). Defaults to the first sheet.
#' @param skip `integer(1)`.
#'   *Applies to delimited file (CSV, TSV), Excel Workbook, or lines.*
#'   Number of lines to skip.
#' @param makeNames `function`.
#'   Apply syntactic naming function to (column) names.
#'
#' @return Varies, depending on the file type (format):
#'
#' - **Plain text delimited** (`CSV`, `TSV`, `TXT`): `data.frame`.\cr
#'   Data separated by commas, tabs, or visual spaces.\cr
#'   Note that TXT structure is amgibuous and actively discouraged.\cr
#'   Refer to `Data frame return` section for details on how to change the
#'   default return type to `DataFrame`, `tbl_df` or `data.table`.\cr
#'   Imported by [vroom::vroom()].
#' - **Excel workbook** (`XLSB`, `XLSX`): `data.frame`.\cr
#'   Resave in plain text delimited format instead, if possible.\cr
#'   Imported by [readxl::read_excel()].
#' - **Legacy Excel workbook (pre-2007)** (`XLS`): `data.frame`.\cr
#'   Resave in plain text delimited format instead, if possible.\cr
#'   Note that import of files in this format is slow.\cr
#'   Imported by [readxl::read_excel()].
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
#'   Imported by [`read_lines()`][readr::read_lines].
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
#' - [data.table](http://r-datatable.com/).
#' - [readr](http://readr.tidyverse.org).
#' - [readxl](http://readxl.tidyverse.org).
#' - [rio](https://cran.r-project.org/package=rio).
#' - [rtracklayer](http://bioconductor.org/packages/rtracklayer/).
#' - [vroom](https://vroom.r-lib.org).
#'
#' Import functions:
#'
#' - `data.table::fread()`.
#' - `readr::read_csv()`.
#' - `rio::import()`.
#' - `rtracklayer::import()`.
#' - `utils::read.table()`.
#' - `vroom::vroom()`.
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
    format = "auto",
    rownames = TRUE,
    colnames = TRUE,
    sheet = 1L,
    skip = 0L,
    makeNames,
    metadata,
    quiet
) {
    ## We're supporting remote files, so don't check using `isAFile()` here.
    assert(
        isAFile(file) || isAURL(file),
        isFlag(rownames),
        isFlag(colnames) || isCharacter(colnames),
        isString(format),
        isScalar(sheet),
        isInt(skip),
        is.function(makeNames),
        isFlag(metadata),
        isFlag(quiet)
    )
    format <- tolower(format)
    ## Allow Google Sheets import using rio, by matching the URL.
    ## Otherwise, coerce the file extension to uppercase, for easy matching.
    if (identical(format, "auto") || identical(format, "none")) {
        ext <- str_match(basename(file), extPattern)[1L, 2L]
        if (is.na(ext)) {
            stop(paste(
                "'file' argument does not contain file type extension.",
                "Set the file format manually using the 'format' argument.",
                "Refer to 'pipette::import()' documentation for details.",
                sep = "\n"
            ))
        }
    } else {
        ext <- format
    }
    ext <- tolower(ext)
    extGroup <- list(
        delim = c("csv", "fwf", "psv", "tsv", "txt"),
        excel = c("xls", "xlsb", "xlsx"),
        lines = c(
            "lines",
            "log",
            "md",        # Markdown
            "py",        # Python
            "r",         # R
            "rmd",       # R Markdown
            "sh"         # Shell
        ),
        rda = c("rda", "rdata"),
        rio = c(
            "arff",      # Weka Attribute-Relation File Format
            "dbf",       # dBase Database File
            "dif",       # Data Interchange Format
            "dta",       # Stata
            "mat",       # Matlab
            "mtp",       # Minitab
            "ods",       # OpenDocument (LibreOffice)
            "por",       # SPSS
            "sas7bdat",  # SASS
            "sav",       # SPSS
            "syd",       # Systat
            "rec",       # Epi Info
            "xpt"        # SASS
        ),
        rtracklayer = c(
            "bed", "bed15", "bedgraph", "bedpe",
            "bigwig", "bw", "wig",
            "gff", "gff1", "gff2", "gff3", "gtf",
            "broadpeak", "narrowpeak"
        ),
        yaml = c("yaml", "yml")
    )
    ## Check that user hasn't changed unsupported  arguments.
    if (!isSubset(
        x = ext,
        y = c(extGroup[["delim"]], extGroup[["excel"]])
    )) {
        assert(
            identical(rownames, eval(formals()[["rownames"]])),
            identical(colnames, eval(formals()[["colnames"]]))
        )
    }
    ## - sheet (excel, prism)
    if (!isSubset(ext, c(extGroup[["excel"]], "pzfx"))) {
        assert(identical(sheet, eval(formals()[["sheet"]])))
    }
    ## - skip
    if (!isSubset(
        x = ext,
        y = c(extGroup[["delim"]], extGroup[["excel"]], extGroup[["lines"]])
    )) {
        assert(identical(skip, eval(formals()[["skip"]])))
    }
    ## - metadata
    if (isSubset(ext, extGroup[["lines"]])) {
        assert(identical(metadata, eval(formals()[["metadata"]])))
    }
    ## Now we're ready to hand off to file-type-specific importers.
    if (isSubset(ext, extGroup[["delim"]])) {
        object <- .importDelim(
            file = file,
            ext = ext,
            colnames = colnames,
            skip = skip,
            metadata = metadata,
            quiet = quiet
        )
    } else if (isSubset(ext, extGroup[["excel"]])) {
        object <- .importExcel(
            file = file,
            colnames = colnames,
            sheet = sheet,
            skip = skip,
            metadata = metadata,
            quiet = quiet
        )
    } else if (identical(ext, "pzfx")) {
        ## GraphPad Prism project.
        ## Note that Prism files always contain column names.
        object <- .importPZFX(
            file = file,
            sheet = sheet,
            metadata = metadata,
            quiet = quiet
        )
    } else if (identical(ext, "rds")) {
        object <- .importRDS(file = file, quiet = quiet)
    } else if (isSubset(ext, extGroup[["rda"]])) {
        object <- .importRDA(file = file, quiet = quiet)
    } else if (identical(ext, "gmt")) {
        object <- .importGMT(file = file, quiet = quiet)
    } else if (identical(ext, "gmx")) {
        object <- .importGMX(file = file, quiet = quiet)
    } else if (identical(ext, "grp")) {
        object <- .importGRP(file = file, quiet = quiet)
    } else if (identical(ext, "json")) {
        object <- .importJSON(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else if (isSubset(ext, extGroup[["yaml"]])) {
        object <- .importYAML(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else if (identical(ext, "mtx")) {
        ## We're always requiring row and column sidecar files for MTX.
        object <- .importMTX(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else if (identical(ext, "counts")) {
        ## bcbio counts format always contains row and column names.
        object <- .importBcbioCounts(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else if (isSubset(ext, extGroup[["lines"]])) {
        object <- .importLines(
            file = file,
            skip = skip,
            quiet = quiet
        )
    } else if (isSubset(ext, extGroup[["rtracklayer"]])) {
        object <- .rtracklayerImport(
            file = file,
            metadata = metadata,
            quiet = quiet
        )
    } else if (isSubset(ext, extGroup[["rio"]])) {
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
        if (isTRUE(rownames) && isSubset("rowname", colnames(object))) {
            if (!isTRUE(quiet)) {
                cli_alert_info("Setting row names from {.var rowname} column.")
            }
            rownames(object) <- object[["rowname"]]
            object[["rowname"]] <- NULL
        }
    }
    if (hasNames(object)) {
        if (isTRUE(any(duplicated(names(object))))) {
            dupes <- sort(names(object)[duplicated(names(object))])
            cli_alert_warning(sprintf(
                "Duplicate names: {.var %s}.",
                toString(dupes, width = 100L)
            ))
        }
        names(object) <- makeNames(names(object))
        if (!isTRUE(hasValidNames(object))) {
            cli_alert_warning("Invalid names detected.")
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

formals(import)[c("makeNames", "metadata", "quiet")] <-
    formalsList[c("import.make.names", "import.metadata", "quiet")]



## Add data provenance metadata.
## Previously, "which" was defined as "pipette", until v0.3.8.
## Updated 2019-10-24.
.slotImportMetadata <- function(object, file, pkg, fun) {
    assert(
        isString(file),
        isString(pkg),
        isString(fun)
    )
    metadata2(object, which = "import") <- list(
        package = "pipette",
        packageVersion = packageVersion("pipette"),
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
## Calls `vroom::vroom()` internally by default.
## Can override using `acid.import.engine` option, which also supports
## data.table and readr packages.
## Updated 2020-08-13.
.importDelim <- function(
    file,
    colnames,
    ext,
    skip,
    metadata,
    quiet
) {
    verbose <- getOption("acid.verbose", default = FALSE)
    assert(
        isFlag(colnames) || isCharacter(colnames),
        isString(ext),
        isInt(skip),
        isFlag(metadata),
        isFlag(quiet),
        isFlag(verbose)
    )
    whatPkg <- match.arg(
        arg = getOption("acid.import.engine", default = "vroom"),
        choices = c("data.table", "readr", "vroom")
    )
    requireNamespaces(whatPkg)
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (identical(whatPkg, "data.table")) {
        ## data.table ----------------------------------------------------------
        whatFun <- "fread"
        what <- get(
            x = whatFun,
            envir = asNamespace(whatPkg),
            inherits = TRUE
        )
        args <- list(
            file = tmpfile,
            blank.lines.skip = TRUE,
            check.names = TRUE,
            data.table = FALSE,
            fill = FALSE,
            na.strings = naStrings,
            skip = skip,
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
        whatFun <- switch(
            EXPR = ext,
            "csv" = "read_csv",
            "tsv" = "read_tsv",
            "txt" = "read_delim"
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
            skip = skip,
            skip_empty_rows = TRUE
        )
    } else if (identical(whatPkg, "vroom")) {
        ## vroom ---------------------------------------------------------------
        whatFun <- "vroom"
        what <- get(
            x = whatFun,
            envir = asNamespace(whatPkg),
            inherits = TRUE
        )
        delim <- switch(
            EXPR = ext,
            "csv" = ",",
            "tsv" = "\t",
            "txt" = " "
        )
        args <- list(
            file = tmpfile,
            delim = delim,
            col_names = colnames,
            col_types = vroom::cols(),
            na = naStrings,
            progress = FALSE,
            skip = skip,
            trim_ws = TRUE,
            .name_repair = make.names
        )
    }
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            whatPkg, whatFun
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
## Updated 2020-08-13.
.importLines <- function(file, skip = 0L, quiet) {
    assert(
        isInt(skip),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "readr", "read_lines"
        ))
    }
    read_lines(
        file = file,
        skip = skip,
        skip_empty_rows = FALSE,
        progress = FALSE
    )
}



## R data ======================================================================
## Internal importer for an R data serialized file (`.rds`).
## Updated 2020-08-13.
.importRDS <- function(file, quiet) {
    assert(isFlag(quiet))
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "base", "readRDS"
        ))
    }
    object <- readRDS(file = tmpfile)
    object
}



## Internal importer for an R data file (`.rda`).
## Updated 2020-08-13.
.importRDA <- function(file, quiet) {
    assert(isFlag(quiet))
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "base", "load"
        ))
    }
    safe <- new.env()
    object <- load(file = tmpfile, envir = safe)
    if (!isTRUE(hasLength(safe, n = 1L))) {
        stop(sprintf("'%s' does not contain a single object.", basename(file)))
    }
    object <- get(object, envir = safe, inherits = FALSE)
    object
}



## Sparse matrix ===============================================================
## Internal importer for a sparse matrix file (`.mtx`).
## Updated 2020-08-13.
.importMTX <- function(file, metadata, quiet) {
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "Matrix", "readMM"
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
## Updated 2020-08-13.
.importMTXSidecar <- function(file, quiet) {
    assert(isFlag(quiet))
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing sidecar {.file %s} at {.path %s}.",
            basename(file), realpath(dirname(file))
        ))
    }
    .importLines(file = file, quiet = quiet)
}



## List ========================================================================
## Internal importer for a JSON file (`.json`).
## Updated 2020-08-13.
.importJSON <- function(file, metadata, quiet) {
    requireNamespaces("jsonlite")
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "jsonlite", "read_json"
        ))
    }
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
## Updated 2020-08-13.
.importYAML <- function(file, metadata, quiet) {
    requireNamespaces("yaml")
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "yaml", "yaml.load_file"
        ))
    }
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
## Updated 2020-08-13.
.importGMT <- function(file, quiet) {
    assert(isFlag(quiet))
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s}.",
            basename(file), realpath(dirname(file))
        ))
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
## Updated 2020-08-13.
.importGMX <- function(file, quiet) {
    assert(isFlag(quiet))
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s}.",
            basename(file), realpath(dirname(file))
        ))
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
## Updated 2020-08-13.
.importExcel <- function(
    file,
    sheet,
    colnames,
    skip,
    metadata,
    quiet
) {
    requireNamespaces("readxl")
    assert(
        isScalar(sheet),
        isFlag(colnames) || isCharacter(colnames),
        isInt(skip),
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "readxl", "read_excel"
        ))
    }
    ## Note that `tryCatch()` or `withCallingHandlers()` doesn't work here.
    ## http://adv-r.had.co.nz/Exceptions-Debugging.html
    warn <- getOption("warn")
    options(warn = 2L)
    object <- readxl::read_excel(
        path = tmpfile,
        sheet = sheet,
        col_names = colnames,
        na = naStrings,
        trim_ws = TRUE,
        skip = skip,
        progress = FALSE,
        .name_repair = make.names
    )
    options(warn = warn)
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



## GraphPad Prism ==============================================================
## Internal importer for a GraphPad Prism file (`.pzfx`).
## Note that this function doesn't support optional column names.
## Updated 2020-08-13.
.importPZFX <- function(
    file,
    sheet,
    metadata,
    quiet
) {
    requireNamespaces("pzfx")
    assert(
        isScalar(sheet),
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "pzfx", "read_pzfx"
        ))
    }
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
## Updated 2020-08-13.
.importBcbioCounts <- function(file, metadata, quiet) {
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "data.table", "fread"
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
## Updated 2020-08-13.
.rioImport <- function(file, metadata, quiet, ...) {
    requireNamespaces("rio")
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "rio", "import"
        ))
    }
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
## Updated 2020-08-13.
.rtracklayerImport <- function(file, metadata, quiet, ...) {
    requireNamespaces("rtracklayer")
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        cli_alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), realpath(dirname(file)),
            "rtracklayer", "import"
        ))
    }
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
