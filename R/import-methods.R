## FIXME Need to improve the documentation for specific classes...



#' Import
#'
#' Read file by extension into R.
#'
#' `import()` supports automatic loading of common file types, by wrapping
#' popular importer functions. It intentionally designed to be simple, with few
#' arguments. Remote URLs and compressed files are supported. If you need more
#' complex import settings, just call the wrapped importer directly instead.
#'
#' @section Row and column names:
#'
#' **Row names.** Row name handling has become an inconsistent mess in R because
#' of differential support in base R, tidyverse, data.table, and Bioconductor.
#' To maintain sanity, `import()` attempts to handle row names automatically.
#' The function checks for a `rowname` column in delimited data, and moves these
#' values into the object's row names, if supported by the return type (e.g.
#' `data.frame`, `DataFrame`). Note that `tbl_df` (tibble) and `data.table`
#' intentionally do not support row names. When returning in this format, no
#' attempt to assign the `rowname` column into the return object's row names is
#' made. Note that `import()` is strict about this matching and only checks for
#' a `rowname` column, similar to the default syntax recommended in
#' `tibble::rownames_to_column()`. To disable this behavior, set
#' `rownames = FALSE`, and no attempt will be made to set the row names.
#'
#' **Column names.** `import()` assumes that delimited files always contain
#' column names. If you are working with a file that doesn't contain column
#' names, either set `colnames = FALSE` or pass the names in as a `character`
#' vector. It's strongly recommended to always define column names in a
#' supported file type.
#'
#' @section Data frame return:
#'
#' By default, `import()` returns a standard `data.frame` for delimited/column
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
#' @section Matrix Market Exchange (`MTXFile`):
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
#' @section GSEA gene set files (`GMTFile`, `GMXFile`, `GRPFile`):
#'
#' Refer to the Broad Institute [GSEA wiki][] for details.
#'
#' [GSEA wiki]: https://goo.gl/3ZkDPb
#'
#' @section `BcbioCountsFile`:
#'
#' [bcbio][] count matrix and related sidecar files are natively supported.
#'
#' - `COUNTS`: Counts table (e.g. RNA-seq aligned counts).
#' - `COLNAMES`: Sidecar file containing column names.
#' - `ROWNAMES`: Sidecar file containing row names.
#'
#' [bcbio]: https://bcbio-nextgen.readthedocs.io/
#'
#' @section Denylisted extensions:
#'
#' These file formats are intentionally not supported:
#' `DOC`, `DOCX`, `PDF`, `PPT`, `PPTX`.
#'
#' @name import
#' @note Updated 2021-06-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param format `character(1)`.
#'   An optional file format type, which can be used to override the file format
#'   inferred from `file`. Only recommended for file and URL paths that don't
#'   contain an extension.
#' @param rownames `logical(1)`.
#'   Automatically assign row names, if `rowname` column is defined.
#'   Applies to file types that return `data.frame` only.
#' @param colnames `logical(1)` or `character`.
#'   Automatically assign column names, using the first header row.
#'   Applies to file types that return `data.frame` only.
#'   Pass in a `character` vector to define the column names manually.
#' @param sheet `character(1)` or `integer(1)`.
#'   *Applies to Excel Workbook, Google Sheet, or GraphPad Prism file.*
#'   Sheet to read. Either a string (the name of a sheet), or an integer (the
#'   position of the sheet). Defaults to the first sheet.
#' @param comment `character(1)`.
#'   Comment character to detect at beginning of line, which will skip when
#'   parsing file. Use `""` to disable interpretation of comments, which is
#'   particularly
#'   useful when parsing lines.
#'   *Applies to plain text delimited and source code lines only.*
#' @param skip `integer(1)`.
#'   *Applies to delimited file (CSV, TSV), Excel Workbook, or lines.*
#'   Number of lines to skip.
#' @param nMax `integer(1)` or `Inf`.
#'   Maximum number of lines to parse.
#'   *Applies to plain text delimited, Excel, and source code lines only.*
#' @param makeNames `function`.
#'   Apply syntactic naming function to (column) names.
#'   *Does not apply to import of R data files.*
#'
#' @return Varies, depending on the file type (format):
#'
#' - **Plain text delimited** (`CSV`, `TSV`, `TXT`): `data.frame`.\cr
#'   Data separated by commas, tabs, or visual spaces.\cr
#'   Note that TXT structure is amgibuous and actively discouraged.\cr
#'   Refer to `Data frame return` section for details on how to change the
#'   default return type to `DataFrame`, `tbl_df` or `data.table`.\cr
#'   Imported by `data.table::fread()` by default.
#' - **Excel workbook** (`XLSB`, `XLSX`): `data.frame`.\cr
#'   Resave in plain text delimited format instead, if possible.\cr
#'   Imported by `readxl::read_excel()`.
#' - **Legacy Excel workbook (pre-2007)** (`XLS`): `data.frame`.\cr
#'   Resave in plain text delimited format instead, if possible.\cr
#'   Note that import of files in this format is slow.\cr
#'   Imported by `readxl::read_excel()`.
#' - **GraphPad Prism project** (`PZFX`): `data.frame`.\cr
#'   Experimental. Consider resaving in CSV format instead.\cr
#'   Imported by `pzfx::read_pzfx()`.
#' - **General feature format** (`GFF`, `GFF1`, `GFF2`, `GFF3`, `GTF`):
#'   `GRanges`.\cr
#'   Imported by `rtracklayer::import()`.
#' - **MatrixMarket exchange sparse matrix** (`MTX`): `sparseMatrix`.\cr
#'   Imported by `Matrix::readMM()`.
#' - **Gene sets (for GSEA)** (`GMT`, `GMX`): `character`.
#' - **Browser extensible data** (`BED`, `BED15`, `BEDGRAPH`, `BEDPE`):
#'   `GRanges`.\cr
#'   Imported by `rtracklayer::import()`.
#' - **ChIP-seq peaks** (`BROADPEAK`, `NARROWPEAK`): `GRanges`.\cr
#'   Imported by `rtracklayer::import()`.
#' - **Wiggle track format** (`BIGWIG`, `BW`, `WIG`): `GRanges`.\cr
#'   Imported by `rtracklayer::import()`.
#' - **JSON serialization data** (`JSON`): `list`.\cr
#'   Imported by `jsonlite::read_json()`.
#' - **YAML serialization data** (`YAML`, `YML`): `list`.\cr
#'   Imported by `yaml::yaml.load_file()`.
#' - **Lines** (`LOG`, `MD`, `PY`, `R`, `RMD`, `SH`): `character`.
#'   Source code or log files.\cr
#'   Imported by `data.table::fread()` by default.
#' - **R data serialized** (`RDS`): *variable*.\cr
#'   Currently recommend over RDA, if possible.\cr
#'   Imported by `readRDS()`.
#' - **R data** (`RDA`, `RDATA`): *variable*.\cr
#'   Must contain a single object.
#'   Doesn't require internal object name to match, unlike `loadData()`.\cr
#'   Imported by `load()`.
#' - **Infrequently used rio-compatible formats** (`ARFF`, `DBF`, `DIF`, `DTA`,
#'   `MAT`, `MTP`, `ODS`, `POR`, `SAS7BDAT`, `SAV`, `SYD`, `REC`, `XPT`):
#'   *variable*.\cr
#'   Imported by `rio::import()`.
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
NULL



## Internal functions ==========================================================
#' Map file extension to corresponding S4 file class
#'
#' @note Updated 2021-06-04.
#' @noRd
.extToFileClass <- function(ext) {
    ext <- tolower(ext)
    dict <- list(
        "arff"         = "RioFile",
        "bash"         = "LinesFile",
        "bcbio-counts" = "BcbioCountsFile",
        "bed"          = "RtracklayerFile",
        "bed15"        = "RtracklayerFile",
        "bedgraph"     = "RtracklayerFile",
        "bedpe"        = "RtracklayerFile",
        "bigwig"       = "RtracklayerFile",
        "broadpeak"    = "RtracklayerFile",
        "bw"           = "RtracklayerFile",
        "counts"       = "BcbioCountsFile",
        "csv"          = "CSVFile",
        "dbf"          = "RioFile",
        "dif"          = "RioFile",
        "dta"          = "RioFile",
        "excel"        = "ExcelFile",
        "fwf"          = "RioFile",
        "gff"          = "RtracklayerFile",
        "gff1"         = "RtracklayerFile",
        "gff2"         = "RtracklayerFile",
        "gff3"         = "RtracklayerFile",
        "gmt"          = "GMTFile",
        "gmx"          = "GMXFile",
        "grp"          = "GRPFile",
        "gtf"          = "RtracklayerFile",
        "json"         = "JSONFile",
        "lines"        = "LinesFile",
        "log"          = "LinesFile",
        "mat"          = "RioFile",
        "md"           = "LinesFile",
        "mtp"          = "RioFile",
        "mtx"          = "MTXFile",
        "narrowpeak"   = "RtracklayerFile",
        "ods"          = "RioFile",
        "por"          = "RioFile",
        "psv"          = "RioFile",
        "py"           = "LinesFile",
        "pzfx"         = "PZFXFile",
        "r"            = "LinesFile",
        "rda"          = "RDataFile",
        "rdata"        = "RDataFile",
        "rds"          = "RDSFile",
        "rec"          = "RioFile",
        "rio"          = "RioFile",
        "rmd"          = "LinesFile",
        "rtracklayer"  = "RtracklayerFile",
        "sas7bdat"     = "RioFile",
        "sav"          = "RioFile",
        "sh"           = "LinesFile",
        "syd"          = "RioFile",
        "table"        = "TableFile",
        "tsv"          = "TSVFile",
        "txt"          = "TableFile",
        "wig"          = "RtracklayerFile",
        "xls"          = "ExcelFile",
        "xlsb"         = "ExcelFile",
        "xlsx"         = "ExcelFile",
        "xpt"          = "RioFile",
        "yaml"         = "YAMLFile",
        "yml"          = "YAMLFile",
        "zsh"          = "LinesFile"
    )
    if (!isSubset(ext, names(dict))) {
        stop(sprintf("Import failure. '%s' extension is not supported.", ext))
    }
    class <- dict[[ext]]
    assert(isString(class))
    class
}



#' Internal importer for a sparse matrix sidecar file (e.g. `.rownames`)
#'
#' @note Updated 2021-06-04.
#' @noRd
.importMTXSidecar <- function(file, quiet) {
    assert(
        isString(file),
        isFlag(quiet)
    )
    if (!isTRUE(quiet)) {
        where <- ifelse(
            test = isAURL(file),
            yes = dirname(file),
            no = realpath(dirname(file))
        )
        alert(sprintf(
            "Importing sidecar {.file %s} at {.path %s}.",
            basename(file), where
        ))
    }
    object <- import(
        file = file,
        format = "lines",
        quiet = quiet
    )
    object
}



#' Return standardized import object
#'
#' @note Updated 2021-06-04.
#' @noRd
.returnImport <- function(
    object,
    file,
    rownames,
    colnames,
    metadata,
    whatPkg,
    whatFun,
    quiet
) {
    validObject(object)
    file <- as.character(file)
    assert(
        isString(file),
        isFlag(rownames),
        isFlag(colnames) || isCharacter(colnames),
        isFlag(metadata),
        isString(whatPkg, nullOK = TRUE),
        isString(whatFun, nullOK = TRUE),
        isFlag(quiet)
    )
    ## Check that manual column names are correct.
    if (isCharacter(colnames)) {
        assert(identical(colnames(object), colnames))
    }
    ## Data frame-specific operations.
    if (is.data.frame(object)) {
        ## Set row names automatically.
        if (isTRUE(rownames) && isSubset("rowname", colnames(object))) {
            if (!isTRUE(quiet)) {
                alertInfo("Setting row names from {.var rowname} column.")
            }
            rownames(object) <- object[["rowname"]]
            object[["rowname"]] <- NULL
        }
    }
    if (hasNames(object)) {
        if (isTRUE(any(duplicated(names(object))))) {
            dupes <- sort(names(object)[duplicated(names(object))])
            alertWarning(sprintf(
                "Duplicate names: {.var %s}.",
                toString(dupes, width = 100L)
            ))
        }
        ## Harden against any object classes that don't support names
        ## assignment, to prevent unwanted error on this step.
        tryCatch(
            expr = {
                names(object) <- makeNames(names(object))
            },
            error = function(e) NULL
        )
        if (!isTRUE(hasValidNames(object))) {
            alertWarning("Invalid names detected.")
        }
    }
    if (isTRUE(metadata)) {
        metadata2(object, which = "import") <-
            list(
                "date" = Sys.Date(),
                "file" = ifelse(
                    test = isTRUE(isAFile(file)),
                    yes = realpath(file),
                    no = file
                ),
                "importerName" = paste0(whatPkg, "::", whatFun),
                "importerVersion" = packageVersion(whatPkg),
                "packageName" = .pkgName,
                "packageVersion" = .pkgVersion
            )
    }
    object
}



## S4 method definitions =======================================================
#' Primary `import` method, that hands off to classed file-extension variants
#'
#' @details
#' We're supporting remote files, so don't check using `isAFile()` here.
#'
#' Allow Google Sheets import using rio, by matching the URL.
#' Otherwise, coerce the file extension to uppercase, for easy matching.
#'
#' @note Updated 2021-06-03.
#' @noRd
`import,character` <-  # nolint
    function(file, format = "auto", ...) {
        assert(
            isAFile(file) || isAURL(file),
            isString(format)
        )
        format <- tolower(format)
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
        class <- .extToFileClass(ext)
        file <- new(Class = class, file)
        import(file = file, ...)
    }



#' Import a delimited file (e.g. `.csv`, `.tsv`).
#'
#' @details
#' Calls `data.table::fread()` internally by default.
#' Can override using `acid.import.engine` option, which also supports
#' data.table and readr packages.
#'
#' @note Updated 2021-06-04.
#' @noRd
`import,DelimFile` <-  # nolint
    function(
        file,
        rownames = TRUE,
        colnames = TRUE,
        comment = "",
        skip = 0L,
        nMax = Inf,
        makeNames,
        engine,
        metadata,
        quiet,
        verbose
    ) {
        assert(
            isString(file),
            isFlag(rownames),
            isFlag(colnames) || isCharacter(colnames),
            is.character(comment) && length(comment) <= 1L,
            isInt(skip), isNonNegative(skip),
            isPositive(nMax),
            is.function(makeNames),
            isString(engine),
            isFlag(metadata),
            isFlag(quiet),
            isFlag(verbose)
        )
        if (isTRUE(quiet)) {
            assert(isFALSE(verbose))
        }
        ext <- switch(
            EXPR = class(file),
            "CSVFile" = "csv",
            "TSVFile" = "tsv",
            "TableFile" = "table",
            stop("Unsupported delim class.")
        )
        whatPkg <- match.arg(arg = engine, choices = .engines)
        if (identical(ext, "table")) {
            whatPkg <- "base"
        }
        requireNamespaces(whatPkg)
        tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
        switch(
            EXPR = whatPkg,
            "base" = {
                whatFun <- "read.table"
                args <- list(
                    "file" = tmpfile,
                    "blank.lines.skip" = TRUE,
                    "comment.char" = comment,
                    "na.strings" = naStrings,
                    "nrows" = nMax,
                    "sep" = switch(
                        EXPR = ext,
                        "csv" = ",",
                        "table" = "",
                        "tsv" = "\t"
                    ),
                    "skip" = skip,
                    "stringsAsFactors" = FALSE,
                    "strip.white" = TRUE
                )
                if (isCharacter(colnames)) {
                    args[["header"]] <- FALSE
                    args[["col.names"]] <- colnames
                } else {
                    args[["header"]] <- colnames
                }
            },
            "data.table" = {
                whatFun <- "fread"
                if (isString(comment)) {
                    ## nocov start
                    stop(sprintf(
                        paste0(
                            "'%s::%s' does not support comment exclusion.\n",
                            "See '%s' for details."
                        ),
                        whatPkg, whatFun,
                        "https://github.com/Rdatatable/data.table/issues/856"
                    ))
                    ## nocov end
                }
                args <- list(
                    "file" = tmpfile,
                    "blank.lines.skip" = TRUE,
                    "check.names" = TRUE,
                    "data.table" = FALSE,
                    "fill" = FALSE,
                    "na.strings" = naStrings,
                    "nrows" = nMax,
                    "skip" = skip,
                    "showProgress" = FALSE,
                    "stringsAsFactors" = FALSE,
                    "strip.white" = TRUE,
                    "verbose" = verbose
                )
                if (isCharacter(colnames)) {
                    args[["header"]] <- FALSE
                    args[["col.names"]] <- colnames
                } else {
                    args[["header"]] <- colnames
                }
            },
            "readr" = {
                whatFun <- "read_delim"
                args <- list(
                    "file" = tmpfile,
                    "col_names" = colnames,
                    "col_types" = readr::cols(),
                    "comment" = comment,
                    "delim" = switch(
                        EXPR = ext,
                        "csv" = ",",
                        "tsv" = "\t"
                    ),
                    "na" = naStrings,
                    "n_max" = nMax,
                    "progress" = FALSE,
                    "trim_ws" = TRUE,
                    "show_col_types" = TRUE,
                    "skip" = skip,
                    "skip_empty_rows" = TRUE
                )
            },
            "vroom" = {
                whatFun <- "vroom"
                args <- list(
                    "file" = tmpfile,
                    "delim" = switch(
                        EXPR = ext,
                        "csv" = ",",
                        "tsv" = "\t"
                    ),
                    "col_names" = colnames,
                    "col_types" = vroom::cols(),
                    "comment" = comment,
                    "na" = naStrings,
                    "n_max" = nMax,
                    "progress" = FALSE,
                    "skip" = skip,
                    "trim_ws" = TRUE,
                    ".name_repair" = make.names
                )
            }
        )
        if (!isTRUE(quiet)) {
            where <- ifelse(
                test = isAURL(file),
                yes = dirname(file),
                no = realpath(dirname(file))
            )
            alert(sprintf(
                paste(
                    "Importing {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), where,
                whatPkg, whatFun
            ))
        }
        what <- get(x = whatFun, envir = asNamespace(whatPkg), inherits = TRUE)
        assert(is.function(what))
        object <- do.call(what = what, args = args)
        assert(is.data.frame(object))
        if (!identical(class(object), "data.frame")) {
            object <- as.data.frame(
                x = object,
                make.names = FALSE,
                stringsAsFactors = FALSE
            )
        }
        ## FIXME Check that colnames input matches, if character.
        .returnImport(
            object = object,
            file = file,
            rownames = rownames,
            colnames = colnames,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }

## FIXME Define "verbose" and "engine" in AcidBase formalsList.
formals(`import,DelimFile`)[c("engine", "verbose")] <-
    .formalsList[c("engine", "verbose")]
formals(`import,DelimFile`)[c("makeNames", "metadata", "quiet")] <-
    formalsList[c("import.make.names", "import.metadata", "quiet")]



## FIXME Need to support stripWhitespace (for all engines).
## FIXME Need to support removeBlank.

#' Import source code lines
#'
#' @note Updated 2021-06-04.
#' @noRd
`import,LinesFile` <-
    function(
        file,
        comment = "",
        skip = 0L,
        nMax = Inf,
        stripWhitespace = FALSE,
        removeBlank = FALSE,
        metadata,
        engine,
        quiet
    ) {
        assert(
            isString(file),
            is.character(comment) && length(comment) <= 1L,
            isInt(skip),
            isInt(skip), isNonNegative(skip),
            isPositive(nMax),
            isFlag(stripWhitespace),
            isFlag(removeBlank),
            isFlag(quiet)
        )
        if (isString(comment)) {
            assert(is.infinite(nMax))
        }
        whatPkg <- match.arg(arg = engine, choices = .engines)
        requireNamespaces(whatPkg)
        tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
        ## FIXME Need to support stripWhitespace, and removeBlank.
        switch(
            EXPR = whatPkg,
            "base" = {
                whatFun <- "readLines"
                args <- list(
                    "con" = tmpfile,
                    "warn" = FALSE
                )
            },
            "data.table" = {
                whatFun <- "fread"
                args <- list(
                    "file" = tmpfile,
                    "blank.lines.skip" = FALSE,
                    "header" = FALSE,
                    "nrows" = nMax,
                    "sep" = "\n",
                    "skip" = skip,
                    "strip.white" = stripWhitespace
                )
            },
            "readr" = {
                whatFun <- "read_lines"
                args <- list(
                    "file" = tmpfile,
                    "n_max" = nMax,
                    "progress" = FALSE,
                    "skip" = skip,
                    "skip_empty_rows" = FALSE
                )
            },
            "vroom" = {
                whatFun <- "vroom_lines"
                args <- list(
                    "file" = tmpfile,
                    "n_max" = nMax,
                    "progress" = FALSE,
                    "skip" = skip
                )
            }
        )
        if (!isTRUE(quiet)) {
            where <- ifelse(
                test = isAURL(file),
                yes = dirname(file),
                no = realpath(dirname(file))
            )
            alert(sprintf(
                paste(
                    "Importing {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), where,
                whatPkg, whatFun
            ))
        }
        if (isTRUE(file.size(tmpfile) == 0L)) {
            return(character())
        }
        what <- get(x = whatFun, envir = asNamespace(whatPkg), inherits = TRUE)
        assert(is.function(what))
        object <- do.call(what = what, args = args)
        if (identical(whatPkg, "data.table")) {
            object <- object[[1L]]
        }
        assert(is.character(object))
        if (isString(comment)) {
            keep <- !grepl(pattern = paste0("^", comment), x = object)
            object <- object[keep]
        }
        if (identical(whatPkg, "base")) {
            if (isTRUE(skip > 0L)) {
                assert(skip < length(object))
                start <- skip + 1L
                end <- length(object)
                object <- object[start:end]
            }
            if (isTRUE(nMax < length(object))) {
                object <- object[1L:nMax]
            }
        }
        .returnImport(
            object = object,
            file = file,
            rownames = FALSE,
            colnames = FALSE,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }

## FIXME Define "verbose" and "engine" in AcidBase formalsList.
formals(`import,LinesFile`)[["engine"]] <-
    .formalsList[["engine"]]
formals(`import,LinesFile`)[c("metadata", "quiet")] <-
    formalsList[c("import.metadata", "quiet")]



#' Import a JSON file (`.json`)
#'
#' @note Updated 2021-06-04.
#' @noRd
`import,JSONFile` <-  # nolint
    function(file, metadata, quiet) {
        requireNamespaces("jsonlite")
        assert(
            isFlag(metadata),
            isFlag(quiet)
        )
        whatPkg <- "jsonlite"
        whatFun <- "read_json"
        tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
        if (!isTRUE(quiet)) {
            where <- ifelse(
                test = isAURL(file),
                yes = dirname(file),
                no = realpath(dirname(file))
            )
            alert(sprintf(
                paste(
                    "Importing {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), where,
                "jsonlite", "read_json"
            ))
        }
        object <- jsonlite::read_json(path = tmpfile)
        .returnImport(
            object = object,
            file = file,
            rownames = FALSE,
            colnames = FALSE,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }

formals(`import,JSONFile`)[c("metadata", "quiet")] <-
    formalsList[c("import.metadata", "quiet")]



#' Import a sparse matrix file (`.mtx`)
#'
#' @note Updated 2021-06-04.
#' @noRd
`import,MTXFile` <-
    function(file, metadata, quiet) {
        assert(
            isFlag(metadata),
            isFlag(quiet)
        )
        rownames <- FALSE
        colnames <- FALSE
        whatPkg <- "Matrix"
        whatFun = "readMM"
        tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
        if (!isTRUE(quiet)) {
            where <- ifelse(
                test = isAURL(file),
                yes = dirname(file),
                no = realpath(dirname(file))
            )
            alert(sprintf(
                paste(
                    "Importing {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), where,
                whatPkg, whatFun
            ))
        }
        object <- readMM(file = tmpfile)
        ## Add the rownames automatically using `.rownames` sidecar file.
        rownamesFile <- tryCatch(
            expr = localOrRemoteFile(
                file = paste(file, "rownames", sep = "."),
                quiet = quiet
            ),
            error = function(e) {
                NULL  # nocov
            }
        )
        if (!is.null(rownamesFile)) {
            rownames <- TRUE
            rownames(object) <-
                .importMTXSidecar(file = rownamesFile, quiet = quiet)
        }
        ## Add the colnames automatically using `.colnames` sidecar file.
        colnamesFile <-
        colnamesFile <- tryCatch(
            expr = localOrRemoteFile(
                file = paste(file, "colnames", sep = "."),
                quiet = quiet
            ),
            error = function(e) {
                NULL  # nocov
            }
        )
        if (!is.null(colnamesFile)) {
            colnames <- TRUE
            colnames(object) <-
                .importMTXSidecar(file = colnamesFile, quiet = quiet)
        }
        .returnImport(
            object = object,
            file = file,
            rownames = rownames,
            colnames = colnames,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }

formals(`import,MTXFile`)[c("metadata", "quiet")] <-
    formalsList[c("import.metadata", "quiet")]



#' Import a file using `rio::import`
#'
#' @note Updated 2021-06-04.
#' @noRd
`import,RioFile` <-  # nolint
    function(
        file,
        rownames = TRUE,
        colnames = TRUE,
        metadata,
        quiet,
        ...
    ) {
        requireNamespaces("rio")
        assert(
            isString(file),
            isFlag(rownames),
            isFlag(colnames) || isCharacter(colnames),
            isFlag(metadata),
            isFlag(quiet)
        )
        whatPkg <- "rio"
        whatFun <- "import"
        tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
        if (!isTRUE(quiet)) {
            where <- ifelse(
                test = isAURL(file),
                yes = dirname(file),
                no = realpath(dirname(file))
            )
            alert(sprintf(
                paste(
                    "Importing {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), where,
                whatPkg, whatFun
            ))
        }
        object <- rio::import(file = tmpfile, ...)
        .returnImport(
            object = object,
            file = file,
            rownames = rownames,
            colnames = colnames,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }

formals(`import,RioFile`)[c("metadata", "quiet")] <-
    formalsList[c("import.metadata", "quiet")]



#' Import an R data serialized file (`.rds`)
#'
#' @note Updated 2021-06-04.
#' @noRd
`import,RDSFile` <-
    function(file, quiet) {
        assert(
            isString(file),
            isFlag(quiet)
        )
        tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
        if (!isTRUE(quiet)) {
            where <- ifelse(
                test = isAURL(file),
                yes = dirname(file),
                no = realpath(dirname(file))
            )
            alert(sprintf(
                paste(
                    "Importing {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), where,
                "base", "readRDS"
            ))
        }
        object <- readRDS(file = tmpfile)
        validObject(object)
        object
    }

formals(`import,RDSFile`)[["quiet"]] <-
    formalsList[["quiet"]]



#' Import an R data file containing multiple objects (`.rda`)
#'
#' @note Updated 2021-06-04.
#' @noRd
`import,RDataFile` <-
    function(file, quiet) {
        assert(
            isString(file),
            isFlag(quiet)
        )
        tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
        if (!isTRUE(quiet)) {
            where <- ifelse(
                test = isAURL(file),
                yes = dirname(file),
                no = realpath(dirname(file))
            )
            alert(sprintf(
                paste(
                    "Importing {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), where,
                "base", "load"
            ))
        }
        safe <- new.env()
        object <- load(file = tmpfile, envir = safe)
        if (!isTRUE(hasLength(safe, n = 1L))) {
            stop(sprintf(
                "'%s' does not contain a single object.",
                basename(file)
            ))
        }
        object <- get(object, envir = safe, inherits = FALSE)
        validObject(object)
        object
    }

formals(`import,RDataFile`)[["quiet"]] <-
    formalsList[["quiet"]]





















#' Import a YAML file (`.yaml`, `.yml`)
#'
#' @note Updated 2021-06-04.
#' @noRd
`import,YAMLFile` <-  # nolint
    function(file, metadata, quiet) {
        requireNamespaces("yaml")
        assert(
            isFlag(metadata),
            isFlag(quiet)
        )
        whatPkg <- "yaml"
        whatFun <- "yaml.load_file"
        tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
        if (!isTRUE(quiet)) {
            where <- ifelse(
                test = isAURL(file),
                yes = dirname(file),
                no = realpath(dirname(file))
            )
            alert(sprintf(
                paste(
                    "Importing {.file %s} at {.path %s}",
                    "using {.pkg %s}::{.fun %s}."
                ),
                basename(file), where,
                whatPkg, whatFun
            ))
        }
        object <- yaml::yaml.load_file(input = tmpfile)
        .returnImport(
            object = object,
            file = file,
            rownames = FALSE,
            colnames = FALSE,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }

formals(`import,YAMLFile`)[c("metadata", "quiet")] <-
    formalsList[c("import.metadata", "quiet")]



## GSEA ========================================================================
#' Internal importer for a gene matrix transposed file (`.gmt`)
#'
#' @note Updated 2020-08-13
#' @noRd
#'
#' @seealso `fgsea::gmtPathways()`.
.importGMT <- function(file, quiet) {
    assert(isFlag(quiet))
    if (!isTRUE(quiet)) {
        where <- ifelse(
            test = isAURL(file),
            yes = dirname(file),
            no = realpath(dirname(file))
        )
        alert(sprintf(
            "Importing {.file %s} at {.path %s}.",
            basename(file), where
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



#' Internal importer for a gene matrix file (`.gmx`)
#'
#' @note Updated 2020-08-13.
#' @noRd
.importGMX <- function(file, quiet) {
    assert(isFlag(quiet))
    if (!isTRUE(quiet)) {
        where <- ifelse(
            test = isAURL(file),
            yes = dirname(file),
            no = realpath(dirname(file))
        )
        alert(sprintf(
            "Importing {.file %s} at {.path %s}.",
            basename(file), where
        ))
    }
    lines <- .importLines(file = file, quiet = quiet)
    pathways <- list(tail(lines, n = -2L))
    names(pathways) <- lines[[1L]]
    pathways
}



#' Internal importer for a gene set file (`.grp`)
#'
#' @note Updated 2021-01-13.
#' @noRd
.importGRP <- .importGMX



## Microsoft Excel =============================================================
## FIXME This should always import colnames, no?

## Note that `readxl::read_excel()` doesn't currently support automatic blank
## lines removal, so ensure that is fixed downstream.

#' Internal importer for a Microsoft Excel worksheet (`.xlsx`)
#'
#' @note Updated 2021-01-13.
#' @noRd
.importExcel <- function(
    file,
    rownames = TRUE,  # FIXME Need to add back support for this.
    colnames = TRUE,
    metadata,
    nMax,
    quiet,
    sheet = 1L,
    skip
) {
    requireNamespaces("readxl")
    assert(
        isFlag(colnames) || isCharacter(colnames),
        isFlag(metadata),
        isPositive(nMax),
        isFlag(quiet),
        isScalar(sheet),
        isInt(skip), isNonNegative(skip)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        where <- ifelse(
            test = isAURL(file),
            yes = dirname(file),
            no = realpath(dirname(file))
        )
        alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), where,
            "readxl", "read_excel"
        ))
    }
    ## Note that `tryCatch()` or `withCallingHandlers()` doesn't work here.
    ## http://adv-r.had.co.nz/Exceptions-Debugging.html
    warn <- getOption("warn")
    options(warn = 2L)
    object <- readxl::read_excel(
        path = tmpfile,
        col_names = colnames,
        n_max = nMax,
        na = naStrings,
        progress = FALSE,
        sheet = sheet,
        skip = skip,
        trim_ws = TRUE,
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
#' Internal importer for a GraphPad Prism file (`.pzfx`)
#'
#' @note Updated 2020-08-13.
#' @noRd
#'
#' @note This function doesn't support optional column names.
.importPZFX <- function(
    file,
    sheet = 1L,
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
        where <- ifelse(
            test = isAURL(file),
            yes = dirname(file),
            no = realpath(dirname(file))
        )
        alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), where,
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
#' Import bcbio featureCounts file
#'
#' @details
#' Internal importer for a bcbio count matrix file (`.counts`).
#' These files contain an `"id"` column that we need to coerce to row names.
#'
#' @note Updated 2020-12-18.
#' @noRd
.importBcbioCounts <- function(file, metadata, quiet) {
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    object <- import(
        file = file,
        format = "tsv",
        metadata = metadata,
        quiet = quiet
    )
    if (isTRUE(metadata)) {
        m <- metadata2(object, which = "import")
        assert(
            is.list(m),
            hasLength(m)
        )
    }
    assert(
        is.data.frame(object),
        isSubset("id", colnames(object)),
        hasNoDuplicates(object[["id"]])
    )
    rownames(object) <- object[["id"]]
    object[["id"]] <- NULL
    object <- as.matrix(object)
    mode(object) <- "integer"
    if (isTRUE(metadata)) {
        metadata2(object, which = "import") <- m
    }
    object
}



## Handoff =====================================================================




#' Handoff to rtracklayer import
#'
#' @note Updated 2020-08-13.
#' @noRd
#'
#' @note Using `tryCatch()` here to error if there are any warnings.
.rtracklayerImport <- function(file, metadata, quiet, ...) {
    requireNamespaces("rtracklayer")
    assert(
        isFlag(metadata),
        isFlag(quiet)
    )
    tmpfile <- localOrRemoteFile(file = file, quiet = quiet)
    if (!isTRUE(quiet)) {
        where <- ifelse(
            test = isAURL(file),
            yes = dirname(file),
            no = realpath(dirname(file))
        )
        alert(sprintf(
            "Importing {.file %s} at {.path %s} using {.pkg %s}::{.fun %s}.",
            basename(file), where,
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



## S4 method exports ===========================================================
#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature("character"),
    definition = `import,character`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature("DelimFile"),
    definition = `import,DelimFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature("LinesFile"),
    definition = `import,LinesFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature("RDSFile"),
    definition = `import,RDSFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature("RDataFile"),
    definition = `import,RDataFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature("JSONFile"),
    definition = `import,JSONFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature("MTXFile"),
    definition = `import,MTXFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature("YAMLFile"),
    definition = `import,YAMLFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature("RioFile"),
    definition = `import,RioFile`
)
