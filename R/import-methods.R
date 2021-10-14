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
#' `tibble::rownames_to_column()`. To disable this behavior, set `rownames =
#' FALSE`, and no attempt will be made to set the row names.
#'
#' **Column names.** `import()` assumes that delimited files always contain
#' column names. If you are working with a file that doesn't contain column
#' names, either set `colnames = FALSE` or pass the names in as a `character`
#' vector. It's strongly recommended to always define column names in a
#' supported file type.
#'
#' @section FASTA and FASTQ files (`FASTAFile`, `FASTQFile`):
#'
#' FASTA and FASTQ files are currently managed internally by the Biostrings
#' package. Refer to `readDNAStringSet` and `readRNAStringSet` for details.
#' Import of these files will return `DNAStringSet` or `RNAStringSet` depending
#' on the input, defined by `moleculeType` argument.
#'
#' @section General feature format (GFF, GTF; `RtracklayerHandoffFile`):
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
#' @section Matrix Market Exchange (`MTXFile`):
#'
#' Reading a Matrix Market Exchange file requires `ROWNAMES` and `COLNAMES`
#' sidecar files containing the corresponding row and column names of the sparse
#' matrix.
#'
#' @section bcbio-nextgen count matrix (`BcbioCountsFile`):
#'
#' [bcbio][] count matrix (e.g. generated from featureCounts) and related
#' sidecar files are natively supported.
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
#' @note Updated 2021-10-14.
#'
#' @inheritParams AcidRoxygen::params
#' @param con `character(1)`, `connection`, or `missing`.
#'   The connection from which data is loaded or to which data is saved. If this
#'   is a character vector, it is assumed to be a filename, and a corresponding
#'   file connection is created and then closed after exporting the object. If a
#'   `BiocFile` derivative, the data is loaded from or saved to the underlying
#'   resource.  If missing, the function will return the output as a character
#'   vector, rather than writing to a connection.
#' @param text `character` or `missing`.
#'   If `con` is missing, this can be a character vector directly providing the
#'   string data to import.
#' @param format `character(1)` or `missing`.
#'   An optional file format type, which can be used to override the file format
#'   inferred from `con`. Only recommended for file and URL paths that don't
#'   contain an extension.
#' @param file `character(1)` or `missing`.
#'   Deprecated in favor of primary `con` argument, defined in BiocIO
#'   package. This argument likely will be removed in a future update.
#' @param rownameCol `NULL`, `character(1)`, or `integer(1)`.
#'   *Applies only when `rownames = TRUE`.*
#'   Column name to use for row names assignment.
#'   If left `NULL` (default), the function will call `matchRownameCol()`
#'   internally to attempt to automatically match the row name column (e.g.
#'   `"rowname"` or `"rn"`).
#'   Otherwise, can manually define using a scalar argument, either the name
#'   directly or position in the column names.
#' @param colnames `logical(1)` or `character`.
#'   Automatically assign column names, using the first header row.
#'   Applies to file types that return `data.frame` only.
#'   Pass in a `character` vector to define the column names manually.
#' @param comment `character(1)`.
#'   Comment character to detect at beginning of line, which will skip when
#'   parsing file. Use `""` to disable interpretation of comments, which is
#'   particularly
#'   useful when parsing lines.
#'   *Applies to plain text delimited and source code lines only.*
#' @param engine `character(1)`.
#'   Engine (package) to use for import.
#'   Currently supported:
#'   - base
#'   - data.table
#'   - readr
#'   - vroom
#' @param makeNames `function`.
#'   Apply syntactic naming function to (column) names.
#'   Function is never applied to row names, when they are defined in object.
#' @param moleculeType `character(1)`.
#'   Molecule type, either DNA or RNA.
#'   Most RNA-seq FASTQ files contain complementary DNA (cDNA) sequences, not
#'   direct sequencing of the RNA molecules.
#' @param nMax `integer(1)` or `Inf`.
#'   Maximum number of lines to parse.
#'   *Applies to plain text delimited, Excel, and source code lines only.*
#' @param removeBlank `logical(1)`.
#'   Remove blank lines.
#'   *Applies to source code lines*.
#' @param rownames `logical(1)`.
#'   Automatically assign row names, if `rowname` column is defined.
#'   Applies to file types that return a data frame only.
#' @param rownamesFile,colnamesFile `character(1)` or `NULL`.
#'   Row names and/or column names sidecare file.
#'   Applies primarily to MatrixMarket Exchange files (e.g. `MTXFile`).
#' @param sheet `character(1)` or `integer(1)`.
#'   Sheet to read. Either a string (the name of a sheet), or an integer (the
#'   position of the sheet). Defaults to the first sheet.
#'   *Applies to Excel Workbook, Google Sheet, or GraphPad Prism file.*
#' @param skip `integer(1)`.
#'   Number of lines to skip.
#'   *Applies to delimited file (CSV, TSV), Excel Workbook, or lines.*
#' @param stripWhitespace `logical(1)`.
#'   Strip leading and/or trailing whitespace.
#'   *Applies to source code lines*.
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
#' - `BiocIO::import()`.
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

#' Inform the user about start of file import
#'
#' @note Updated 2021-09-24.
#' @noRd
.alertImport <- function(
    con,
    whatPkg,
    whatFun
) {
    assert(
        is(con, "PipetteFile"),
        isString(whatPkg),
        isString(whatFun)
    )
    file <- attr(x = con, which = "origResource")
    if (is.null(file)) {
        file <- resource(con)
    }
    alert(sprintf(
        "Importing {.%s %s} using {.pkg %s}::{.fun %s}.",
        ifelse(
            test = isAURL(file),
            yes = "url",
            no = "file"
        ),
        file,
        whatPkg, whatFun
    ))
    invisible(TRUE)
}



#' Auto-decompress a file, if necessary
#'
#' @note Updated 2021-09-24.
#' @noRd
#'
#' @details
#' Note that `data.table::fread()` still doesn't natively support compressed
#' files. R on Windows can run into `tempdir()` write permission issues, unless
#' R is running as administrator. Ensure that decompressed file is removed
#' manually before attempting to overwrite, otherwise this step can error out.
#'
#' Alternatively, can check for BiocIO "CompressedFile" class.
.autoDecompress <- function(file) {
    file <- realpath(file)
    out <- vapply(
        X = file,
        FUN = function(file) {
            if (!grepl(compressExtPattern, file)) {
                return(file)
            }
            tmpdir <- realpath(tempdir())
            pattern <- paste0(.pkgName, "-")
            if (isFALSE(grepl(
                pattern = file.path(tmpdir, pattern),
                x = file
            ))) {
                tmpfile <- tempfile(
                    pattern = pattern,
                    tmpdir = tmpdir,
                    fileext = paste0(".", fileExt(file))
                )
                file.copy(from = file, to = tmpfile)
                file <- tmpfile
            }
            destfile <- decompress(
                file = file,
                remove = TRUE,
                overwrite = TRUE
            )
            assert(isString(destfile))
            destfile
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    out <- realpath(out)
    out
}



#' Map file extension to corresponding S4 file class
#'
#' @note Updated 2021-08-24.
#' @noRd
.extToFileClass <- function(ext) {
    ext <- tolower(ext)
    if (identical(ext, "txt")) {
        ## nocov start
        abort(sprintf(
            fmt = paste(
                "Automatic import of {.var %s} file is not supported.",
                "Specify using {.arg %s} (e.g. {.var %s}, {.var %s}).",
                sep = "\n"
            ),
            "txt",
            "format",
            "lines", "table"
        ))
        ## nocov end
    }
    dict <- list(
        "arff"         = "RioHandoffFile",
        "bash"         = "LinesFile",
        "bcbio-counts" = "BcbioCountsFile",
        "bed"          = "RtracklayerHandoffFile",
        "bed15"        = "RtracklayerHandoffFile",
        "bedgraph"     = "RtracklayerHandoffFile",
        "bedpe"        = "RtracklayerHandoffFile",
        "bigwig"       = "RtracklayerHandoffFile",
        "broadpeak"    = "RtracklayerHandoffFile",
        "bw"           = "RtracklayerHandoffFile",
        "counts"       = "BcbioCountsFile",
        "csv"          = "CSVFile",
        "dbf"          = "RioHandoffFile",
        "dif"          = "RioHandoffFile",
        "dta"          = "RioHandoffFile",
        "excel"        = "ExcelFile",
        "fa"           = "FASTAFile",
        "fasta"        = "FASTAFile",
        "fastq"        = "FASTQFile",
        "fq"           = "FASTQFile",
        "fwf"          = "RioHandoffFile",
        "gff"          = "RtracklayerHandoffFile",
        "gff1"         = "RtracklayerHandoffFile",
        "gff2"         = "RtracklayerHandoffFile",
        "gff3"         = "RtracklayerHandoffFile",
        "gmt"          = "GMTFile",
        "gmx"          = "GMXFile",
        "grp"          = "GRPFile",
        "gsheet"       = "RioHandoffFile",  # Google Sheets
        "gtf"          = "RtracklayerHandoffFile",
        "json"         = "JSONFile",
        "lines"        = "LinesFile",
        "log"          = "LinesFile",
        "mat"          = "RioHandoffFile",
        "md"           = "LinesFile",
        "mtp"          = "RioHandoffFile",
        "mtx"          = "MTXFile",
        "narrowpeak"   = "RtracklayerHandoffFile",
        "ods"          = "RioHandoffFile",
        "por"          = "RioHandoffFile",
        "psv"          = "RioHandoffFile",
        "py"           = "LinesFile",
        "pzfx"         = "PZFXFile",
        "r"            = "LinesFile",
        "rda"          = "RDataFile",
        "rdata"        = "RDataFile",
        "rds"          = "RDSFile",
        "rec"          = "RioHandoffFile",
        "rio"          = "RioHandoffFile",
        "rmd"          = "LinesFile",
        "rtracklayer"  = "RtracklayerHandoffFile",
        "sas7bdat"     = "RioHandoffFile",
        "sav"          = "RioHandoffFile",
        "sh"           = "LinesFile",
        "syd"          = "RioHandoffFile",
        "table"        = "TableFile",
        "tsv"          = "TSVFile",
        "wig"          = "RtracklayerHandoffFile",
        "xls"          = "ExcelFile",
        "xlsb"         = "ExcelFile",
        "xlsx"         = "ExcelFile",
        "xpt"          = "RioHandoffFile",
        "yaml"         = "YAMLFile",
        "yml"          = "YAMLFile",
        "zsh"          = "LinesFile"
    )
    if (!isSubset(ext, names(dict))) {
        abort(sprintf("{.var %s} extension is not supported.", ext))
    }
    class <- dict[[ext]]
    assert(isString(class))
    class
}



#' Get function
#'
#' @note Updated 2021-08-24.
#' @noRd
#'
#' @param f `character(1)`.
#'   Function name.
#' @param pkg `character(1)`.
#'   Package name.
#'
#' @return `function`.
.getFunction <- function(f, pkg) {
    assert(
        isString(f),
        isString(pkg)
    )
    requireNamespaces(pkg)
    x <- get(x = f, envir = asNamespace(pkg), inherits = TRUE)
    assert(is.function(x))
    x
}



#' Internal importer for a sparse matrix sidecar file (e.g. `.rownames`)
#'
#' @note Updated 2021-09-25.
#' @noRd
.importMTXSidecar <- function(file, quiet) {
    assert(
        isString(file),
        isFlag(quiet)
    )
    if (isFALSE(quiet)) {
        alert(sprintf("Importing sidecar {.file %s}.", file))
    }
    object <- import(
        con = as.character(file),
        format = "lines",
        quiet = quiet
    )
    object
}



#' Dynamically handle a local or remote file path
#'
#' @section Vectorization:
#'
#' This function is vectorized and supports mixed local and remote paths. Remote
#' files are downloaded locally to a temporary directory.
#'
#' @section Compressed files:
#'
#' Compressed files will automatically be decompressed. Currently, these file
#' extensions are natively supported: `BZ2`, `GZ`, `XZ`, and `ZIP`.
#'
#' @note Updated 2021-09-24.
#' @noRd
#'
#' @inheritParams AcidRoxygen::params
#' @param file `character(1)`.
#'   Local file paths or remote URLs.
#' @param tempPrefix `character(1)`.
#'   Prefix to use for temporary file basename.
#'
#' @return `character`.
#' Local file path(s). Stops on a missing file.
#'
#' @seealso
#' - `tempfile()`.
#' - `tempdir()`.
#'
#' @examples
#' ## Local
#' file <- system.file("extdata/example.csv", package = "pipette")
#' x <- localOrRemoteFile(file)
#' basename(x)
#'
#' ## Remote
#' file <- AcidBase::pasteURL(
#'     pipetteTestsURL,
#'     "hgnc.txt.gz",
#'     protocol = "none"
#' )
#' x <- .localOrRemoteFile(file)
#' basename(x)
.localOrRemoteFile <- function(
    file,
    tempPrefix = .pkgName,
    quiet = getOption(
        x = "acid.quiet",
        default = FALSE
    )
) {
    assert(
        isCharacter(file),
        isString(tempPrefix),
        isFlag(quiet)
    )
    file <- mapply(
        file = file,
        FUN = function(file) {
            if (isFALSE(isAURL(file))) {
                return(file)
            }
            ## Remote file mode.
            assert(hasInternet())
            ## Note that for `.gtf.gz` we want to return only `.gz` here.
            ## This behavor differs from matching using `extPattern` global.
            ext <- str_match(
                string = basename(file),
                pattern = "\\.([a-zA-Z0-9]+)$"
            )
            ext <- na.omit(ext[1L, 2L])
            ## Write mode for binary files.
            ## Note that files without extension will use default.
            ## https://github.com/tidyverse/readxl/issues/374
            binary <- c(
                "bz2",
                "gz",
                "rda",
                "rds",
                "xls",
                "xlsx",
                "xz",
                "zip"
            )
            if (isSubset(ext, binary)) {
                ## Write binary.
                mode <- "wb"
            } else {
                ## Write (default).
                mode <- "w"
            }
            tmpdir <- realpath(tempdir())
            fileext <- fileExt(file)
            if (is.na(fileext)) {
                fileext <- ""  # nocov
            } else {
                fileext <- paste0(".", fileext)
            }
            destfile <- tempfile(
                pattern = paste0(tempPrefix, "-"),
                tmpdir = tmpdir,
                fileext = fileext
            )
            download(
                url = file,
                destfile = destfile,
                quiet = quiet,
                mode = mode
            )
            destfile
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    realpath(.autoDecompress(file))
}



#' Return standardized import object
#'
#' @note Updated 2021-09-24.
#' @noRd
.returnImport <- function(
    object,
    con,
    rownames = FALSE,
    rownameCol = NULL,
    colnames = FALSE,
    makeNames = FALSE,
    metadata = FALSE,
    whatPkg = NULL,
    whatFun = NULL,
    quiet = getOption(
        x = "acid.quiet",
        default = FALSE
    )
) {
    validObject(object)
    assert(
        is(con, "PipetteFile"),
        isFlag(rownames),
        isScalar(rownameCol) || is.null(rownameCol),
        isFlag(colnames) || isCharacter(colnames),
        is.function(makeNames) ||
            is.null(makeNames) ||
            isFALSE(makeNames),
        isFlag(metadata),
        isString(whatPkg, nullOK = TRUE),
        isString(whatFun, nullOK = TRUE),
        isFlag(quiet)
    )
    file <- attr(x = con, which = "origResource")
    if (is.null(file)) {
        file <- resource(con)
    }
    if (!is.null(rownameCol)) {
        rownames <- TRUE
    }
    ## Check that manual column names are correct.
    if (isCharacter(colnames)) {
        assert(identical(colnames(object), colnames))  # nocov
    }
    ## Attempt to set row names automatically for data frames, when applicable.
    if (
        is.data.frame(object) &&
        isTRUE(rownames) &&
        !hasRownames(object)
    ) {
        if (is.null(rownameCol)) {
            rownameCol <- matchRownameColumn(object)
        }
        if (!is.null(rownameCol)) {
            assert(isScalar(rownameCol))
            if (!isString(rownameCol)) {
                rownameCol <- colnames(object)[[rownameCol]]
            }
            assert(
                isString(rownameCol),
                isSubset(rownameCol, colnames(object))
            )
            if (isFALSE(quiet)) {
                alertInfo(sprintf(
                    "Setting row names from {.var %s} column.",
                    rownameCol
                ))
            }
            rownames(object) <- object[[rownameCol]]
            object[[rownameCol]] <- NULL
        }
    }
    if (hasRownames(object)) {
        assert(hasNoDuplicates(rownames(object)))
    }
    if (hasNames(object)) {
        if (isTRUE(any(duplicated(names(object))))) {
            ## nocov start
            dupes <- sort(names(object)[duplicated(names(object))])
            alertWarning(sprintf(
                "Duplicate names: {.var %s}.",
                toInlineString(dupes, n = 5L)
            ))
            ## nocov end
        }
        ## Ensure names are syntactically valid, when applicable.
        if (is.function(makeNames)) {
            ## Harden against any object classes that don't support names
            ## assignment, to prevent unwanted error on this step.
            tryCatch(
                expr = {
                    names(object) <- makeNames(names(object))
                },
                error = function(e) NULL
            )
            if (isFALSE(hasValidNames(object))) {
                alertWarning("Invalid names detected.")  # nocov
            }
        }
        assert(hasNoDuplicates(names(object)))
    }
    if (isTRUE(metadata)) {
        metadata2(object, which = "import") <-
            SimpleList(
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



## Primary S4 methods ==========================================================

#' Primary `import` method, that hands off to classed file-extension variants
#'
#' @details
#' We're supporting remote files, so don't check using `isAFile()` here.
#'
#' Allow Google Sheets import using rio, by matching the URL.
#' Otherwise, coerce the file extension to uppercase, for easy matching.
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,character,con` <-  # nolint
    function(
        con,
        format,
        text,  # NULL
        ...
    ) {
        if (
            missing(format) ||
            identical(format, "auto") ||
            identical(format, "none")
        ) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            isString(format, nullOK = TRUE),
            is.null(text)
        )
        if (grepl(
            pattern = "^https://docs\\.google\\.com/spreadsheets",
            x = con
        )) {
            format <- "gsheet"
        }
        if (is.null(format)) {
            format <- str_match(
                string = basename(con),
                pattern = extPattern
            )[1L, 2L]
            if (is.na(format)) {
                abort(sprintf(
                    fmt = paste(
                        "{.arg %s} ({.file %s}) doesn't contain extension.",
                        "Set the file format manually using {.arg %s}.",
                        "Refer to {.pkg %s}::{.fun %s} for details.",
                        sep = "\n"
                    ),
                    "con", basename(con),
                    "format",
                    "pipette", "import"
                ))
            }
        }
        assert(isString(format))
        class <- .extToFileClass(format)
        assert(
            hasMethod(
                f = "import",
                signature = signature(
                    con = class,
                    format = "missingOrNULL",
                    text = "missingOrNULL"
                ),
                where = asNamespace(.pkgName)
            )
        )
        switch(
            EXPR = format,
            "gsheet" = {
                con <- new(Class = class, resource = con)
            },
            {
                origResource <- con
                if (isAFile(origResource)) {
                    origResource <- realpath(origResource)
                }
                resource <- .localOrRemoteFile(con)
                con <- new(Class = class, resource = resource)
                attr(con, which = "origResource") <- origResource
            }
        )
        validObject(con)
        assert(
            is(con, "PipetteFile"),
            is(con, "BiocFile")
        )
        import(
            con = con,
            format = NULL,
            text = NULL,
            ...
        )
    }



#' Soft deprecated legacy handler for primary "file" argument
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,character,file` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        file,  # deprecated
        ...
    ) {
        assert(isString(file))
        con <- file
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        import(
            con = con,
            format = format,
            text = text,
            ...
        )
    }



## R data importers ============================================================

#' Import an R data file containing multiple objects (`.rda`)
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,RDataFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "base"
        whatFun <- "load"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        saveEnv <- new.env()
        args <- list(
            "file" = file,
            "envir" = saveEnv
        )
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        if (isFALSE(hasLength(saveEnv, n = 1L))) {
            abort(sprintf(
                "{.file %s} does not contain a single object.",
                basename(file)
            ))
        }
        object <- get(object, envir = saveEnv, inherits = FALSE)
        if (isFALSE(quiet)) {
            tryCatch(
                expr = {
                    validObject(object)
                },
                error = function(e) {
                    conditionMessage(e)  # nocov
                }
            )
        }
        object
    }



#' Import an R data serialized file (`.rds`)
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,RDSFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "base"
        whatFun <- "readRDS"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list("file" = file)
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        if (isFALSE(quiet)) {
            tryCatch(
                expr = {
                    validObject(object)
                },
                error = function(e) {
                    conditionMessage(e)  # nocov
                }
            )
        }
        object
    }



## Array importers =============================================================

#' Import a delimited file (e.g. `.csv`, `.tsv`).
#'
#' @details
#' Calls `data.table::fread()` internally by default.
#' Can override using `acid.import.engine` option, which also supports
#' data.table and readr packages.
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,DelimFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        rownames = TRUE,
        rownameCol = NULL,
        colnames = TRUE,
        comment = "",
        skip = 0L,
        nMax = Inf,
        engine = getOption(
            x = "acid.import.engine",
            default = "data.table"
        ),
        makeNames = getOption(
            x = "acid.import.make.names",
            default = syntactic::makeNames
        ),
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(rownames),
            isScalar(rownameCol) || is.null(rownameCol),
            isFlag(colnames) || isCharacter(colnames),
            is.character(comment) && length(comment) <= 1L,
            isInt(skip), isNonNegative(skip),
            isPositive(nMax),
            isString(engine),
            is.function(makeNames) ||
                is.null(makeNames) ||
                isFALSE(makeNames),
            isFlag(metadata),
            isFlag(quiet)
        )
        file <- resource(con)
        ext <- switch(
            EXPR = class(con),
            "CSVFile" = "csv",
            "TSVFile" = "tsv",
            "TableFile" = "table",
            abort("Unsupported delim class.")
        )
        whatPkg <- match.arg(arg = engine, choices = .engines)
        if (identical(ext, "table")) {
            whatPkg <- "base"  # nocov
        }
        switch(
            EXPR = whatPkg,
            "base" = {
                whatFun <- "read.table"
                args <- list(
                    "file" = file,
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
                    ## nocov start
                    args[["header"]] <- FALSE
                    args[["col.names"]] <- colnames
                    ## nocov end
                } else {
                    args[["header"]] <- colnames
                }
            },
            "data.table" = {
                whatFun <- "fread"
                if (isString(comment)) {
                    ## nocov start
                    abort(sprintf(
                        paste0(
                            "{.pkg %s}::{.fun %s} does not support ",
                            "comment exclusion.\n",
                            "See also: {.url %s}."
                        ),
                        whatPkg, whatFun,
                        "https://github.com/Rdatatable/data.table/issues/856"
                    ))
                    ## nocov end
                }
                args <- list(
                    "file" = file,
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
                    "verbose" = FALSE
                )
                if (isCharacter(colnames)) {
                    ## nocov start
                    args[["header"]] <- FALSE
                    args[["col.names"]] <- colnames
                    ## nocov end
                } else {
                    args[["header"]] <- colnames
                }
            },
            "readr" = {
                whatFun <- "read_delim"
                args <- list(
                    "file" = file,
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
                    "skip" = skip,
                    "skip_empty_rows" = TRUE,
                    "trim_ws" = TRUE
                )
            },
            "vroom" = {
                whatFun <- "vroom"
                args <- list(
                    "file" = file,
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
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        assert(is.data.frame(object))
        if (!identical(class(object), "data.frame")) {
            object <- as.data.frame(
                x = object,
                make.names = FALSE,
                stringsAsFactors = FALSE
            )
        }
        if (
            identical(engine, "data.table") &&
            isTRUE(any(object == "", na.rm = TRUE))
        ) {
            object <- sanitizeNA(object)  # nocov
        }
        assert(
            allAreAtomic(object),
            isFALSE(any(object == "", na.rm = TRUE))
        )
        .returnImport(
            object = object,
            con = con,
            rownames = rownames,
            rownameCol = rownameCol,
            colnames = colnames,
            makeNames = makeNames,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



#' Import a Microsoft Excel worksheet (`.xlsx`)
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,ExcelFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        sheet = 1L,
        rownames = TRUE,
        rownameCol = NULL,
        colnames = TRUE,
        skip = 0L,
        nMax = Inf,
        makeNames = getOption(
            x = "acid.import.make.names",
            default = syntactic::makeNames
        ),
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isScalar(sheet),
            isFlag(rownames) || isCharacter(rownames),
            isScalar(rownameCol) || is.null(rownameCol),
            isFlag(colnames) || isCharacter(colnames),
            isInt(skip), isNonNegative(skip),
            isPositive(nMax),
            is.function(makeNames) ||
                is.null(makeNames) ||
                isFALSE(makeNames),
            isFlag(metadata),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "readxl"
        whatFun <- "read_excel"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        warn <- getOption(x = "warn")
        args <- list(
            "path" = file,
            "col_names" = colnames,
            "n_max" = nMax,
            "na" = naStrings,
            "progress" = FALSE,
            "sheet" = sheet,
            "skip" = skip,
            "trim_ws" = TRUE,
            ".name_repair" = make.names
        )
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        options(warn = 2L)
        object <- do.call(what = what, args = args)
        options(warn = warn)
        object <- as.data.frame(
            x = object,
            make.names = FALSE,
            stringsAsFactors = FALSE
        )
        object <- removeNA(object)
        .returnImport(
            object = object,
            con = con,
            rownames = rownames,
            rownameCol = rownameCol,
            colnames = colnames,
            makeNames = makeNames,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



#' Import a sparse matrix file (`.mtx`)
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,MTXFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        rownamesFile,
        colnamesFile,
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        file <- resource(con)
        origFile <- attr(con, which = "origResource")
        if (is.null(origFile)) {
            origFile <- file
        }
        if (missing(rownamesFile)) {
            rownamesFile <- paste0(origFile, ".rownames")
        }
        if (missing(colnamesFile)) {
            colnamesFile <- paste0(origFile, ".colnames")
        }
        assert(
            is.null(format),
            is.null(text),
            isString(rownamesFile, nullOK = TRUE),
            isString(colnamesFile, nullOK = TRUE),
            isFlag(metadata),
            isFlag(quiet)
        )
        whatPkg <- "Matrix"
        whatFun <- "readMM"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list("file" = file)
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        ## Add the rownames automatically using `.rownames` sidecar file.
        rownamesFile <- tryCatch(
            expr = .localOrRemoteFile(
                file = rownamesFile,
                quiet = quiet
            ),
            error = function(e) {
                NULL  # nocov
            }
        )
        if (isAFile(rownamesFile)) {
            rownames(object) <-
                .importMTXSidecar(file = rownamesFile, quiet = quiet)
        }
        ## Add the colnames automatically using `.colnames` sidecar file.
        colnamesFile <- tryCatch(
            expr = .localOrRemoteFile(
                file = colnamesFile,
                quiet = quiet
            ),
            error = function(e) {
                NULL  # nocov
            }
        )
        if (isAFile(colnamesFile)) {
            colnames(object) <-
                .importMTXSidecar(file = colnamesFile, quiet = quiet)
        }
        .returnImport(
            object = object,
            con = con,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



#' Import a GraphPad Prism file (`.pzfx`)
#'
#' @note Updated 2021-10-12.
#' @noRd
#'
#' @note This function doesn't support optional column names.
`import,PZFXFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        sheet = 1L,
        makeNames = getOption(
            x = "acid.import.make.names",
            default = syntactic::makeNames
        ),
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isScalar(sheet),
            is.function(makeNames) ||
                is.null(makeNames) ||
                isFALSE(makeNames),
            isFlag(metadata),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "pzfx"
        whatFun <- "read_pzfx"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list(
            "path" = file,
            "table" = sheet
        )
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        object <- removeNA(object)
        .returnImport(
            object = object,
            con = con,
            rownames = FALSE,
            colnames = TRUE,
            makeNames = makeNames,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



#' Import bcbio count matrix generated by featureCounts
#'
#' @details
#' Internal importer for a bcbio count matrix file (`.counts`).
#' These files contain an `"id"` column that we need to coerce to row names.
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,BcbioCountsFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(metadata),
            isFlag(quiet)
        )
        file <- resource(con)
        object <- import(
            file = file,
            format = "tsv",
            rownames = FALSE,
            colnames = TRUE,
            makeNames = FALSE,
            metadata = metadata,
            quiet = quiet
        )
        assert(
            is.data.frame(object),
            isSubset("id", colnames(object)),
            hasNoDuplicates(object[["id"]])
        )
        if (isTRUE(metadata)) {
            m <- metadata2(object, which = "import")
        }
        rownames(object) <- object[["id"]]
        object[["id"]] <- NULL
        ## Don't attempt to coerce `"annotated_combined.counts"` file to matrix.
        if (isSubset("symbol", colnames(object))) {
            if (isFALSE(quiet)) {
                alertInfo("Annotated counts detected.")
            }
            object <- object[
                ,
                c("symbol", setdiff(colnames(object), "symbol")),
                drop = FALSE
            ]
        } else {
            object <- as.matrix(object)
            mode(object) <- "integer"
        }
        if (isTRUE(metadata)) {
            metadata2(object, which = "import") <- m
        }
        object
    }



## Non-array importers =========================================================

#' Import source code lines
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,LinesFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        comment = "",
        skip = 0L,
        nMax = Inf,
        stripWhitespace = FALSE,
        removeBlank = FALSE,
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        engine = getOption(
            x = "acid.import.engine",
            default = "base"
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            is.character(comment) && length(comment) <= 1L,
            isInt(skip),
            isInt(skip), isNonNegative(skip),
            isPositive(nMax),
            isFlag(stripWhitespace),
            isFlag(removeBlank),
            isFlag(quiet)
        )
        if (isString(comment) || isTRUE(removeBlank)) {
            assert(
                identical(nMax, eval(formals()[["nMax"]])),
                identical(skip, eval(formals()[["skip"]])),
                msg = sprintf(
                    fmt = paste0(
                        "'%s' or '%s' arguments are not supported when ",
                        "either '%s' or '%s' are enabled."
                    ),
                    "nMax", "skip",
                    "comment", "removeBlank"
                )
            )
        }
        file <- resource(con)
        whatPkg <- match.arg(arg = engine, choices = .engines)
        switch(
            EXPR = whatPkg,
            "base" = {
                whatFun <- "readLines"
                args <- list(
                    "con" = file,
                    "warn" = FALSE
                )
            },
            "data.table" = {
                whatFun <- "fread"
                args <- list(
                    "file" = file,
                    "blank.lines.skip" = removeBlank,
                    "fill" = FALSE,
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
                    "file" = file,
                    "n_max" = nMax,
                    "progress" = FALSE,
                    "skip" = skip,
                    "skip_empty_rows" = removeBlank
                )
            },
            "vroom" = {
                whatFun <- "vroom_lines"
                args <- list(
                    "file" = file,
                    "n_max" = nMax,
                    "progress" = FALSE,
                    "skip" = skip
                )
            }
        )
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        if (isTRUE(file.size(file) == 0L)) {
            return(character())
        }
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        if (identical(whatPkg, "data.table")) {
            object <- object[[1L]]
        }
        assert(is.character(object))
        if (isTRUE(stripWhitespace)) {
            object <- gsub(
                pattern = "^[[:space:]]+",
                replacement = "",
                x = object
            )
            object <- gsub(
                pattern = "[[:space:]]+$",
                replacement = "",
                x = object
            )
        }
        if (isTRUE(removeBlank)) {
            requireNamespaces("stringi")
            object <- stringi::stri_remove_empty(object)
        }
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
            con = con,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



#' Import a JSON file (`.json`)
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,JSONFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(metadata),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "jsonlite"
        whatFun <- "read_json"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list("path" = file)
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        .returnImport(
            object = object,
            con = con,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



#' Import a YAML file (`.yaml`, `.yml`)
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,YAMLFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(metadata),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "yaml"
        whatFun <- "yaml.load_file"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list("input" = file)
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        .returnImport(
            object = object,
            con = con,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



## Bioinformatics importers ====================================================

#' Import a FASTA file
#'
#' @note Updated 2021-09-24.
#' @noRd
#'
#' @seealso
#' - `Biostrings::readDNAStringSet()`.
#'
#' @return Varies, depending on the `moleculeType` argument:
#' - `"DNA"`: `DNAStringSet`.
#' - `"RNA"`: `RNAStringSet`.
`import,FASTAFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        moleculeType = c("DNA", "RNA"),
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(metadata),
            isFlag(quiet)
        )
        moleculeType <- match.arg(moleculeType)
        file <- resource(con)
        whatPkg <- "Biostrings"
        whatFun <- paste0("read", moleculeType, "StringSet")
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list(
            "filepath" = file,
            "format" = "fasta",
            "nrec" = -1L,
            "skip" = 0L,
            "seek.first.rec" = TRUE,
            "use.names" = metadata
        )
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        assert(is(object, paste0(moleculeType, "StringSet")))
        if (
            hasNames(object) &&
            any(grepl(
                pattern = "|",
                x = head(names(object)),
                fixed = TRUE
            ))
        ) {
            attributes <- strsplit(
                x = names(object),
                split = "|",
                fixed = TRUE
            )
            attributes <- SimpleList(attributes)
            names <- vapply(
                X = attributes,
                FUN = `[[`,
                1L,
                FUN.VALUE = character(1L),
                USE.NAMES = FALSE
            )
            names(object) <- names
            names(attributes) <- names
            metadata(object)[["attributes"]] <- attributes
        }
        .returnImport(
            object = object,
            con = con,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



#' Import a FASTQ file
#'
#' @note Updated 2021-09-24.
#' @noRd
#'
#' @seealso
#' - `Biostrings::readDNAStringSet()`.
#'
#' @return Varies, depending on the `moleculeType` argument:
#' - `"DNA"`: `DNAStringSet`.
#' - `"RNA"`: `RNAStringSet`.
`import,FASTQFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        moleculeType = c("DNA", "RNA"),
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(metadata),
            isFlag(quiet)
        )
        moleculeType <- match.arg(moleculeType)
        file <- resource(con)
        whatPkg <- "Biostrings"
        whatFun <- paste0("read", moleculeType, "StringSet")
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list(
            "filepath" = file,
            "format" = "fastq",
            "nrec" = -1L,
            "skip" = 0L,
            "seek.first.rec" = TRUE,
            "use.names" = metadata,
            "with.qualities" = metadata
        )
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        assert(is(object, paste0(moleculeType, "StringSet")))
        .returnImport(
            object = object,
            con = con,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



#' Import a gene matrix transposed file (`.gmt`)
#'
#' @note Updated 2021-10-12.
#' @noRd
#'
#' @seealso `fgsea::gmtPathways()`.
`import,GMTFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        lines <- import(
            file = resource(con),
            format = "lines",
            metadata = FALSE,
            quiet = quiet
        )
        lines <- strsplit(lines, split = "\t")
        object <- lapply(lines, tail, n = -2L)
        names(object) <- vapply(
            X = lines,
            FUN = head,
            FUN.VALUE = character(1L),
            n = 1L
        )
        object
    }



#' Import a gene matrix file (`.gmx`)
#'
#' @note Updated 2021-10-12.
#' @noRd
`import,GMXFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        )
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        lines <- import(
            file = resource(con),
            format = "lines",
            metadata = FALSE,
            quiet = quiet
        )
        object <- list(tail(lines, n = -2L))
        names(object) <- lines[[1L]]
        object
    }



#' Import a gene set file (`.grp`)
#'
#' @note Updated 2021-06-04.
#' @noRd
`import,GRPFile` <- `import,GMXFile`    # nolint



## Handoff methods =============================================================

#' Import a file using `rio::import()`
#'
#' @note Updated 2021-10-24.
#' @noRd
`import,RioHandoffFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        rownames = TRUE,
        rownameCol = NULL,
        colnames = TRUE,
        makeNames = getOption(
            x = "acid.import.make.names",
            default = syntactic::makeNames
        ),
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        ),
        ...
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(rownames),
            isScalar(rownameCol) || is.null(rownameCol),
            isFlag(colnames) || isCharacter(colnames),
            is.function(makeNames) ||
                is.null(makeNames) ||
                isFALSE(makeNames),
            isFlag(metadata),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "rio"
        whatFun <- "import"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list("file" = file, ...)
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        .returnImport(
            object = object,
            con = con,
            rownames = rownames,
            rownameCol = rownameCol,
            colnames = colnames,
            makeNames = makeNames,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



#' Import file using `rtracklayer::import()`
#'
#' @note Updated 2021-10-12.
#' @noRd
#'
#' @note Using `tryCatch()` here to error if there are any warnings.
`import,RtracklayerHandoffFile` <-  # nolint
    function(
        con,
        format,  # NULL
        text,  # NULL
        metadata = getOption(
            x = "acid.import.metadata",
            default = FALSE
        ),
        quiet = getOption(
            x = "acid.quiet",
            default = FALSE
        ),
        ...
    ) {
        assert(
            is.null(format),
            is.null(text),
            isFlag(metadata),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "rtracklayer"
        whatFun <- "import"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list("con" = file, ...)
        requireNamespaces(whatPkg)
        what <- methodFunction(
            f = whatFun,
            signature = signature(
                "con" = "character",
                "format" = "missing",
                "text" = "ANY"
            ),
            package = whatPkg
        )
        tryCatch(
            expr = {
                object <- do.call(what = what, args = args)
            },
            warning = function(w) {
                abort(w)  # nocov
            }
        )
        .returnImport(
            object = object,
            con = con,
            metadata = metadata,
            whatPkg = whatPkg,
            whatFun = whatFun,
            quiet = quiet
        )
    }



## S4 method exports ===========================================================

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "character",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,character,con`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "character",
        format = "character",
        text = "missingOrNULL"
    ),
    definition = `import,character,con`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "missingOrNULL",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,character,file`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "missingOrNULL",
        format = "character",
        text = "missingOrNULL"
    ),
    definition = `import,character,file`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "DelimFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,DelimFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "LinesFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,LinesFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "ExcelFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,ExcelFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "FASTAFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,FASTAFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "FASTQFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,FASTQFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "GMTFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,GMTFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "GMXFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,GMXFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "GRPFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,GRPFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "JSONFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,JSONFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "MTXFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,MTXFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PZFXFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,PZFXFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "YAMLFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,YAMLFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "RDSFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,RDSFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "RDataFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,RDataFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "RioHandoffFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,RioHandoffFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "RtracklayerHandoffFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,RtracklayerHandoffFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "BcbioCountsFile",
        format = "missingOrNULL",
        text = "missingOrNULL"
    ),
    definition = `import,BcbioCountsFile`
)
