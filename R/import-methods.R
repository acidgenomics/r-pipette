#' Import
#'
#' Read file by extension into R.
#'
#' @name import
#' @note Updated 2023-07-12.
#'
#' @details
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
#' `data.frame`, `DFrame`). Note that `tbl_df` (tibble) and `data.table`
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
#' @section FASTA and FASTQ files:
#'
#' FASTA and FASTQ files are currently managed internally by the Biostrings
#' package. Refer to `readDNAStringSet` and `readRNAStringSet` for details.
#' Import of these files will return `DNAStringSet` or `RNAStringSet` depending
#' on the input, defined by `moleculeType` argument.
#'
#' @section General feature format (GFF, GTF):
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' See also:
#'
#' - [Ensembl spec](https://useast.ensembl.org/info/website/upload/gff.html)
#' - [GENCODE spec](https://www.gencodegenes.org/pages/data_format.html)
#'
#' @section Gene cluster text format (GCT):
#'
#' Refer to the [IGV GCT website][] for details.
#'
#' [IGV GCT website]: https://software.broadinstitute.org/software/igv/GCT
#'
#' @section GSEA gene set files:
#'
#' Refer to the Broad Institute [GSEA wiki][] for details.
#'
#' [GSEA wiki]: https://goo.gl/3ZkDPb
#'
#' @section Matrix Market Exchange:
#'
#' Reading a Matrix Market Exchange file requires `ROWNAMES` and `COLNAMES`
#' sidecar files containing the corresponding row and column names of the sparse
#' matrix.
#'
#' @section bcbio-nextgen count matrix:
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
#' @section Duplicate methods:
#'
#' `GMTFile` and `OBOFile` are also supported by BiocSet package.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param con `character(1)`, `connection`, or `missing`.
#' The connection from which data is loaded or to which data is saved. If this
#' is a character vector, it is assumed to be a filename, and a corresponding
#' file connection is created and then closed after exporting the object. If a
#' `BiocFile` derivative, the data is loaded from or saved to the underlying
#' resource.  If missing, the function will return the output as a character
#' vector, rather than writing to a connection.
#'
#' @param format `character(1)` or `missing`.
#' An optional file format type, which can be used to override the file format
#' inferred from `con`. Only recommended for file and URL paths that don't
#' contain an extension.
#'
#' @param text `character` or `missing`.
#' *Not currently supported.*
#'
#' @param colnames `logical(1)` or `character`.
#' Automatically assign column names, using the first header row.
#' Applies to file types that return `data.frame` only.
#' Pass in a `character` vector to define the column names manually.
#'
#' @param comment `character(1)`.
#' Comment character to detect at beginning of line, which will skip when
#' parsing file. Use `""` to disable interpretation of comments, which is
#' particularly
#' useful when parsing lines.
#' *Applies to plain text delimited and source code lines only.*
#'
#' @param engine `character(1)`.
#' Engine (package) to use for import.
#'
#' Currently supported:
#' - base
#' - data.table
#' - readr
#'
#' @param makeNames `function`.
#' Apply syntactic naming function to (column) names.
#' Function is never applied to row names, when they are defined in object.
#'
#' @param moleculeType `character(1)`.
#' Molecule type, either DNA or RNA.
#' Most RNA-seq FASTQ files contain complementary DNA (cDNA) sequences, not
#' direct sequencing of the RNA molecules.
#'
#' @param nMax `integer(1)` or `Inf`.
#' Maximum number of lines to parse.
#' *Applies to plain text delimited, Excel, and source code lines only.*
#'
#' @param quote `character(1)`.
#' The set of quoting characters.
#' To disable quoting altogether, use `quote = ""` (not generally recommended).
#' *Applies to plain text delimited files only.*
#'
#' @param removeBlank `logical(1)`.
#' Remove blank lines.
#' *Applies to source code lines*.
#'
#' @param return `character(1)`.
#' Object class to return.
#'
#' @param rownameCol `NULL`, `character(1)`, or `integer(1)`.
#' *Applies only when `rownames = TRUE`.*
#' Column name to use for row names assignment.
#' If left `NULL` (default), the function will call `matchRownameCol()`
#' internally to attempt to automatically match the row name column (e.g.
#' `"rowname"` or `"rn"`).
#' Otherwise, can manually define using a scalar argument, either the name
#' directly or position in the column names.
#'
#' @param rownames `logical(1)`.
#' Automatically assign row names, if `rowname` column is defined.
#' Applies to file types that return a data frame only.
#'
#' @param rownamesFile,colnamesFile `character(1)` or `NULL`.
#' Row names and/or column names sidecare file.
#' Applies primarily to MatrixMarket Exchange files (e.g. `MTXFile`).
#'
#' @param sheet `character(1)` or `integer(1)`.
#' Sheet to read. Either a string (the name of a sheet), or an integer (the
#' position of the sheet). Defaults to the first sheet.
#' *Applies to Excel Workbook, Google Sheet, or GraphPad Prism file.*
#'
#' @param skip `integer(1)`.
#' Number of lines to skip.
#' *Applies to delimited file (CSV, TSV), Excel Workbook, or lines.*
#'
#' @param stripWhitespace `logical(1)`.
#' Strip leading and/or trailing whitespace.
#' *Applies to source code lines*.
#'
#' @return Varies, depending on the file type (format):
#'
#' - **R data serialized** (`RDS`):
#' *variable*.\cr
#' Currently recommend over RDA, if possible.\cr
#' Imported by `readRDS()`.
#' - **R data** (`RDA`, `RDATA`):
#' *variable*.\cr
#' Must contain a single object.
#' Doesn't require internal object name to match, unlike `loadData()`.\cr
#' Imported by `load()`.
#' - **Plain text delimited** (`CSV`, `TSV`, `TXT`):
#' `data.frame`.\cr
#' Data separated by commas, tabs, or visual spaces.\cr
#' Note that TXT structure is amgibuous and actively discouraged.\cr
#' Refer to `Data frame return` section for details on how to change the
#' default return type to `DFrame`, `tbl_df` or `data.table`.\cr
#' Imported by `readr::read_delim()` by default.
#' - **Excel workbook** (`XLSB`, `XLSX`):
#' `data.frame`.\cr
#' Resave in plain text delimited format instead, if possible.\cr
#' Imported by `readxl::read_excel()`.
#' - **Legacy Excel workbook (pre-2007)** (`XLS`):
#' `data.frame`.\cr
#' Resave in plain text delimited format instead, if possible.\cr
#' Note that import of files in this format is slow.\cr
#' Imported by `readxl::read_excel()`.
#' - **GraphPad Prism project** (`PZFX`):
#' `data.frame`.\cr
#' Experimental. Consider resaving in CSV format instead.\cr
#' Imported by `pzfx::read_pzfx()`.
#' - **General feature format** (`GFF`, `GFF1`, `GFF2`, `GFF3`, `GTF`):
#' `GRanges`.\cr
#' Imported by `rtracklayer::import()`.
#' - **MatrixMarket exchange sparse matrix** (`MTX`):
#' `sparseMatrix`.\cr
#' Imported by `Matrix::readMM()`.
#' - **Sequence alignment/map format (`SAM`, `BAM`, `CRAM`):
#' `list`.\cr
#' Imported by `Rsamtools::scanBam`.
#' - **Mutation annotation format** (`MAF`):
#' `MAF`.\cr
#' Imported by `maftools::read.maf()`.
#' - **Variant annotation format** (`VCF`, `BCF`):
#' `list`.\cr
#' Imported by `Rsamtools::scanBcf`.
#' - **Gene cluster text** (`GCT`):
#' `matrix` or `data.frame`.\cr
#' Imported by `readr::read_delim()`.
#' - **Gene sets (for GSEA)** (`GMT`, `GMX`):
#' `character`.
#' - **Browser extensible data** (`BED`, `BED15`, `BEDGRAPH`, `BEDPE`):
#' `GRanges`.\cr
#' Imported by `rtracklayer::import()`.
#' - **ChIP-seq peaks** (`BROADPEAK`, `NARROWPEAK`):
#' `GRanges`.\cr
#' Imported by `rtracklayer::import()`.
#' - **Wiggle track format** (`BIGWIG`, `BW`, `WIG`):
#' `GRanges`.\cr
#' Imported by `rtracklayer::import()`.
#' - **JSON serialization data** (`JSON`):
#' `list`.\cr
#' Imported by `jsonlite::read_json()`.
#' - **YAML serialization data** (`YAML`, `YML`):
#' `list`.\cr
#' Imported by `yaml::yaml.load_file()`.
#' - **Lines** (`LOG`, `MD`, `PY`, `R`, `RMD`, `SH`):
#' `character`.\cr
#' Source code or log files.\cr
#' Imported by `readr::read_delim()` by default.
#' - **Infrequently used rio-compatible formats** (`ARFF`, `DBF`, `DIF`, `DTA`,
#' `MAT`, `MTP`, `ODS`, `POR`, `SAS7BDAT`, `SAV`, `SYD`, `REC`, `XPT`):
#' *variable*.\cr
#' Imported by `rio::import()`.
#'
#' @seealso
#' Packages:
#'
#' - [BiocIO](https://bioconductor.org/packages/BiocIO/).
#' - [data.table](https://r-datatable.com/).
#' - [readr](https://readr.tidyverse.org/).
#' - [vroom](https://vroom.r-lib.org/).
#' - [readxl](https://readxl.tidyverse.org/).
#' - [rtracklayer](https://bioconductor.org/packages/rtracklayer/).
#' - [maftools](https://bioconductor.org/packages/maftools/).
#' - [Rsamtools](https://bioconductor.org/packages/Rsamtools/).
#' - [rio](https://cran.r-project.org/package=rio).
#'
#' Import functions:
#'
#' - `BiocIO::import()`.
#' - `data.table::fread()`.
#' - `maftools::read.maf()`.
#' - `readr::read_delim()`.
#' - `rio::import()`.
#' - `rtracklayer::import()`.
#' - `utils::read.table()`.
#' - `vroom::vroom()`.
#' - `Rsamtools::scanBam`.
#' - `Rsamtools::scanBcf`.
#'
#' @examples
#' con <- system.file("extdata", "example.csv", package = "pipette")
#'
#' ## Row and column names enabled.
#' x <- import(con = con)
#' print(head(x))
#'
#' ## Row and column names disabled.
#' x <- import(con = con, rownames = FALSE, colnames = FALSE)
#' print(head(x))
NULL



## Internal functions ==========================================================

#' Inform the user about start of file import
#'
#' @note Updated 2022-05-12.
#' @noRd
.alertImport <-
    function(con,
             whatPkg,
             whatFun) {
        assert(
            is(con, "PipetteFile"),
            isString(whatPkg),
            isString(whatFun)
        )
        file <- attr(x = con, which = "origResource")
        if (is.null(file)) {
            file <- resource(con)
        }
        fileType <- ifelse(test = isAURL(file), yes = "url", no = "file")
        ## Handle edge case of cleaning Google Sheets URL.
        if (identical(fileType, "url")) {
            file <- sub(pattern = "\\#.+$", replacement = "", x = file)
        }
        alert(sprintf(
            "Importing {.%s %s} using {.pkg %s}::{.fun %s}.",
            fileType, file,
            whatPkg, whatFun
        ))
        invisible(TRUE)
    }



#' Map file format extension to corresponding S4 file class
#'
#' @note Updated 2022-07-07.
#' @noRd
.formatToFileClass <-
    function(format) {
        class <- switch(
            EXPR = sub(
                pattern = compressExtPattern,
                replacement = "",
                x = tolower(format),
                fixed = FALSE,
                ignore.case = FALSE
            ),
            "arff" = "Rio",
            "bam" = "BAM",
            "bash" = "Lines",
            "bcbio-counts" = "BcbioCounts",
            "bcf" = "BCF",
            "bed" = "Rtracklayer",
            "bed15" = "Rtracklayer",
            "bedgraph" = "Rtracklayer",
            "bedpe" = "Rtracklayer",
            "bigwig" = "Rtracklayer",
            "broadpeak" = "Rtracklayer",
            "bw" = "Rtracklayer",
            "counts" = "BcbioCounts",
            "cram" = "CRAM",
            "csv" = "CSV",
            "dbf" = "Rio",
            "dif" = "Rio",
            "dta" = "Rio",
            "excel" = "Excel",
            "fa" = "FASTA",
            "fasta" = "FASTA",
            "fastq" = "FASTQ",
            "fq" = "FASTQ",
            "fwf" = "Rio",
            "gct" = "GCT",
            "gff" = "Rtracklayer",
            "gff1" = "Rtracklayer",
            "gff2" = "Rtracklayer",
            "gff3" = "Rtracklayer",
            "gmt" = "GMT",
            "gmx" = "GMX",
            "grp" = "GRP",
            "gsheet" = "Rio",
            "gtf" = "Rtracklayer",
            "json" = "JSON",
            "lines" = "Lines",
            "log" = "Lines",
            "maf" = "MAF",
            "mat" = "Rio",
            "md" = "Lines",
            "mtp" = "Rio",
            "mtx" = "MTX",
            "narrowpeak" = "Rtracklayer",
            "obo" = "OBO",
            "ods" = "Rio",
            "por" = "Rio",
            "psv" = "Rio",
            "py" = "Lines",
            "pzfx" = "PZFX",
            "r" = "Lines",
            "rda" = "RData",
            "rdata" = "RData",
            "rds" = "RDS",
            "rec" = "Rio",
            "rio" = "Rio",
            "rmd" = "Lines",
            "rtracklayer" = "Rtracklayer",
            "sam" = "SAM",
            "sas7bdat" = "Rio",
            "sav" = "Rio",
            "sh" = "Lines",
            "syd" = "Rio",
            "table" = "Table",
            "txt" = {
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
            },
            "tsv" = "TSV",
            "vcf" = "VCF",
            "wig" = "Rtracklayer",
            "xls" = "Excel",
            "xlsb" = "Excel",
            "xlsx" = "Excel",
            "xpt" = "Rio",
            "yaml" = "YAML",
            "yml" = "YAML",
            "zsh" = "Lines",
            abort(sprintf(
                "{.pkg %s} does not support {.var %s} extension.",
                .pkgName, format
            ))
        )
        class <- paste0("Pipette", class, "File")
        class
    }



#' Get function
#'
#' @note Updated 2021-08-24.
#' @noRd
#'
#' @param f `character(1)`.
#' Function name.
#' @param pkg `character(1)`.
#' Package name.
#'
#' @return `function`.
.getFunction <-
    function(f, pkg) {
        assert(
            isString(f),
            isString(pkg),
            requireNamespaces(pkg)
        )
        x <- get(x = f, envir = asNamespace(pkg), inherits = TRUE)
        assert(is.function(x))
        x
    }



#' Internal importer for a sparse matrix sidecar file (e.g. `.rownames`)
#'
#' @note Updated 2021-09-25.
#' @noRd
.importMTXSidecar <-
    function(file, quiet) {
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
#' @note Updated 2022-09-13.
#' @noRd
#'
#' @inheritParams AcidRoxygen::params
#' @param file `character(1)`.
#' Local file path or remote URL.
#' @param tempPrefix `character(1)`.
#' Prefix to use for temporary file basename.
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
#' file <- system.file("extdata", "example.csv", package = "pipette")
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
#' print(x)
.localOrRemoteFile <-
    function(file, quiet) {
        assert(
            isString(file),
            isFlag(quiet)
        )
        tmpDir <- tempdir2()
        tmpPrefix <- paste0(.pkgName, "-")
        fileExt <- fileExt(file)
        tmpFileExt <- ifelse(
            test = is.na(fileExt),
            yes = "",
            no = paste0(".", fileExt)
        )
        if (isAURL(file)) {
            assert(hasInternet())
            url <- file
            file <- tempfile(
                pattern = tmpPrefix,
                tmpdir = tmpDir,
                fileext = tmpFileExt
            )
            if (isSubset(
                x = fileExt(
                    path = url,
                    pattern = "\\.([a-zA-Z0-9]+)$"
                ),
                y = c(
                    "bz2",
                    "gz",
                    "rda",
                    "rds",
                    "xls",
                    "xlsx",
                    "xz",
                    "zip"
                )
            )) {
                ## Write binary.
                mode <- "wb"
            } else {
                ## Write (default).
                mode <- "w" # nocov
            }
            download(
                url = url,
                destfile = file,
                quiet = quiet,
                mode = mode
            )
        }
        assert(isAFile(file))
        if (isMatchingRegex(pattern = compressExtPattern, x = file)) {
            if (!isMatchingFixed(
                pattern = file.path(tmpDir, tmpPrefix),
                x = file
            )) {
                tmpFile <- tempfile(
                    pattern = tmpPrefix,
                    tmpdir = tmpDir,
                    fileext = tmpFileExt
                )
                file.copy(from = file, to = tmpFile)
                file <- tmpFile
            }
            file <- decompress(
                file = file,
                remove = TRUE,
                overwrite = TRUE
            )
        }
        realpath(file)
    }



#' Return standardized import object
#'
#' @note Updated 2021-09-24.
#' @noRd
.returnImport <-
    function(object,
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
             )) {
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
            assert(identical(colnames(object), colnames)) # nocov
        }
        ## Attempt to set row names automatically for data frames.
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
                    rownameCol <- colnames(object)[[rownameCol]] # nocov
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
            if (hasDuplicates(names(object))) {
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
                    alertWarning("Invalid names detected.") # nocov
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
#' @note Updated 2023-06-29.
#' @noRd
`import,character` <- # nolint
    function(con,
             format,
             text, # missing
             ...) {
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
        dots <- list(...)
        if (isSubset("quiet", names(dots))) {
            quiet <- dots[["quiet"]]
        } else {
            quiet <- getOption(x = "acid.quiet", default = FALSE)
        }
        assert(
            isString(format, nullOK = TRUE),
            is.null(text),
            isFlag(quiet)
        )
        if (isMatchingRegex(
            pattern = "^https://docs\\.google\\.com/spreadsheets",
            x = con
        )) {
            format <- "gsheet"
        }
        if (is.null(format)) {
            format <- fileExt(con)
        }
        assert(
            isString(format),
            msg = sprintf(
                fmt = paste(
                    "{.arg %s} ({.file %s}) doesn't contain extension.",
                    "Set the file format manually using {.arg %s}.",
                    "Refer to {.pkg %s}::{.fun %s} for details.",
                    sep = "\n"
                ),
                "con", basename(con),
                "format",
                "pipette", "import"
            )
        )
        class <- .formatToFileClass(format)
        assert(
            hasMethod(
                f = "import",
                signature = signature(
                    con = class,
                    format = "missing",
                    text = "missing"
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
                resource <- .localOrRemoteFile(con, quiet = quiet)
                con <- new(Class = class, resource = resource)
                attr(con, which = "origResource") <- origResource
            }
        )
        validObject(con)
        assert(is(con, "PipetteFile"))
        import(con = con, ...)
    }



## R data importers ============================================================

#' Import an R data file containing multiple objects (`.rda`)
#'
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteRDataFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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
                    conditionMessage(e) # nocov
                }
            )
        }
        object
    }



#' Import an R data serialized file (`.rds`)
#'
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteRDSFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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
                    conditionMessage(e) # nocov
                }
            )
        }
        object
    }



## Array importers =============================================================

#' Import a delimited file (e.g. `.csv`, `.tsv`).
#'
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteDelimFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             rownames = TRUE,
             rownameCol = NULL,
             colnames = TRUE,
             quote = "\"",
             comment = "",
             skip = 0L,
             nMax = Inf,
             engine = getOption(
                 x = "acid.import.engine",
                 default = "base"
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
             ),
             verbose = getOption(
                 x = "acid.verbose",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            is.null(format),
            is.null(text),
            isFlag(rownames),
            isScalar(rownameCol) || is.null(rownameCol),
            isFlag(colnames) || isCharacter(colnames),
            is.character(quote) && length(quote) <= 1L,
            is.character(comment) && length(comment) <= 1L,
            isInt(skip), isNonNegative(skip),
            isPositive(nMax),
            isString(engine),
            is.function(makeNames) ||
                is.null(makeNames) ||
                isFALSE(makeNames),
            isFlag(metadata),
            isFlag(quiet),
            isFlag(verbose)
        )
        if (isTRUE(verbose)) {
            assert(isFALSE(quiet))
        }
        file <- resource(con)
        ext <- switch(
            EXPR = class(con),
            "PipetteCSVFile" = "csv",
            "PipetteTSVFile" = "tsv",
            "PipetteTableFile" = "table",
            abort("Unsupported delim class.")
        )
        whatPkg <- match.arg(arg = engine, choices = .engines)
        if (identical(ext, "table")) {
            whatPkg <- "base" # nocov
        }
        switch(
            EXPR = whatPkg,
            "base" = {
                whatFun <- "read.table"
                args <- list(
                    "file" = file,
                    "blank.lines.skip" = TRUE,
                    "comment.char" = comment,
                    "fill" = FALSE,
                    "na.strings" = naStrings,
                    "nrows" = nMax,
                    "quote" = quote,
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
                    "quote" = quote,
                    "skip" = skip,
                    "showProgress" = FALSE,
                    "stringsAsFactors" = FALSE,
                    "strip.white" = TRUE,
                    "verbose" = verbose
                )
                if (isCharacter(colnames)) {
                    ## nocov start
                    args[["header"]] <- FALSE
                    args[["col.names"]] <- colnames
                    ## nocov end
                } else if (isTRUE(colnames)) {
                    ## Usage of "auto" instead of TRUE will attempt to handle
                    ## malformed columns, similar to readr engine.
                    args[["header"]] <- "auto"
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
                    "lazy" = TRUE,
                    "na" = naStrings,
                    "name_repair" = make.names,
                    "n_max" = nMax,
                    "progress" = verbose,
                    "quote" = quote,
                    "show_col_types" = verbose,
                    "skip" = skip,
                    "skip_empty_rows" = TRUE,
                    "trim_ws" = TRUE
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
        assert(allAreAtomic(object))
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
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteExcelFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
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
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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
        options(warn = 2L) # nolint
        object <- do.call(what = what, args = args)
        options(warn = warn) # nolint
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
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteMTXFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             rownamesFile,
             colnamesFile,
             metadata = getOption(
                 x = "acid.import.metadata",
                 default = FALSE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        file <- resource(con)
        origFile <- attr(con, which = "origResource")
        if (is.null(origFile)) {
            origFile <- file # nocov
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
                NULL # nocov
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
                NULL # nocov
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
#' @note Updated 2023-07-07.
#' @noRd
#'
#' @note This function doesn't support optional column names.
`import,PipettePZFXFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
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
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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



## Non-array importers =========================================================

#' Import source code lines
#'
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteLinesFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
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
             ),
             verbose = getOption(
                 x = "acid.verbose",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            is.null(format),
            is.null(text),
            is.character(comment) && length(comment) <= 1L,
            isInt(skip),
            isInt(skip), isNonNegative(skip),
            isPositive(nMax),
            isFlag(stripWhitespace),
            isFlag(removeBlank),
            isFlag(quiet),
            isFlag(verbose)
        )
        if (isTRUE(verbose)) {
            assert(isFALSE(quiet))
        }
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
                    "strip.white" = stripWhitespace,
                    "verbose" = verbose
                )
            },
            "readr" = {
                whatFun <- "read_lines"
                args <- list(
                    "file" = file,
                    "lazy" = TRUE,
                    "n_max" = nMax,
                    "progress" = verbose,
                    "skip" = skip,
                    ## This setting considers lines containing spaces empty.
                    "skip_empty_rows" = FALSE
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
                x = object,
                fixed = FALSE
            )
            object <- gsub(
                pattern = "[[:space:]]+$",
                replacement = "",
                x = object,
                fixed = FALSE
            )
        }
        if (isTRUE(removeBlank)) {
            assert(requireNamespaces("stringi"))
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
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteJSONFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             metadata = getOption(
                 x = "acid.import.metadata",
                 default = FALSE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteYAMLFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             metadata = getOption(
                 x = "acid.import.metadata",
                 default = FALSE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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

#' Import a binary sequencing alignment file (`.bam`)
#'
#' @note Updated 2023-07-12.
#' @noRd
`import,PipetteBAMFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "Rsamtools"
        whatFun <- "scanBam"
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
        assert(
            is.list(object),
            hasLength(object, n = 1L),
            is.list(object[[1L]])
        )
        object <- object[[1L]]
        object
    }



## FIXME This is decompressing and modifying the con, which we don't want
## How to we preserve the original input???

#' Import a binary variant call file (`.bcf`)
#'
#' @note Updated 2023-07-12.
#' @noRd
`import,PipetteBCFFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "Rsamtools"
        whatFun <- "scanBcf"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        origFile <- attr(x = con, which = "origResource")
        indexFile <- .localOrRemoteFile(
            file = paste0(origFile, ".csi"),
            quiet = quiet
        )
        if (!isAFile(paste0(file, ".csi"))) {
            file.copy(
                from = indexFile,
                to = paste0(file, ".csi"),
                overwrite = FALSE
            )
        }
        args <- list("file" = file)
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        assert(
            is.list(object),
            identical(
                x = names(object),
                y = c(
                    "CHROM",
                    "POS",
                    "ID",
                    "REF",
                    "ALT",
                    "QUAL",
                    "FILTER",
                    "INFO",
                    "FORMAT",
                    "GENO",
                    "RecordsPerRange"
                )
            )
        )
        object
    }



#' Import bcbio count matrix generated by featureCounts
#'
#' @details
#' Internal importer for a bcbio count matrix file (`.counts`).
#' These files contain an `"id"` column that we need to coerce to row names.
#'
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteBcbioCountsFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             metadata = getOption(
                 x = "acid.import.metadata",
                 default = FALSE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            is.null(format),
            is.null(text),
            isFlag(metadata),
            isFlag(quiet)
        )
        object <- import(
            con = resource(con),
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



#' Import a compressed reference-oriented alignment map file (`.cram`)
#'
#' @note Updated 2023-07-12.
#' @noRd
`import,PipetteCRAMFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            requireNamespaces("Rsamtools"),
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "Rsamtools"
        whatFun <- "scanBam"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        tmpBamFile <- Rsamtools::asBam(
            file = file,
            destination = tempfile(),
            overwrite = FALSE,
            indexDestination = TRUE
        )
        args <- list("file" = tmpBamFile)
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        file.remove(tmpBamFile)
        assert(
            is.list(object),
            hasLength(object, n = 1L),
            is.list(object[[1L]])
        )
        object <- object[[1L]]
        object
    }



#' Import a FASTA file
#'
#' @note Updated 2023-07-07.
#' @noRd
#'
#' @seealso
#' - `Biostrings::readDNAStringSet()`.
#' - `Biostrings::readRNAStringSet()`.
#' - `Biostrings::readAAStringSet()`.
#'
#' @return Varies, depending on the `moleculeType` argument:
#' - `"DNA"`: `DNAStringSet`.
#' - `"RNA"`: `RNAStringSet`.
#' - `"AA"`: `AAStringSet`.
`import,PipetteFASTAFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             moleculeType = c("DNA", "RNA", "AA"),
             metadata = getOption(
                 x = "acid.import.metadata",
                 default = FALSE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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
            "use.names" = TRUE
        )
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        tryCatch(
            expr = {
                object <- do.call(what = what, args = args)
            },
            warning = function(w) {
                msg <- w[["message"]]
                if (isMatchingFixed(
                    x = msg,
                    pattern = "invalid one-letter sequence codes"
                )) {
                    msg <- paste(
                        msg,
                        "Ensure that 'moleculeType' argument is correct.",
                        sep = "\n"
                    )
                }
                abort(msg)
            }
        )
        assert(is(object, paste0(moleculeType, "StringSet")))
        if (hasNames(object)) {
            if (allAreMatchingRegex(
                pattern = "\\bMI(MAT)?[0-9]+\\b",
                x = names(object)
            )) {
                alertInfo("miRBase FASTA file detected.")
                spl <- strsplit(
                    x = names(object),
                    split = " ",
                    fixed = TRUE
                )
                names <- vapply(
                    X = spl,
                    FUN = `[[`,
                    1L,
                    FUN.VALUE = character(1L),
                    USE.NAMES = FALSE
                )
                mat <- do.call(what = rbind, args = spl)
                assert(identical(ncol(mat), 5L))
                attributes <- DataFrame(
                    "id" = mat[, 1L],
                    "accession" = mat[, 2L],
                    "organism" = paste(mat[, 3L], mat[, 4L]),
                    "name" = mat[, 5L]
                )
                names(object) <- names
                rownames(attributes) <- names
                metadata(object)[["attributes"]] <- attributes
            } else if (allAreMatchingFixed(
                pattern = "|", x = names(object)
            )) {
                alertInfo(sprintf(
                    "Splitting attributes by {.var %s} separator.", "|"
                ))
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
#' @note Updated 2023-07-07.
#' @noRd
#'
#' @seealso
#' - `Biostrings::readDNAStringSet()`.
#'
#' @return Varies, depending on the `moleculeType` argument:
#' - `"DNA"`: `DNAStringSet`.
#' - `"RNA"`: `RNAStringSet`.
`import,PipetteFASTQFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             moleculeType = c("DNA", "RNA"),
             metadata = getOption(
                 x = "acid.import.metadata",
                 default = FALSE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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



#' Import a gene cluster text file (`.gct`)
#'
#' @note Updated 2023-07-07.
#' @noRd
#'
#' @seealso
#' - https://software.broadinstitute.org/software/igv/GCT
`import,PipetteGCTFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             metadata = getOption(
                 x = "acid.import.metadata",
                 default = FALSE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             ),
             return = c("matrix", "data.frame")) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        return <- match.arg(return)
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        object <- import(
            con = resource(con),
            format = "tsv",
            engine = "readr",
            skip = 2L,
            metadata = metadata,
            quiet = quiet
        )
        assert(
            is.data.frame(object),
            identical(
                x = colnames(object)[c(1L, 2L)],
                y = c("Name", "Description")
            )
        )
        header <- import(
            con = resource(con),
            format = "lines",
            nMax = 2L,
            quiet = TRUE
        )
        assert(
            is.character(header),
            hasLength(header, n = 2L)
        )
        header <- strsplit(header, split = "\t", fixed = TRUE)
        dim1 <- as.integer(header[[2L]][c(1L, 2L)])
        dim2 <- c(nrow(object), ncol(object) - 2L)
        assert(
            identical(dim1, dim2),
            msg = "Dimension mismatch."
        )
        switch(
            EXPR = return,
            "data.frame" = {
                out <- object
            },
            "matrix" = {
                out <- as.matrix(object[, seq(from = 3L, to = ncol(object))])
                assert(identical(dim(out), dim2))
                if (isTRUE(metadata)) {
                    attr(out, "import") <- attr(object, "import")
                }
            }
        )
        rownames(out) <- object[["Name"]]
        out
    }



#' Import a gene matrix transposed file (`.gmt`)
#'
#' @note Updated 2023-07-07.
#' @noRd
#'
#' @seealso `fgsea::gmtPathways()`.
`import,PipetteGMTFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        lines <- import(
            con = resource(con),
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
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteGMXFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        lines <- import(
            con = resource(con),
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
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteGRPFile` <- # nolint
    `import,PipetteGMXFile`



#' Import a mutation annotation format file (`.maf`)
#'
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteMAFFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "maftools"
        whatFun <- "read.maf"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list(
            "maf" = file,
            "verbose" = !quiet
        )
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        assert(is(object, "MAF"))
        object
    }



#' Import an open biomedical ontologies file (`.obo`)
#'
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteOBOFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
        assert(
            is.null(format),
            is.null(text),
            isFlag(quiet)
        )
        file <- resource(con)
        whatPkg <- "ontologyIndex"
        whatFun <- "get_ontology"
        if (isFALSE(quiet)) {
            .alertImport(
                con = con,
                whatPkg = whatPkg,
                whatFun = whatFun
            )
        }
        args <- list(
            "file" = file,
            "propagate_relationships" = "is_a",
            "extract_tags" = "everything"
        )
        what <- .getFunction(f = whatFun, pkg = whatPkg)
        object <- do.call(what = what, args = args)
        assert(is(object, "ontology_index"))
        object
    }



#' Import a sequence alignment map file (`.sam`)
#'
#' @note Updated 2023-07-12.
#' @noRd
`import,PipetteSAMFile` <- # nolint
    `import,PipetteCRAMFile`



#' Import a variant call file (`.vcf`)
#'
#' @note Updated 2023-07-12.
#' @noRd
`import,PipetteVCFFile` <- # nolint
    `import,PipetteBCFFile`



## Handoff methods =============================================================

#' Import a file using `rio::import()`
#'
#' @note Updated 2023-07-07.
#' @noRd
`import,PipetteRioFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
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
             ...) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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
#' @note Updated 2023-07-07.
#' @noRd
#'
#' @note Using `tryCatch()` here to error if there are any warnings.
`import,PipetteRtracklayerFile` <- # nolint
    function(con,
             format, # missing
             text, # missing
             metadata = getOption(
                 x = "acid.import.metadata",
                 default = FALSE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             ),
             ...) {
        if (missing(format)) {
            format <- NULL
        }
        if (missing(text)) {
            text <- NULL
        }
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
        assert(requireNamespaces(whatPkg))
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
                abort(w) # nocov
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
        format = "missing",
        text = "missing"
    ),
    definition = `import,character`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "character",
        format = "character",
        text = "missing"
    ),
    definition = `import,character`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteRDSFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteRDSFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteRDataFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteRDataFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteDelimFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteDelimFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteLinesFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteLinesFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteExcelFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteExcelFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteBAMFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteBAMFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteBCFFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteBCFFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteCRAMFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteCRAMFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteFASTAFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteFASTAFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteFASTQFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteFASTQFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteGCTFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteGCTFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteGMTFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteGMTFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteGMXFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteGMXFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteGRPFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteGRPFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteJSONFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteJSONFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteMAFFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteMAFFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteMTXFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteMTXFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteOBOFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteOBOFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipettePZFXFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipettePZFXFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteSAMFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteSAMFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteVCFFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteVCFFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteYAMLFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteYAMLFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteRioFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteRioFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteRtracklayerFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteRtracklayerFile`
)

#' @rdname import
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "PipetteBcbioCountsFile",
        format = "missing",
        text = "missing"
    ),
    definition = `import,PipetteBcbioCountsFile`
)



## Deprecated S4 method exports ================================================

## Updated 2022-09-13.
.importError <-
    function(con,
             format,
             text,
             ...) {
        abort(sprintf(
            "Need to define {.arg %s} (e.g. instead of {.arg %s}).",
            "con", "file"
        ))
    }

#' @rdname import
#' @usage NULL
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "missing",
        format = "missing",
        text = "missing"
    ),
    definition = .importError
)

#' @rdname import
#' @usage NULL
#' @export
setMethod(
    f = "import",
    signature = signature(
        con = "missing",
        format = "character",
        text = "missing"
    ),
    definition = .importError
)
