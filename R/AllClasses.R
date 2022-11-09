#' File extension classes
#'
#' Currently intended for use with `import` function.
#'
#' @details
#' Extends `BiocFile` defined in BiocIO package.
#'
#' @export
#' @note Updated 2022-11-09.
#'
#' @section Primary classes:
#'
#' - `BcbioCountsFile`: bcbio-nextgen counts file.
#' - `CSVFile`: Comma-separated values file (CSV).
#' - `FASTAFile`: FASTA file.
#' - `FASTQFile`: FASTQ file.
#' - `GCTFile`: Gene Cluster Text file (GCT).
#' - `GMTFile`: Gene matrix transposed file (GMT).
#' - `GMXFile`: Gene matrix file (GMX).
#' - `GRPFile`: Gene set file (GRP).
#' - `JSONFile`: JSON file.
#' - `MTXFile`: MatrixMarket exchange file (MTX).
#' - `OBOFile`: Open Biomedical Ontologies file (OBO).
#' - `PZFXFile`: GraphPad Prism file (PZFX).
#' - `RDSFile`: R data file containing a single, serialized object (RDS).
#' - `RDataFile`: R Data file containing multiple objects (RData/RDA).
#' - `TSVFile`: Tab-separated values file (TSV).
#' - `TableFile`: Base R table file (TXT).
#' - `YAMLFile`: YAML file.
#'
#' @section `DelimFile-class`:
#'
#' Delimited file.
#'
#' File extension group supporting:
#'
#' - CSV: Comma-separated values
#' - TSV: Tab-separated values
#' - Base R table (TXT)
#'
#' @section `ExcelFile-class`:
#'
#' Microsoft Excel file.
#'
#' File extension group supporting:
#'
#' - XLS
#' - XLSB
#' - XLSX
#'
#' @section `LinesFile-class`:
#'
#' Source code lines file.
#'
#' File extension group supporting:
#'
#' - BASH: Bash shell script
#' - LOG: Log file
#' - MD: Markdown file
#' - PY: Python script
#' - R: R script
#' - RMD: R Markdown file
#' - SH: Shell script (e.g. Bash, POSIX)
#' - ZSH: Zsh shell script
#'
#' @section `RioHandoffFile-class`:
#'
#' File extension supported by `rio::import`.
#'
#' File extension group supporting:
#'
#' - ARFF: Weka Attribute-Relation File Format
#' - DBF: dBase Database File
#' - DIF: Data Interchange Format
#' - DTA: Stata
#' - FWF: Fixed-width formatting
#' - MAT: Matlab
#' - MTP: Minitab
#' - ODS: OpenDocument (LibreOffice)
#' - POR: SPSS
#' - PSV: Pipe-separated values
#' - SAS7BDAT: SASS
#' - SAV: SPSS
#' - SYD: Systat
#' - REC: Epi Info
#' - XPT: SASS
#'
#' @section `RtracklayerHandoffFile-class`:
#'
#' File extension supported by `rtracklayer::import()`.
#'
#' File extension group supporting:
#'
#' - BED, BED15, BEDGRAPH, BEDPE
#' - BIGWIG, BW, WIG
#' - GFF, GFF1, GFF2, GFF3, GTF
#' - BROADPEAK, NARROWPEAK
#'
#' @seealso
#' - `getGeneric`.
#' - `BiocIO::FileForFormat`.
setClass(
    Class = "PipetteFile",
    contains = "BiocFile"
)



## File extension groups =======================================================

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "DelimFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "ExcelFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "LinesFile",
    contains = "PipetteFile"
)



## Handoff classes =============================================================

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "RioHandoffFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "RtracklayerHandoffFile",
    contains = "PipetteFile"
)



## File extensions =============================================================

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "BcbioCountsFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "CSVFile",
    contains = "DelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "FASTAFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "FASTQFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "GCTFile",
    contains = "DelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "GMTFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "GMXFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "GRPFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "JSONFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "MTXFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "OBOFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PZFXFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "RDataFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "RDSFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "TableFile",
    contains = "DelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "TSVFile",
    contains = "DelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "YAMLFile",
    contains = "PipetteFile"
)
