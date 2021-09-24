## NOTE Not currently exporting these, since they are only used for `import`
## function, and do not need to be made accessible to other packages.



#' File extension classes
#'
#' Currently intended for use with `import` function.
#'
#' Extends `BiocFile` defined in BiocIO package.
#'
#' @keywords internal
#' @note Updated 2021-09-24.
#'
#' @seealso
#' - `getGeneric`.
setClass(
    Class = "PipetteFile",
    contains = "BiocFile"
)
setValidity(
    Class = "PipetteFile",
    method = function(object) {
        ## FIXME Update the validity checks here.
        ok <- validate(
            isString(object)
        )
        if (!isTRUE(ok)) return(ok)
        TRUE
    }
)



## File extension groups =======================================================
#' @describeIn PipetteFile-class
#' Delimited file.\cr
#' File extension group supporting:
#' - CSV: Comma-separated values
#' - TSV: Tab-separated values
#' - Base R table (TXT)
setClass(
    Class = "DelimFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' Microsoft Excel file.\cr
#' File extension group supporting:
#' - XLS
#' - XLSB
#' - XLSX
setClass(
    Class = "ExcelFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' Source code lines file.\cr
#' File extension group supporting:
#' - BASH: Bash shell script
#' - LOG: Log file
#' - MD: Markdown file
#' - PY: Python script
#' - R: R script
#' - RMD: R Markdown file
#' - SH: Shell script (e.g. Bash, POSIX)
#' - ZSH: Zsh shell script
setClass(
    Class = "LinesFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' File extension supported by `rio::import`.\cr
#' File extension group supporting:
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
setClass(
    Class = "RioFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' File extension supported by `rtracklayer::import`.\cr
#' File extension group supporting:
#' - BED, BED15, BEDGRAPH, BEDPE
#' - BIGWIG, BW, WIG
#' - GFF, GFF1, GFF2, GFF3, GTF
#' - BROADPEAK, NARROWPEAK
setClass(
    Class = "RtracklayerFile",
    contains = "PipetteFile"
)



## File extensions =============================================================
#' @describeIn PipetteFile-class
#' bcbio-nextgen counts file.
setClass(
    Class = "BcbioCountsFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' Comma-separated values file (CSV).
setClass(
    Class = "CSVFile",
    contains = "DelimFile"
)



#' @describeIn PipetteFile-class
#' FASTA file.
setClass(
    Class = "FASTAFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' FASTQ file.
setClass(
    Class = "FASTQFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' Gene matrix transposed file (GMT).
setClass(
    Class = "GMTFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' Gene matrix file (GMX).
setClass(
    Class = "GMXFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' Gene set file (GRP).
setClass(
    Class = "GRPFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' JSON file.
setClass(
    Class = "JSONFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' MatrixMarket exchange file (MTX).
setClass(
    Class = "MTXFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' GraphPad Prism file (PZFX).
setClass(
    Class = "PZFXFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' R Data file containing multiple objects (RData/RDA).
setClass(
    Class = "RDataFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' R data file containing a single, serialized object (RDS).
setClass(
    Class = "RDSFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' Base R table file (TXT).
setClass(
    Class = "TableFile",
    contains = "DelimFile"
)



#' @describeIn PipetteFile-class
#' Tab-separated values file (TSV).
setClass(
    Class = "TSVFile",
    contains = "DelimFile"
)



#' @describeIn PipetteFile-class
#' YAML file.
setClass(
    Class = "YAMLFile",
    contains = "PipetteFile"
)
