#' File extension classes
#'
#' Currently intended for use with `import` function.
#'
#' @note Updated 2021-06-03.
#'
#' @seealso
#' - `getGeneric`.
setClass(
    Class = "PipetteFile",
    contains = "character"
)
setValidity(
    Class = "PipetteFile",
    method = function(object) {
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
#' File extension group.
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
#' YAML file.
setClass(
    Class = "YAMLFile",
    contains = "PipetteFile"
)
