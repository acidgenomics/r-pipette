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
#' File extension group.
setClass(
    Class = "DelimFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' Microsoft Excel file.\cr
#' File extension group.
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
#' File extension group.
setClass(
    Class = "RioFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' File extension supported by `rtracklayer::import`.\cr
#' File extension group.
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
#' Comma separated values (CSV) file.\cr
#' Contains `DelimFile` class.
setClass(
    Class = "CSVFile",
    contains = "DelimFile"
)



## FIXME Need to add support for this.
## Also need to add code coverage using an example file.

#' @describeIn PipetteFile-class
#' Fixed width format (FWF) file.\cr
#' Contains `DelimFile` class.
setClass(
    Class = "FWFFile",
    contains = "DelimFile"
)



#' @describeIn PipetteFile-class
#' MatrixMarket exchange (MTX) file.
setClass(
    Class = "MTXFile",
    contains = "PipetteFile"
)



#' GraphPad Prism file (PZFX)
#'
#' @note Updated 2021-06-03.
#' @noRd
setClass(
    Class = "PZFXFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' R Data file containing multiple objects.
setClass(
    Class = "RDataFile",
    contains = "PipetteFile"
)



#' @describeIn PipetteFile-class
#' R data file containing a single object (serialized).
setClass(
    Class = "RDSFile",
    contains = "PipetteFile"
)



setClass(
    Class = "TableFile",
    contains = "DelimFile"
)



setClass(
    Class = "TSVFile",
    contains = "DelimFile"
)



setClass(
    Class = "TXTFile",
    contains = "DelimFile"
)



setClass(
    Class = "XLSFile",
    contains = "ExcelFile"
)



setClass(
    Class = "XLSBFile",
    contains = "ExcelFile"
)



setClass(
    Class = "XLSXFile",
    contains = "ExcelFile"
)




## RDataObjectsFile (RDA / RData)
## RDataSingleObjectFile (RDS)
##
## GMTFile  GeneMatrixTransposedFile
## GMXFile  GeneMatrixFile
## GRPFile  GeneSetFile
##
## JSONFile
## YAMLFile
