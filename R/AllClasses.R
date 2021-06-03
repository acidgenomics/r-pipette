## DelimFile
## ExcelFile
## LinesFile
## MatrixMarketFile
## PrismFile
## RDAFile
## RDSFile
##
## RDataObjectsFile
## RDataSingleObjectFile
##
## GMTFile  GeneMatrixTransposedFile
## GMXFile  GeneMatrixFile
## GRPFile  GeneSetFile
##
## JSONFile
## YAMLFile
##
## BcbioCountsFile
##
## RtracklayerFile
## RioFile



#' Pipette file reference class
#'
#' @note Updated 2021-06-03.
setRefClass(
    Class = "PipetteFile",
    contains = "character"
)



#' Delimited file
#'
#' @note Updated 2021-06-03.
setClass(
    Class = "DelimFile",
    contains = "PipetteFile"
)
