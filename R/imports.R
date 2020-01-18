#' @importFrom GenomicRanges GRanges
#' @importFrom Matrix readMM writeMM
#' @importFrom RCurl getURL url.exists
#' @importFrom R.utils gzip
#' @importFrom S4Vectors DataFrame Rle head mcols mcols<- metadata metadata<-
#'   tail
#' @importFrom SingleCellExperiment reducedDimNames reducedDim
#' @importFrom SummarizedExperiment assayNames assayNames<- assays colData
#'   rowData rowRanges
#' @importFrom acidbase basenameSansExt bapply compressExtPattern dots
#'   extPattern initDir matchArgsToDoCall pasteURL realpath standardizeCall
#' @importFrom cli cli_alert cli_alert_info cli_alert_success cli_alert_warning
#'   cli_text
#' @importFrom data.table fread fwrite
#' @importFrom goalie assert allAreAtomic allAreExisting allAreFiles
#'   allAreNonExisting allAreURLs allHaveAccess areDisjointSets areSameLength
#'   areSetEqual formalCompress hasColnames hasCols hasDimnames hasInternet
#'   hasLength hasNames hasNoDuplicates hasRownames hasRows hasValidNames
#'   hasValidDimnames isAFile isAURL isAny isCharacter isFlag isMatchingRegex
#'   isScalar isString isSubset
#' @importFrom methods as is slotNames validObject
#' @importFrom rtracklayer import
#' @importFrom stats na.omit
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom utils download.file packageVersion
NULL
