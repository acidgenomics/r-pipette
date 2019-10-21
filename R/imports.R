#' @importMethodsFrom transformer coerce
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom Matrix readMM writeMM
#' @importFrom RCurl getURL url.exists
#' @importFrom R.utils gzip
#' @importFrom S4Vectors DataFrame Rle head mcols mcols<- metadata metadata<-
#'   tail
#' @importFrom SingleCellExperiment reducedDimNames reducedDim
#' @importFrom SummarizedExperiment assayNames assayNames<- assays colData
#'   rowData rowRanges
#' @importFrom data.table fread fwrite
#' @importFrom goalie assert allAreAtomic allAreExisting allAreFiles
#'   allAreNonExisting allAreURLs allHaveAccess areDisjointSets areSameLength
#'   areSetEqual bapply compressExtPattern extPattern formalCompress hasColnames
#'   hasCols hasDimnames hasInternet hasLength hasNames hasNoDuplicates
#'   hasRownames hasRows hasValidNames hasValidDimnames isAFile isAURL isAny
#'   isCharacter isFlag isMatchingRegex isScalar isString isSubset
#'   matchArgsToDoCall standardizeCall
#' @importFrom methods as is slotNames validObject
#' @importFrom rtracklayer import
#' @importFrom stats na.omit
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom transformer as.data.frame as.data.table as_tibble atomize decode
#'   encode factorize
#' @importFrom utils download.file packageVersion
NULL
