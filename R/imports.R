#' @importFrom AcidBase basenameSansExt bapply compress compressExtPattern
#'   decompress dots download extPattern fileExt formalsList initDir
#'   matchArgsToDoCall pasteURL realpath requireNamespaces standardizeCall
#' @importFrom BiocFileCache BiocFileCache bfcadd bfccache bfcdownload
#'   bfcneedsupdate bfcquery bfcrpath
#' @importFrom RCurl getURL url.exists
#' @importFrom cli cli_alert cli_alert_info cli_alert_success cli_alert_warning
#'   cli_text
#' @importFrom goalie assert allAreAtomic allAreExisting allAreFiles
#'   allAreNonExisting allAreURLs allHaveAccess areDisjointSets areSameLength
#'   areSetEqual formalCompress hasColnames hasCols hasDimnames hasInternet
#'   hasLength hasNames hasNoDuplicates hasRownames hasRows hasValidNames
#'   hasValidDimnames isAFile isAURL isAny isCharacter isFlag isMatchingRegex
#'   isInt isScalar isString isSubset validNames
#' @importFrom methods as is slotNames validObject .hasSlot
#' @importFrom rappdirs user_cache_dir
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom syntactic makeNames
#' @importFrom utils download.file packageName packageVersion
#' @importFrom vroom cols vroom vroom_lines vroom_write
NULL



## This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL






## FIXME MOVE THESE TO BASEJUMP

#' @importClassesFrom GenomicRanges GenomicRanges
#' @importClassesFrom IRanges IRanges Ranges
#' @importClassesFrom S4Vectors DataFrame
NULL

## FIXME Move these to basejump
#' @importFrom IRanges end start width
#' @importFrom S4Vectors DataFrame Rle head mcols mcols<- metadata metadata<-
#'   na.omit tail
#' @importFrom SummarizedExperiment assayNames assayNames<- assay colData
#'   colData<- rowData rowData<- rowRanges
