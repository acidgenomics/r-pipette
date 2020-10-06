#' @importClassesFrom GenomicRanges GenomicRanges
#' @importClassesFrom IRanges IRanges Ranges
#' @importClassesFrom S4Vectors DataFrame
NULL



#' @importFrom AcidBase basenameSansExt bapply compress compressExtPattern
#'   decompress dots extPattern fileExt formalsList initDir matchArgsToDoCall
#'   pasteURL realpath requireNamespaces standardizeCall
#' @importFrom BiocFileCache BiocFileCache bfcadd bfccache bfcdownload
#'   bfcneedsupdate bfcquery bfcrpath
#' @importFrom IRanges end start width
#' @importFrom Matrix readMM writeMM
#' @importFrom RCurl getURL
#' @importFrom S4Vectors DataFrame Rle head mcols mcols<- metadata metadata<-
#'   na.omit tail
#' @importFrom SingleCellExperiment reducedDimNames reducedDim
#' @importFrom SummarizedExperiment assayNames assayNames<- assay colData
#'   colData<- rowData rowData<- rowRanges
#' @importFrom cli cli_alert cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom data.table fread fwrite
#' @importFrom goalie assert allAreAtomic allAreExisting allAreFiles
#'   allAreNonExisting allAreURLs allHaveAccess areDisjointSets areSameLength
#'   areSetEqual formalCompress hasColnames hasCols hasDimnames hasInternet
#'   hasLength hasNames hasNoDuplicates hasRownames hasRows hasValidNames
#'   hasValidDimnames isAFile isAURL isAny isCharacter isFlag isMatchingRegex
#'   isInt isScalar isString isSubset validNames
#' @importFrom methods as is slotNames validObject .hasSlot
#' @importFrom rappdirs user_cache_dir
#' @importFrom readr read_lines write_lines
#' @importFrom rtracklayer import
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom syntactic makeNames
#' @importFrom utils download.file packageName packageVersion
#' @importFrom vroom cols vroom vroom_write
NULL



## This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
