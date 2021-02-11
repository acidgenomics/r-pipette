#' pipette
#'
#' Input/output functions for biological data formats.
#'
#' @keywords internal
"_PACKAGE"



#' @importClassesFrom AcidGenerics DataFrame List
#' @importClassesFrom GenomicRanges GenomicRanges
#' @importClassesFrom IRanges IRanges Ranges
#' @importClassesFrom Matrix Matrix
NULL



#' @importFrom AcidBase basenameSansExt bapply compress compressExtPattern
#'   decompress dots download extPattern fileExt formalsList initDir
#'   matchArgsToDoCall pasteURL realpath requireNamespaces standardizeCall
#' @importFrom AcidCLI alert alertInfo alertSuccess alertWarning
#' @importFrom AcidGenerics DataFrame Rle head mcols mcols<- metadata metadata<-
#'   na.omit tail
#' @importFrom BiocFileCache BiocFileCache bfcadd bfccache bfcdownload
#'   bfcneedsupdate bfcquery bfcrpath
#' @importFrom IRanges end start width
#' @importFrom Matrix readMM writeMM
#' @importFrom RCurl getURL
#' @importFrom digest digest
#' @importFrom goalie assert allAreAtomic allAreExisting allAreFiles
#'   allAreNonExisting allAreURLs allHaveAccess areDisjointSets areSameLength
#'   areSetEqual formalCompress hasColnames hasCols hasDimnames hasInternet
#'   hasLength hasNames hasNoDuplicates hasRownames hasRows hasValidNames
#'   hasValidDimnames isAFile isAURL isAny isCharacter isFlag isInstalled isInt
#'   isMatchingRegex isNonNegative isPositive isScalar isString isSubset
#'   validNames
#' @importFrom methods as is slotNames validObject .hasSlot
#' @importFrom rappdirs user_cache_dir
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom syntactic makeNames
#' @importFrom utils download.file packageName packageVersion
#' @importFrom vroom cols vroom vroom_lines vroom_write
NULL
