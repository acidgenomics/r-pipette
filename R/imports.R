#' @importClassesFrom GenomicRanges GenomicRanges
#' @importClassesFrom IRanges IRanges Ranges
#' @importClassesFrom S4Vectors DataFrame
NULL



#' @importFrom AcidBase basenameSansExt bapply compress compressExtPattern
#'   decompress dots download extPattern fileExt formalsList initDir
#'   matchArgsToDoCall pasteURL realpath requireNamespaces standardizeCall
#' @importFrom AcidCLI alert alertInfo alertSuccess alertWarning
#' @importFrom BiocFileCache BiocFileCache bfcadd bfccache bfcdownload
#'   bfcneedsupdate bfcquery bfcrpath
#' @importFrom IRanges end start width
#' @importFrom S4Vectors DataFrame Rle head mcols mcols<- metadata metadata<-
#'   na.omit tail
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



## This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
