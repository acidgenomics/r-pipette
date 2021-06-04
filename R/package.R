#' pipette
#'
#' Input/output functions for biological data formats.
#'
#' @keywords internal
#'
#' @importClassesFrom AcidGenerics DataFrame List SimpleList
#' @importClassesFrom GenomicRanges GenomicRanges
#' @importClassesFrom IRanges IRanges Ranges
#' @importClassesFrom Matrix Matrix
#'
#' @importMethodsFrom IRanges end start width
#'
#' @importFrom AcidBase basenameSansExt bapply compress compressExtPattern
#'   decompress dots download download.file extPattern fileExt formalsList
#'   initDir packageName packageVersion pasteURL realpath requireNamespaces
#'   standardizeCall
#' @importFrom AcidCLI alert alertInfo alertSuccess alertWarning
#' @importFrom AcidGenerics DataFrame Rle end head mcols mcols<- metadata
#'   metadata<- na.omit start tail width
#' @importFrom Matrix readMM writeMM
#' @importFrom RCurl getURL
#' @importFrom goalie assert allAreAtomic allAreExisting allAreFiles
#'   allAreNonExisting allAreURLs allHaveAccess areDisjointSets areSameLength
#'   areSetEqual formalCompress hasColnames hasCols hasDimnames hasInternet
#'   hasLength hasNames hasNoDuplicates hasRownames hasRows hasValidNames
#'   hasValidDimnames isAFile isAURL isAny isCharacter isFlag isInstalled isInt
#'   isMatchingRegex isNonNegative isPositive isScalar isString isSubset
#'   validNames validate
#' @importFrom methods as is hasMethod new setGeneric setValidity slotNames
#'   validObject .hasSlot
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom syntactic makeNames
"_PACKAGE"
