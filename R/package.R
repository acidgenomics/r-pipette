#' pipette
#'
#' Input/output functions for biological data formats.
#'
#' @keywords internal
#'
#' @importClassesFrom AcidBase missingOrNULL
#' @importClassesFrom BiocIO BiocFile
#' @importClassesFrom GenomicRanges GenomicRanges
#' @importClassesFrom IRanges IntegerRanges Ranges
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DataFrame List SimpleList
#'
#' @importFrom AcidBase basenameSansExt dots download fileExt initDir
#'   methodFunction pasteURL realpath requireNamespaces standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning
#'   toInlineString
#' @importFrom BiocGenerics as.data.frame end start strand width
#' @importFrom BiocIO resource
#' @importFrom GenomicRanges seqnames
#' @importFrom Matrix readMM writeMM
#' @importFrom RCurl getURL
#' @importFrom S4Vectors DataFrame Rle SimpleList head mcols mcols<- metadata
#'   metadata<- na.omit tail
#' @importFrom goalie assert allAreAtomic allAreExisting allAreFiles
#'   allAreNonExisting allAreURLs allHaveAccess areDisjointSets areSameLength
#'   areSetEqual bapply compressExtPattern extPattern formalCompress hasColnames
#'   hasCols hasDimnames hasInternet hasLength hasNames hasNoDuplicates
#'   hasRownames hasRows hasValidNames hasValidDimnames isAFile isAURL isAny
#'   isCharacter isFlag isInstalled isInt isMatchingRegex isNonNegative
#'   isPositive isScalar isString isSubset validNames validate
#' @importFrom methods as is hasMethod new setGeneric signature validObject
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom syntactic makeNames
#' @importFrom utils download.file packageName packageVersion
"_PACKAGE"
