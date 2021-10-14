#' pipette
#'
#' Input/output functions for biological data formats.
#'
#' @keywords internal
#'
#' @importClassesFrom AcidGenerics BiocFile DataFrame GenomicRanges
#'   IntegerRanges List Ranges SimpleList missingOrNULL
#' @importClassesFrom Matrix Matrix
#'
#' @importFrom AcidBase basenameSansExt dots download download.file fileExt
#'   initDir methodFunction packageName packageVersion pasteURL realpath
#'   requireNamespaces standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning
#'   toInlineString
#' @importFrom AcidGenerics DataFrame Rle SimpleList end head mcols mcols<-
#'   metadata metadata<- na.omit seqnames start strand tail width
#' @importFrom BiocIO resource
#' @importFrom Matrix readMM writeMM
#' @importFrom RCurl getURL
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
"_PACKAGE"
