#' pipette
#'
#' Input/output functions for biological data formats.
#'
#' @keywords internal
"_PACKAGE"



## Classes =====================================================================

#' @importClassesFrom GenomicRanges GRanges GRangesList
#' @importClassesFrom IRanges IRanges Ranges
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DFrame List SimpleList
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics as.DataFrame atomize droplevels2 encode export
#' factorize import matchRownameColumn metadata2 metadata2<- removeNa sanitizeNa
#' sanitizePercent unfactorize
#' @importFrom BiocGenerics anyDuplicated as.data.frame do.call end lapply start
#' strand which width
#' @importFrom GenomicRanges seqnames
#' @importFrom S4Vectors DataFrame Rle SimpleList as.factor decode droplevels
#' head mcols mcols<- metadata metadata<- na.omit runValue tail
#' @importFrom syntactic makeNames
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase basenameSansExt compress decompress dots download
#' fileExt initDir methodFunction pasteUrl realpath simpleClass standardizeCall
#' tempdir2
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning
#' toInlineString
#' @importFrom Matrix readMM writeMM
#' @importFrom goalie assert allAreAtomic allAreExisting allAreExistingUrls
#' allAreFiles allAreIntegerish allAreMatchingFixed allAreMatchingRegex
#' allAreNonExisting allHaveAccess areDisjointSets areSameLength areSetEqual
#' bapply compressExtPattern extPattern formalCompress hasColnames hasCols
#' hasDimnames hasDuplicates hasLength hasNames hasNoDuplicates hasRownames
#' hasRows hasValidNames hasValidDimnames isADir isAFile isATempFile isAUrl
#' isAnExistingUrl isAny isCharacter isFlag isInstalled isInt isMatchingFixed
#' isMatchingRegex isNonNegative isPositive isScalar isString isSubset
#' requireNamespaces validNames validate
#' @importFrom jsonlite read_json
#' @importFrom methods as is hasMethod new setGeneric signature slot slot<-
#' validObject
#' @importFrom utils download.file packageName packageVersion
#' @importFrom yaml yaml.load_file
NULL
