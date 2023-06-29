## Classes =====================================================================

#' @importClassesFrom BiocIO BiocFile
#' @importClassesFrom GenomicRanges GRanges
#' @importClassesFrom IRanges IRanges Ranges
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DFrame List SimpleList
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics as.DataFrame atomize droplevels2 encode factorize
#' matchRownameColumn metadata2 metadata2<- removeNA sanitizeNA
#' sanitizePercent
#' @importFrom BiocGenerics anyDuplicated as.data.frame do.call end lapply start
#' strand which width
#' @importFrom BiocIO export import resource
#' @importFrom GenomicRanges seqnames
#' @importFrom S4Vectors DataFrame Rle SimpleList as.factor decode droplevels
#' head mcols mcols<- metadata metadata<- na.omit tail
#' @importFrom syntactic makeNames
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase basenameSansExt compress decompress dots download
#' fileExt initDir methodFunction pasteURL realpath standardizeCall tempdir2
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning
#' toInlineString
#' @importFrom Matrix readMM writeMM
#' @importFrom goalie assert allAreAtomic allAreExisting allAreFiles
#' allAreMatchingFixed allAreMatchingRegex allAreNonExisting allAreURLs
#' allHaveAccess areDisjointSets areSameLength areSetEqual bapply
#' compressExtPattern extPattern formalCompress hasColnames hasCols hasDimnames
#' hasDuplicates hasInternet hasLength hasNames hasNoDuplicates hasRownames
#' hasRows hasValidNames hasValidDimnames isAFile isAURL isAny isCharacter
#' isFlag isInstalled isInt isMatchingFixed isMatchingRegex isNonNegative
#' isPositive isScalar isString isSubset requireNamespaces validNames validate
#' @importFrom methods as is hasMethod new setGeneric signature validObject
#' @importFrom utils packageName packageVersion
NULL
