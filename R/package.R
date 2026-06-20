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

#' @importFrom AcidGenerics as.DataFrame
#' @importFrom AcidGenerics atomize
#' @importFrom AcidGenerics droplevels2
#' @importFrom AcidGenerics encode
#' @importFrom AcidGenerics export
#' @importFrom AcidGenerics factorize
#' @importFrom AcidGenerics import
#' @importFrom AcidGenerics matchRownameColumn
#' @importFrom AcidGenerics metadata2
#' @importFrom AcidGenerics metadata2<-
#' @importFrom AcidGenerics removeNa
#' @importFrom AcidGenerics sanitizeNa
#' @importFrom AcidGenerics sanitizePercent
#' @importFrom AcidGenerics unfactorize
#' @importFrom BiocGenerics anyDuplicated
#' @importFrom BiocGenerics as.data.frame
#' @importFrom BiocGenerics do.call
#' @importFrom BiocGenerics end
#' @importFrom BiocGenerics lapply
#' @importFrom BiocGenerics start
#' @importFrom BiocGenerics strand
#' @importFrom BiocGenerics which
#' @importFrom BiocGenerics width
#' @importFrom GenomicRanges seqnames
#' @importFrom S4Vectors DataFrame
#' @importFrom S4Vectors Rle
#' @importFrom S4Vectors SimpleList
#' @importFrom S4Vectors as.factor
#' @importFrom S4Vectors decode
#' @importFrom S4Vectors droplevels
#' @importFrom S4Vectors head
#' @importFrom S4Vectors mcols
#' @importFrom S4Vectors mcols<-
#' @importFrom S4Vectors metadata
#' @importFrom S4Vectors metadata<-
#' @importFrom S4Vectors na.omit
#' @importFrom S4Vectors runValue
#' @importFrom S4Vectors tail
#' @importFrom syntactic autopadZeros
#' @importFrom syntactic makeNames
NULL


## Standard functions ==========================================================

#' @importFrom AcidBase basenameSansExt
#' @importFrom AcidBase compress
#' @importFrom AcidBase decompress
#' @importFrom AcidBase dots
#' @importFrom AcidBase download
#' @importFrom AcidBase fileExt
#' @importFrom AcidBase initDir
#' @importFrom AcidBase methodFunction
#' @importFrom AcidBase pasteUrl
#' @importFrom AcidBase pkgCacheDir
#' @importFrom AcidBase randomString
#' @importFrom AcidBase realpath
#' @importFrom AcidBase simpleClass
#' @importFrom AcidBase standardizeCall
#' @importFrom AcidBase strRemoveEmpty
#' @importFrom AcidBase tempdir2
#' @importFrom AcidCLI abort
#' @importFrom AcidCLI alert
#' @importFrom AcidCLI alertInfo
#' @importFrom AcidCLI alertSuccess
#' @importFrom AcidCLI alertWarning
#' @importFrom AcidCLI toInlineString
#' @importFrom Matrix readMM
#' @importFrom Matrix writeMM
#' @importFrom goalie assert
#' @importFrom goalie allAreAtomic
#' @importFrom goalie allAreExisting
#' @importFrom goalie allAreExistingUrls
#' @importFrom goalie allAreFiles
#' @importFrom goalie allAreIntegerish
#' @importFrom goalie allAreMatchingFixed
#' @importFrom goalie allAreMatchingRegex
#' @importFrom goalie allAreNonExisting
#' @importFrom goalie allHaveAccess
#' @importFrom goalie areDisjointSets
#' @importFrom goalie areSameLength
#' @importFrom goalie areSetEqual
#' @importFrom goalie bapply
#' @importFrom goalie compressExtPattern
#' @importFrom goalie extPattern
#' @importFrom goalie formalCompress
#' @importFrom goalie hasColnames
#' @importFrom goalie hasCols
#' @importFrom goalie hasDimnames
#' @importFrom goalie hasDuplicates
#' @importFrom goalie hasLength
#' @importFrom goalie hasNames
#' @importFrom goalie hasNoDuplicates
#' @importFrom goalie hasRownames
#' @importFrom goalie hasRows
#' @importFrom goalie hasValidNames
#' @importFrom goalie hasValidDimnames
#' @importFrom goalie isADir
#' @importFrom goalie isAFile
#' @importFrom goalie isATempFile
#' @importFrom goalie isAUrl
#' @importFrom goalie isAnExistingUrl
#' @importFrom goalie isAny
#' @importFrom goalie isCharacter
#' @importFrom goalie isFlag
#' @importFrom goalie isInstalled
#' @importFrom goalie isInt
#' @importFrom goalie isMatchingFixed
#' @importFrom goalie isMatchingRegex
#' @importFrom goalie isNonNegative
#' @importFrom goalie isPositive
#' @importFrom goalie isScalar
#' @importFrom goalie isString
#' @importFrom goalie isSubset
#' @importFrom goalie requireNamespaces
#' @importFrom goalie validNames
#' @importFrom goalie validate
#' @importFrom jsonlite read_json
#' @importFrom methods as
#' @importFrom methods is
#' @importFrom methods hasMethod
#' @importFrom methods new
#' @importFrom methods setGeneric
#' @importFrom methods signature
#' @importFrom methods slot
#' @importFrom methods slot<-
#' @importFrom methods validObject
#' @importFrom utils packageName
#' @importFrom utils packageVersion
#' @importFrom yaml yaml.load_file
NULL
