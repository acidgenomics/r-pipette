## Classes =====================================================================
#' @importClassesFrom AcidBase missingOrNULL
#' @importClassesFrom BiocIO BiocFile
#' @importClassesFrom GenomicRanges GenomicRanges
#' @importClassesFrom IRanges IntegerRanges Ranges
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DataFrame List SimpleList
NULL



## S3 generics and methods =====================================================
#' @importFrom data.table as.data.table
#' @importFrom tibble as_tibble
NULL



## S4 generics and methods =====================================================
#' @importFrom AcidGenerics as.DataFrame atomize encode factorize
#'   matchRownameColumn metadata2 metadata2<- removeNA sanitizeNA
#'   sanitizePercent
#' @importFrom BiocGenerics as.data.frame end start strand width
#' @importFrom BiocIO export import resource
#' @importFrom GenomicRanges seqnames
#' @importFrom S4Vectors DataFrame Rle SimpleList decode droplevels head mcols
#'   mcols<- metadata metadata<- na.omit tail
#' @importFrom methods coerce
#' @importFrom syntactic makeNames
NULL



## Standard functions ==========================================================
#' @importFrom AcidBase basenameSansExt compress decompress dots download
#'   fileExt initDir methodFunction pasteURL realpath requireNamespaces
#'   standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning
#'   toInlineString
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
#' @importFrom utils download.file packageName packageVersion
NULL
