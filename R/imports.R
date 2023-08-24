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
#' @importFrom BiocFileCache bfcadd bfccache bfcdownload bfcneedsupdate bfcquery
#' bfcrpath
#' @importFrom BiocGenerics anyDuplicated as.data.frame do.call end lapply start
#' strand which width
#' @importFrom BiocIO export import resource
#' @importFrom GenomicRanges seqnames
#' @importFrom S4Vectors DataFrame Rle SimpleList as.factor decode droplevels
#' head mcols mcols<- metadata metadata<- na.omit tail
#' @importFrom syntactic makeNames
NULL

#' @importMethodsFrom rtracklayer import
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase basenameSansExt compress decompress dots download
#' fileExt initDir methodFunction pasteURL realpath standardizeCall tempdir2
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning
#' toInlineString
#' @importFrom BiocFileCache BiocFileCache
#' @importFrom Matrix readMM writeMM
#' @importFrom data.table fread fwrite
#' @importFrom digest digest
#' @importFrom goalie assert allAreAtomic allAreExisting allAreExistingURLs
#' allAreFiles allAreMatchingFixed allAreMatchingRegex allAreNonExisting
#' allHaveAccess areDisjointSets areSameLength areSetEqual bapply
#' compressExtPattern extPattern formalCompress hasColnames hasCols hasDimnames
#' hasDuplicates hasLength hasNames hasNoDuplicates hasRownames hasRows
#' hasValidNames hasValidDimnames isADir isAFile isATempFile isAURL
#' isAnExistingURL isAny isCharacter isFlag isInstalled isInt isMatchingFixed
#' isMatchingRegex isNonNegative isPositive isScalar isString isSubset
#' requireNamespaces validNames validate
#' @importFrom httr2 req_perform request resp_body_json
#' @importFrom jsonlite read_json
#' @importFrom methods as is hasMethod new setGeneric signature validObject
#' @importFrom readr read_delim write_delim
#' @importFrom tools R_user_dir
#' @importFrom utils packageName packageVersion
#' @importFrom yaml yaml.load_file
NULL
