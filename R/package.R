#' brio
#'
#' Extension of the rio and rtracklayer packages with improved support for
#' biological data structures.
#'
#' @importMethodsFrom S4Transformer coerce
#'
#' @importFrom Matrix readMM writeMM
#' @importFrom RCurl getURL url.exists
#' @importFrom R.utils gzip
#' @importFrom S4Transformer as.data.frame as_tibble atomize
#' @importFrom SingleCellExperiment reducedDims
#' @importFrom SummarizedExperiment assayNames assayNames<- assays colData
#'   rowData rowRanges
#' @importFrom data.table as.data.table fread
#' @importFrom goalie assert allAreExisting allAreNonExisting allAreURLs
#'   allHaveAccess areDisjointSets areSameLength areSetEqual bapply
#'   formalCompress hasColnames hasCols hasInternet hasLength hasNames
#'   hasNoDuplicates hasRownames hasRows hasValidDimnames isAFile isAURL isAny
#'   isCharacter isFlag isMatchingRegex isNonEmpty isString isSubset
#'   matchArgsToDoCall standardizeCall
#' @importFrom methods as is
#' @importFrom readr read_lines read_tsv write_lines
#' @importFrom stats na.omit
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom tibble as_tibble column_to_rownames
#' @importFrom tools file_path_sans_ext
#' @importFrom utils download.file head tail
"_PACKAGE"

# NAMESPACE conflicts
# import: rio, rtracklayer

#' @importFrom R.utils gzip
#' @export
R.utils::gzip

#' @importFrom utils untar
#' @export
utils::untar
