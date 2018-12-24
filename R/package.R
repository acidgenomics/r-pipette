#' brio
#' 
#' Extension of the rio package with improved support for biological data
#' structures.
#' 
#' @importFrom Matrix readMM writeMM
#' @importFrom RCurl getURL url.exists
#' @importFrom R.utils gzip
#' @importFrom S4Vectors as.data.frame
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
#' @importFrom jsonlite read_json
#' @importFrom methods as is
#' @importFrom readr read_lines read_tsv write_lines
#' @importFrom readxl read_excel
#' @importFrom stats na.omit
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom tibble as_tibble column_to_rownames
#' @importFrom tools file_path_sans_ext
#' @importFrom utils download.file
#' @importFrom yaml yaml.load_file
"_PACKAGE"

# NAMESPACE conflicts
# import: rio, rtracklayer

#' @importFrom R.utils gzip
#' @export
R.utils::gzip
