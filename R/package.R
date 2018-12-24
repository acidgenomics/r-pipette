#' brio
#' 
#' Extension of the rio package with improved support for biological data
#' structures.
#' 
#' @importFrom RCurl getURL url.exists
#' @importFrom R.utils gzip
#' @importFrom data.table as.data.table fread
#' @importFrom goalie assert allAreExisting allAreNonExisting allAreURLs
#'   allHaveAccess areSameLength areSetEqual bapply formalCompress hasCols
#'   hasInternet hasLength hasNames hasNoDuplicates hasRownames hasRows isAFile
#'   isAURL isAny isCharacter isFlag isMatchingRegex isString isSubset
#'   standardizeCall
#' @importFrom jsonlite read_json
#' @importFrom readr read_lines read_tsv
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract str_match str_subset
#' @importFrom tibble as_tibble column_to_rownames
#' @importFrom tools file_path_sans_ext
#' @importFrom yaml yaml.load_file
"_PACKAGE"

# NAMESPACE conflicts
# import: rio, rtracklayer

#' @importFrom R.utils gzip
#' @export
R.utils::gzip
