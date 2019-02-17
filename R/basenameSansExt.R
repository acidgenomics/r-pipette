#' Get the base name without the file extension
#'
#' @details
#' This function is vectorized and supports multiple file paths.
#'
#' @export
#' @inherit base::basename
#'
#' @seealso
#' - [`basename()`][base::basename].
#' - `tools::file_path_sans_ext()`.
#'
#' @examples
#' basenameSansExt(c("dir/file.txt", "dir/archive.tar.gz"))
basenameSansExt <- function(path) {
    x <- basename(path = path)
    x <- sub(pattern = compressExtPattern, replacement = "", x = x)
    x <- sub(pattern = "([^.]+)\\.[[:alnum:]]+$", replacement = "\\1", x = x)
    x
}
