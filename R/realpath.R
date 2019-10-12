#' Express file paths in canonical form
#'
#' @export
#' @inherit base::normalizePath
#' @note Updated 2019-10-12.
#'
#' @seealso
#' - [file.path()].
#' - [normalizePath()].
#'
#' @examples
#' realpath(".")
#' normalizePath(".")
realpath <- function(path) {
    ## Ensure we're matching the platform conventions.
    ## For example, AppVeyor CI tests on Windows but uses "/" instead of "\\".
    normalizePath(
        path = path,
        winslash = .Platform$file.sep,  # nolint
        mustWork = TRUE
    )
}
