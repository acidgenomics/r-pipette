#' Express file paths in canonical form
#'
#' @export
#' @inherit base::normalizePath
#' @note Updated 2019-10-12.
#'
#' @seealso
#' Standard path modifiers:
#' - [normalizePath()].
#' - [file.path()].
#'
#' Checking for existence, access:
#' - [file.access()].
#' - [file.exists()].
#' - [goalie::hasAccess()]
#' - [goalie::allHaveAccess()].
#'
#' @examples
#' realpath(".")
#' normalizePath(".")
realpath <- function(path) {
    assert(allHaveAccess(path))
    normalizePath(
        path = path,
        winslash = .Platform$file.sep,  # nolint
        mustWork = TRUE
    )
}
