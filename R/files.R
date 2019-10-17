#' File manipulation
#'
#' @name files
#' @note Updated 2019-10-17.
#'
#' @return Functions return normalized file paths, instead of `logical`, with
#'   the exception of [fileExists()], which returns an unnamed `logical`.
#'
#' @seealso
#' `help("files")`
#'
#' - `file.exists()`.
#' - `file.create()`.
#' - `file.remove()`.
#' - `file.rename()`.
#' - `file.append()`.
#' - `file.copy()`.
#' - `file.symlink()`.
#' - `file.link()`.
#'
#' - `goalie::isFile()`.
#' - `goalie::allAreFiles()`.
#'
#' @examples
#' unlink(c("a.txt", "b.txt", "c.txt"))
#' fileCreate("a.txt")
#' fileCopy(from = "a.txt", to = "b.txt")
#' fileAppend(from = "a.txt", to = "b.txt")
#' ## Symbolic links don't work on Windows.
#' if (.Platform$OS.type != "windows") {
#'     fileSymlink(from = "a.txt", to = "c.txt")
#' }
#' fileRemove("b.txt")
#' fileExists("a.txt", "b.txt", "c.txt")
#' unlink(c("a.txt", "b.txt", "c.txt"))
NULL



.dotsToFiles <- function(...) {
    x <- unlist(list(...))
    assert(isCharacter(x))
    x
}



#' @rdname files
#' @export
fileExists <- function(...) {
    file.exists(...)
}



#' @rdname files
#' @export
fileCreate <- function(...) {
    files <- .dotsToFiles(...)
    ok <- file.create(..., showWarnings = FALSE)
    assert(all(ok))
    invisible(realpath(files))
}



#' @rdname files
#' @export
fileRemove <- function(...) {
    files <- .dotsToFiles(...)
    files <- realpath(files)
    tryCatch(
        expr = file.remove(...),
        warning = function(w) {
            stop(w)
        }
    )
    invisible(files)
}



## Default "file1", "file2" approach is confusing.
#' @rdname files
#' @export
fileAppend <- function(from, to) {
    ok <- file.append(file1 = to, file2 = from)
    assert(isTRUE(ok))
    invisible(realpath(to))
}



#' @rdname files
#' @export
fileCopy <- function(from, to, overwrite = FALSE) {
    assert(fileExists(from))
    ok <- file.copy(
        from = from,
        to = to,
        overwrite = overwrite,
        recursive = FALSE,
        copy.mode = TRUE,
        copy.date = TRUE
    )
    assert(isTRUE(ok))
    invisible(realpath(to))
}



#' @rdname files
#' @export
fileSymlink <- function(from, to, overwrite = FALSE) {
    assert(
        fileExists(from),
        isFlag(overwrite)
    )
    if (identical(overwrite, FALSE)) {
        assert(!fileExists(to))
    } else if (identical(overwrite, TRUE) && fileExists(to)) {
        fileRemove(to)
    }
    file.symlink(from = from, to = to)
    assert(file.exists(to))
    invisible(realpath(to))
}
