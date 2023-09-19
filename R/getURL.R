#' Download and import a URL
#'
#' @export
#' @note Updated 2023-09-19.
#'
#' @return `character`.
#' Source code lines.
#'
#' @examples
#' url <- "https://ftp.ncbi.nlm.nih.gov/genomes/"
#' if (goalie::isAnExistingURL(url)) {
#'     lines <- getURL(url)
#'     print(head(lines, n = 7L))
#' }
getURL <-
    function(
        url,
        quiet = getOption(x = "acid.quiet", default = FALSE)) {
        assert(
            isAnExistingURL(url),
            isFlag(quiet)
        )
        destfile <- tempfile()
        download.file(
            url = url,
            destfile = destfile,
            quiet = quiet
        )
        assert(isAFile(destfile))
        x <- import(con = destfile, format = "lines", quiet = TRUE)
        assert(is.character(x))
        unlink(destfile)
        x
    }
