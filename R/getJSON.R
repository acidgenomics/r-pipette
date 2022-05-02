#' Get JSON from an API
#'
#' @export
#' @note Updated 2020-01-05.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `list`.
#'
#' @examples
#' ## Access the UCSC Genome Browser API.
#' url <- "https://api.genome.ucsc.edu/list/ucscGenomes"
#' json <- getJSON(url)
#' names(json)
getJSON <- function(url) {
    assert(
        requireNamespaces(c("httr", "jsonlite")),
        isAURL(url)
    )
    reponse <- httr::GET(
        url = url,
        httr::content_type("application/json")
    )
    httr::stop_for_status(reponse)
    x <- httr::content(reponse)
    x <- jsonlite::toJSON(x)
    x <- jsonlite::fromJSON(x)
    assert(is.list(x))
    x
}
