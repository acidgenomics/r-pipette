#' Get JSON from an API
#'
#' @export
#' @note Updated 2020-01-05.
#'
#' @examples
#' ## Get current list of genomes from UCSC Genome Browser.
#' url <- "https://api.genome.ucsc.edu/list/ucscGenomes"
#' json <- getJSON(url)
#' names(json)
getJSON <- function(url) {
    requireNamespaces(c("httr", "jsonlite"))
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
