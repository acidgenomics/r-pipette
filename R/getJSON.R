#' Get JSON from an API
#'
#' @export
#' @note Updated 2023-07-06.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `list`.
#'
#' @seealso
#' - httr2 package.
#'
#' @examples
#' ## Access the UCSC Genome Browser API.
#' url <- "https://api.genome.ucsc.edu/list/ucscGenomes"
#' json <- getJSON(url)
#' names(json)
getJSON <- function(url) {
    assert(
        requireNamespaces("httr2"),
        isAURL(url)
    )
    req <- httr2::request(url)
    resp <- httr2::req_perform(req)
    json <- httr2::resp_body_json(resp)
    assert(is.list(json))
    json
}
