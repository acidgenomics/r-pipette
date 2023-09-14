#' Get JSON from an API
#'
#' @export
#' @note Updated 2023-09-14.
#'
#' @details
#' Requires httr2 package to be installed.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `list`.
#'
#' @examples
#' ## Access the UCSC Genome Browser API.
#' url <- pasteURL(
#'     "api.genome.ucsc.edu",
#'     "list",
#'     "ucscGenomes",
#'     protocol = "https"
#' )
#' if (goalie::isAnExistingURL(url)) {
#'     json <- getJSON(url)
#'     print(names(json))
#' }
#'
#' ## Access Ensembl genome assembly metadata.
#' ## The Ensembl REST API server is prone to connection issues, so keeping
#' ## disabled here in the working example.
#' ## > url <- pasteURL(
#' ## >     "rest.ensembl.org",
#' ## >     "info",
#' ## >     "assembly",
#' ## >     paste0(
#' ## >         "Homo sapiens",
#' ## >         "?", "content-type=application/json"
#' ## >     ),
#' ## >     protocol = "https"
#' ## > )
#' ## > if (goalie::isAnExistingURL(url)) {
#' ## >     json <- getJSON(url)
#' ## >     print(names(json))
#' ## > }
getJSON <- function(url) {
    assert(
        requireNamespaces("httr2"),
        isAnExistingURL(url)
    )
    req <- httr2::request(url)
    resp <- httr2::req_perform(req)
    json <- httr2::resp_body_json(resp)
    assert(is.list(json))
    json
}
