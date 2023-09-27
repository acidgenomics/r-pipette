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
#' url <- pasteUrl(
#'     "api.genome.ucsc.edu",
#'     "list",
#'     "ucscGenomes",
#'     protocol = "https"
#' )
#' if (goalie::isAnExistingUrl(url)) {
#'     json <- getJson(url)
#'     print(names(json))
#' }
#'
#' ## Access Ensembl genome assembly metadata.
#' ## The Ensembl REST API server is prone to connection issues, so keeping
#' ## disabled here in the working example.
#' ## > url <- pasteUrl(
#' ## >     "rest.ensembl.org",
#' ## >     "info",
#' ## >     "assembly",
#' ## >     paste0(
#' ## >         "Homo sapiens",
#' ## >         "?", "content-type=application/json"
#' ## >     ),
#' ## >     protocol = "https"
#' ## > )
#' ## > if (goalie::isAnExistingUrl(url)) {
#' ## >     json <- getJson(url)
#' ## >     print(names(json))
#' ## > }
getJson <- function(url) {
    assert(
        requireNamespaces("httr2"),
        isAnExistingUrl(url)
    )
    req <- httr2::request(url)
    resp <- httr2::req_perform(req)
    json <- httr2::resp_body_json(resp)
    assert(is.list(json))
    json
}
