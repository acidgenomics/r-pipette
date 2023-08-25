#' Get JSON from an API
#'
#' @export
#' @note Updated 2023-08-25.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `list`.
#'
#' @seealso
#' - httr2 package. Refer specifically to the `request`, `req_perform`, and
#' `resp_body_json` function documentation.
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
#' url <- pasteURL(
#'     "rest.ensembl.org",
#'     "info",
#'     "assembly",
#'     paste0(
#'         "Homo sapiens",
#'         "?", "content-type=application/json"
#'     ),
#'     protocol = "https"
#' )
#' if (goalie::isAnExistingURL(url)) {
#'     json <- getJSON(url)
#'     print(names(json))
#' }
getJSON <- function(url) {
    assert(isAnExistingURL(url))
    req <- request(url)
    resp <- req_perform(req)
    json <- resp_body_json(resp)
    assert(is.list(json))
    json
}
