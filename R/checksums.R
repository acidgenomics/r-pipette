#' Generate checksum from file
#'
#' @name checksums
#' @note Updated 2023-08-24.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `digest::digest`.
#'
#' @examples
#' file <- system.file("extdata/example.csv", package = "pipette")
#' md5(file)
#' sha256(file)
NULL



## Updated 2023-08-24.
.digest <-
    function(file,
             algo = c("md5", "sha256")) {
        assert(isAFile(file))
        algo <- match.arg(algo)
        x <- digest(
            object = file,
            algo = algo,
            file = TRUE
        )
        assert(isString(x))
        x
    }


#' @rdname checksums
#' @export
md5 <- function(file) {
    .digest(file = file, algo = "md5")
}



#' @rdname checksums
#' @export
sha256 <- function(file) {
    .digest(file = file, algo = "sha256")
}
