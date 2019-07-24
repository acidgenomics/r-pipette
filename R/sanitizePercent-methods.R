#' @name sanitizePercent
#' @inherit bioverbs::sanitizePercent
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' object <- c("100%", "10.0%", "1%", "0.1%", "0.01%")
#' class(object)
#' print(object)
#' x <- sanitizePercent(object)
#' class(x)
#' print(x)
NULL



#' @rdname sanitizePercent
#' @name sanitizePercent
#' @importFrom bioverbs sanitizePercent
#' @usage sanitizePercent(object, ...)
#' @export
NULL



## Updated 2019-07-19.
`sanitizePercent,atomic` <-  # nolint
    function(object) {
        object
    }



#' @rdname sanitizePercent
#' @export
setMethod(
    f = "sanitizePercent",
    signature = signature("atomic"),
    definition = `sanitizePercent,atomic`
)



## Updated 2019-07-19.
`sanitizePercent,character` <-  # nolint
    function(object) {
        if (all(grepl("%$", object))) {
            as.numeric(sub("%$", "", object)) / 100L
        } else {
            object
        }
    }



#' @rdname sanitizePercent
#' @export
setMethod(
    f = "sanitizePercent",
    signature = signature("character"),
    definition = `sanitizePercent,character`
)
