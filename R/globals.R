#' File extension pattern
#'
#' Note optional matching of compression formats.
#'
#' @export
#' @examples
#' extPattern
extPattern <- "\\.([a-zA-Z0-9]+)(\\.bz|bz2|gz|xz)?$"



formalsList <- list(
    data.frame = quote(getOption("brio.data.frame", "data.frame")),
    load.dir = quote(getOption("brio.load.dir", ".")),
    save.dir = quote(getOption("brio.save.dir", ".")),
    save.ext = quote(getOption("brio.save.ext", "rds")),
    save.overwrite = quote(getOption("brio.save.overwrite", TRUE)),
    save.compress = quote(getOption("brio.save.compress", TRUE))
)



#' `NA` strings
#' @export
#' @examples
#' naStrings
naStrings <- c("", "NA", "#N/A", "NULL", "null")
