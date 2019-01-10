# Using this for `extPattern()` also.
# Order is important here.
compressExtPattern <- "\\.(bz2|gz|xz)"

#' File extension pattern
#'
#' Note optional matching of compression formats.
#'
#' @export
#' @examples
#' extPattern
extPattern <- paste0(
    "\\.([a-zA-Z0-9]+)",
    "(", compressExtPattern, ")?$"
)

#' Compression extension pattern
#'
#' @export
#' @examples
#' compressExtPattern
compressExtPattern <- paste0(compressExtPattern, "$")



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



#' R data load error
#' @export
#' @examples
#' message(rdataLoadError)
rdataLoadError <- paste(
    "Failed to load data.",
    "R data files must contain '.rda', '.rds', or '.RData' extension.",
    sep = "\n"
)



#' R data extension pattern
#' @export
#' @examples
#' rdataExtPattern
rdataExtPattern <- "\\.(rd[a|ata|s])$"
