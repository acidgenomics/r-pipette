globalVariables(".")



packageVersion <- packageVersion("brio")

#' brio package cache URL
#' @keywords internal
#' @export
#' @examples
#' brioCacheURL
brioCacheURL <- paste0(
    "http://brio.seq.cloud/",
    "v", packageVersion$major, ".", packageVersion$minor  # nolint
)



# Using this for `extPattern()` also.
# Order is important here.
compressExtPattern <- "\\.(bz2|gz|xz|zip)"

#' File extension pattern
#'
#' Note the optional matching of compression formats here.
#'
#' @export
#' @examples
#' extPattern
extPattern <- paste0(
    "\\.([a-zA-Z0-9]+)",
    "(", compressExtPattern, ")?$"
)

#' Compression extension pattern
#' @export
#' @examples
#' compressExtPattern
compressExtPattern <- paste0(compressExtPattern, "$")



formalsList <- list(
    data.frame = quote(
        getOption("basejump.data.frame", default = "data.frame")
    ),
    load.dir = quote(
        getOption("basejump.load.dir", default = ".")
    ),
    save.dir = quote(
        getOption("basejump.save.dir", default = ".")
    ),
    save.ext = quote(
        getOption("basejump.save.ext", default = "rds")
    ),
    save.overwrite = quote(
        getOption("basejump.save.overwrite", default = TRUE)
    ),
    save.compress = quote(
        getOption("basejump.save.compress", default = TRUE)
    )
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
