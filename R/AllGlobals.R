globalVariables(".")



.compressExtPattern <- "\\.(bz2|gz|xz|zip)"
.extPattern <- paste0(
    "\\.([a-zA-Z0-9]+)",
    "(", .compressExtPattern, ")?$"
)
.compressExtPattern <- paste0(.compressExtPattern, "$")

.formalsList <- list(
    export.compress = quote(
        getOption("acid.export.compress", default = FALSE)
    ),
    export.dir = quote(
        getOption("acid.export.dir", default = ".")
    ),
    export.ext = quote(
        getOption(
            "acid.export.ext",
            default = c(
                "csv", "csv.gz", "csv.bz2",
                "tsv", "tsv.gz", "tsv.bz2"
            )
        )
    ),
    export.sparse.ext = quote(
        getOption(
            "acid.export.sparse.ext",
            default = c("mtx", "mtx.gz", "mtx.bz2")
        )
    ),
    load.dir = quote(
        getOption("acid.load.dir", default = ".")
    ),
    overwrite = quote(
        getOption("acid.overwrite", default = TRUE)
    ),
    save.compress = quote(
        getOption("acid.save.compress", default = TRUE)
    ),
    save.dir = quote(
        getOption("acid.save.dir", default = ".")
    ),
    save.ext = quote(
        getOption("acid.save.ext", default = c("rds", "rda"))
    )
)

.rdataExtPattern <- "\\.(rd[a|ata|s])$"

.rdataLoadError <- paste(
    "Failed to load data.",
    "R data files must contain '.rda', '.rds', or '.RData' extension.",
    sep = "\n"
)

.version <- packageVersion("brio")



#' brio test data URL
#' @export
#' @keywords internal
#' @examples
#' brioTestsURL
brioTestsURL <- paste0(
    "http://tests.acidgenomics.com/brio/",
    "v", .version$major, ".", .version$minor  # nolint
)



#' NA strings
#' @export
#' @examples
#' naStrings
naStrings <- c(
    "NA",
    "#N/A",
    "NULL",
    "null",
    ""
)
