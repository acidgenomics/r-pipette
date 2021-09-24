## FIXME Take this out, now that decompress is defined in BiocIO.

## Modified version of:
## > getS3method(
## >     f = "decompressFile",
## >     class = "default",
## >     envir = asNamespace("R.utils")
## > )
##
## Note that `gunzip`, etc. are wrappers of `decompressFile`:
## > getS3method(
## >     f = "gunzip",
## >     class = "default",
## >     envir = asNamespace("R.utils")
## > )

#' @rdname compress
#' @export
decompress <- function(
    file,
    remove,
    overwrite
) {
    assert(
        isString(file),
        isFlag(remove),
        isFlag(overwrite)
    )
    file <- realpath(file)
    assert(isTRUE(grepl(pattern = compressExtPattern, x = file)))
    ext <- substring(
        text = file,
        first = regexpr(
            pattern = compressExtPattern,
            text = file,
            ignore.case = TRUE
        ) + 1L
    )
    destfile <- gsub(
        pattern = sprintf("[.]%s$", ext),
        replacement = "",
        x = file,
        ignore.case = TRUE
    )
    assert(!identical(file, destfile))
    if (isAFile(destfile)) {
        ## nocov start
        if (isTRUE(overwrite)) {
            file.remove(destfile)
        }
        else {
            abort(sprintf("File exists: {.file %s}.", destfile))
        }
        ## nocov end
    }
    ## For ZIP files, hand off to `utils::unzip()` and early return.
    if (identical(ext, "zip")) {
        destfile <- unzip(
            zipfile = file,
            files = NULL,
            list = FALSE,
            overwrite = overwrite,
            junkpaths = FALSE,
            exdir = ".",
            unzip = "internal",
            setTimes = FALSE
        )
        if (length(destfile) > 1L) {
            ## nocov start
            alertWarning(sprintf(
                "{.var %s} contains multiple files.",
                basename(file)
            ))
            ## nocov end
        }
        if (isTRUE(remove)) {
            file.remove(file)
        }
        destfile <- realpath(destfile)
        return(invisible(destfile))
    }
    whatFun <- switch(
        EXPR = ext,
        bz2 = "bzfile",
        gz = "gzfile",
        xz = "xzfile"
    )
    fun <- get(
        x = whatFun,
        envir = asNamespace("base"),
        mode = "function",
        inherits = FALSE
    )
    assert(is.function(fun))
    inn <- fun(file, open = "rb")
    on.exit(if (!is.null(inn)) close(inn))
    outComplete <- FALSE
    out <- file(destfile, open = "wb")
    on.exit({
        ## nocov start
        if (!is.null(out)) close(out)
        if (!outComplete) file.remove(destfile)
        ## nocov end
    }, add = TRUE)
    ## Don't keep as integer here, otherwise can hit integer overflow
    ## on large files.
    nbytes <- as.numeric(0L)
    repeat {
        bfr <- readBin(
            con = inn,
            what = raw(0L),
            size = 1L,
            ## See 'BFR.SIZE' in `R.utils::decompressFile`.
            n = 1e+07L
        )
        n <- length(bfr)
        if (n == 0L) break
        nbytes <- nbytes + n
        writeBin(bfr, con = out, size = 1L)
        bfr <- NULL
    }
    outComplete <- TRUE
    close(out)
    out <- NULL
    attr(destfile, "bytes") <- nbytes
    if (remove) {
        close(inn)
        inn <- NULL
        file.remove(file)
    }
    invisible(destfile)
}

formals(decompress)[c("remove", "overwrite")] <-
    .formalsList[c(
        "decompress.remove",
        "overwrite"
    )]
