## Coerce an S4 DataFrame to a standard data.frame.
##
## This function will return an informative error if an S4 DataFrame contains
## complex columns that can't be coerced to atomic or list.
##
## Not exporting this method because we don't want to mask the default
## conventions currently used by Bioconductor.
##
## Updated 2019-08-08.
`.coerce,DataFrame,data.frame` <-  # nolint
    function(x) {
        ## Decode Rle columns, which can be coerced.
        x <- decode(x)
        ## Check for valid columns (atomic, list).
        valid <- vapply(
            X = x,
            FUN = function(x) {
                is.atomic(x) || is.list(x)
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = TRUE
        )
        ## Error if S4 columns are nested.
        if (!all(valid)) {
            invalid <- x[, names(valid[!valid]), drop = FALSE]
            invalid <- vapply(
                X = invalid,
                FUN = class,
                FUN.VALUE = character(1L)
            )
            invalid <- sprintf("%s (%s)", names(invalid), invalid)
            stop(
                "Only atomic and list columns are supported.\n",
                sprintf(
                    "Invalid columns: %s.",
                    toString(invalid, width = 100L)
                )
            )
        }
        ## Don't use `as.data.frame()` here. It can unexpectedly sanitize row
        ## names (e.g. gene symbols), whereas the `as()` method does not.
        as(x, "data.frame")
    }
