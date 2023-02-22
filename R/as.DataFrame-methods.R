#' @name as.DataFrame
#' @inherit AcidGenerics::as.DataFrame
#' @note Updated 2023-02-22.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(GenomicRanges, sparseMatrix, package = "AcidTest")
#'
#' ## `GenomicRanges` to `DataFrame` ====
#' from <- GenomicRanges
#' to <- as.DataFrame(from)
#' print(to)
#'
#' ## `Matrix` to `DataFrame` ====
#' from <- sparseMatrix
#' to <- as.DataFrame(from)
#' print(to)
#'
#' ## `list` to `DataFrame` ====
#' from <- list(
#'     "a" = list(c(1, 2), c(3, 4)),
#'     "b" = list(NULL, NULL)
#' )
#' to <- as.DataFrame(from)
#' print(to)
NULL



## Updated 2023-02-22.
`as.DataFrame,list` <- # nolint
    function(x) { # nolint
        if (!hasLength(x)) {
            return(DataFrame())
        }
        if (!hasNames(x)) {
            names(x) <- paste0("X", seq_along(x))
        }
        ncols <- length(x)
        nrows <- length(x[[1L]])
        rn <- names(x[[1L]])
        if (!is.null(rn)) {
            assert(
                all(bapply(
                    X = x,
                    FUN = function(x, rn) {
                        areSetEqual(names(x), rn)
                    },
                    rn = rn
                )),
                msg = "Names of list elements are mismatched."
            )
            x <- lapply(
                X = x,
                FUN = function(x, rn) {
                    x <- x[rn]
                    x <- unname(x)
                    x
                },
                rn = rn
            )
        }
        df <- new(Class = "DFrame", listData = x, nrows = nrows)
        rownames(df) <- rn
        assert(
            identical(dim(df), c(nrows, ncols)),
            msg = sprintf(
                "Dimension mismatch during {.cls %s} to {.cls %s} coercion.",
                "list", "DataFrame"
            )
        )
        df
    }

## Updated 2022-02-08.
`as.DataFrame,matrix` <- # nolint
    function(x) {
        to <- as.data.frame(x, stringsAsFactors = FALSE)
        to <- as(to, "DataFrame")
        ## Move row names automatically, if defined.
        if (!hasRownames(to)) {
            rncol <- matchRownameColumn(to)
            if (is.character(rncol) && length(rncol) == 1L) {
                rownames(to) <- as.character(to[[rncol]])
                to[[rncol]] <- NULL
            }
        }
        to
    }

## Updated 2021-10-14.
`as.DataFrame,GenomicRanges` <- # nolint
    function(x) {
        DataFrame(
            "seqnames" = seqnames(x),
            "start" = start(x),
            "end" = end(x),
            "width" = width(x),
            "strand" = strand(x),
            mcols(x, use.names = FALSE),
            row.names = names(x)
        )
    }

## Updated 2021-10-14.
`as.DataFrame,IntegerRanges` <- # nolint
    function(x) {
        DataFrame(
            "start" = start(x),
            "end" = end(x),
            "width" = width(x),
            mcols(x, use.names = FALSE),
            row.names = names(x)
        )
    }

## Updated 2022-02-08.
`as.DataFrame,Matrix` <- # nolint
    function(x) {
        as.DataFrame(as.matrix(x))
    }

## Updated 2022-03-22.
`as.DataFrame,SimpleList` <- # nolint
    function(x) {
        as.DataFrame(as.list(x))
    }

## Updated 2022-02-08.
`as.DataFrame,data.frame` <- # nolint
    `as.DataFrame,matrix`

## Updated 2022-02-08.
`as.DataFrame,matrix` <- # nolint
    `as.DataFrame,matrix`



#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "GenomicRanges"),
    definition = `as.DataFrame,GenomicRanges`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "IntegerRanges"),
    definition = `as.DataFrame,IntegerRanges`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "Matrix"),
    definition = `as.DataFrame,Matrix`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "SimpleList"),
    definition = `as.DataFrame,SimpleList`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "data.frame"),
    definition = `as.DataFrame,data.frame`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "list"),
    definition = `as.DataFrame,list`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "matrix"),
    definition = `as.DataFrame,matrix`
)
