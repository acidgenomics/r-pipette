#' @name as.DataFrame
#' @inherit AcidGenerics::as.DataFrame
#' @note Updated 2023-02-22.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(GRanges, sparseMatrix, package = "AcidTest")
#'
#' ## `GRanges` to `DFrame` ====
#' from <- GRanges
#' to <- as.DataFrame(from)
#' print(to)
#'
#' ## `Matrix` to `DFrame` ====
#' from <- sparseMatrix
#' to <- as.DataFrame(from)
#' print(to)
#'
#' ## `list` to `DFrame` ====
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
        assert(
            all(bapply(
                X = x,
                FUN = function(x, y) {
                    identical(length(x), y)
                },
                y = length(x[[1L]])
            )),
            msg = "List elements contain variable lengths."
        )
        if (!hasNames(x)) {
            names(x) <- paste0("X", seq_along(x))
        }
        ncols <- length(x)
        nrows <- length(x[[1L]])
        rn <- names(x[[1L]])
        ## Dynamically reorder list elements only when all named.
        if (!is.null(rn) && all(bapply(X = x, FUN = hasNames))) {
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
                "list", "DFrame"
            )
        )
        df
    }

## Updated 2022-02-08.
`as.DataFrame,matrix` <- # nolint
    function(x) {
        to <- as.data.frame(x, stringsAsFactors = FALSE)
        to <- as(to, "DFrame")
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

## Updated 2023-04-26.
`as.DataFrame,GRanges` <- # nolint
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

## Updated 2023-04-26.
`as.DataFrame,IRanges` <- # nolint
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
    signature = signature(x = "GRanges"),
    definition = `as.DataFrame,GRanges`
)

#' @rdname as.DataFrame
#' @export
setMethod(
    f = "as.DataFrame",
    signature = signature(x = "IRanges"),
    definition = `as.DataFrame,IRanges`
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
