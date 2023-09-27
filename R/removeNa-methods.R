#' @name removeNa
#' @inherit AcidGenerics::removeNa
#' @note Updated 2021-10-14.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' ## atomic ====
#' removeNa(c("hello", "world", NA))
#' removeNa(c(1, 2, NA))
#'
#' ## matrix ====
#' from <- matrix(
#'     data = c(1, NA, 3, NA, NA, NA, 2, NA, 4),
#'     nrow = 3,
#'     ncol = 3
#' )
#' print(from)
#' to <- removeNa(from)
#' print(to)
#'
#' ## DFrame ====
#' from <- S4Vectors::DataFrame(
#'     "a" = c("A", NA, "C"),
#'     "b" = c(NA, NA, NA),
#'     "c" = c("B", NA, "D")
#' )
#' print(from)
#' to <- removeNa(from)
#' print(to)
NULL



## Updated 2019-07-19.
.allNonNA <- function(x) {
    !all(is.na(x))
}



## Updated 2019-07-19.
`removeNa,atomic` <- # nolint
    function(object) {
        na.omit(object)
    }



## Updated 2019-07-19.
`removeNa,matrix` <- # nolint
    function(object) {
        keepRows <- apply(X = object, MARGIN = 1L, FUN = .allNonNA)
        keepCols <- apply(X = object, MARGIN = 2L, FUN = .allNonNA)
        object[keepRows, keepCols, drop = FALSE]
    }



## Updated 2023-04-26.
`removeNa,DFrame` <- # nolint
    `removeNa,matrix`

## Updated 2021-02-02.
`removeNa,Matrix` <- # nolint
    `removeNa,matrix`

## Updated 2019-07-19.
`removeNa,data.frame` <- # nolint
    `removeNa,matrix`



#' @rdname removeNa
#' @export
setMethod(
    f = "removeNa",
    signature = signature(object = "DFrame"),
    definition = `removeNa,DFrame`
)

#' @rdname removeNa
#' @export
setMethod(
    f = "removeNa",
    signature = signature(object = "Matrix"),
    definition = `removeNa,Matrix`
)

#' @rdname removeNa
#' @export
setMethod(
    f = "removeNa",
    signature = signature(object = "atomic"),
    definition = `removeNa,atomic`
)

#' @rdname removeNa
#' @export
setMethod(
    f = "removeNa",
    signature = signature(object = "data.frame"),
    definition = `removeNa,data.frame`
)

#' @rdname removeNa
#' @export
setMethod(
    f = "removeNa",
    signature = signature(object = "matrix"),
    definition = `removeNa,matrix`
)
