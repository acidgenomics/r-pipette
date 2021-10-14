## FIXME Move all methods down here



#' @name removeNA
#' @inherit AcidGenerics::removeNA
#' @note Updated 2019-10-12.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' ## atomic ====
#' removeNA(c("hello", "world", NA))
#' removeNA(c(1, 2, NA))
#'
#' ## matrix ====
#' from <- matrix(
#'     data = c(1, NA, 3, NA, NA, NA, 2, NA, 4),
#'     nrow = 3,
#'     ncol = 3
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
#'
#' ## DataFrame ====
#' from <- DataFrame(
#'     a = c("A", NA, "C"),
#'     b = c(NA, NA, NA),
#'     c = c("B", NA, "D")
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
NULL



## Updated 2019-07-19.
.allNonNA <- function(x) {
    !all(is.na(x))
}



## Updated 2019-07-19.
`removeNA,atomic` <-  # nolint
    function(object) {
        na.omit(object)
    }



## Updated 2019-07-19.
`removeNA,matrix` <-  # nolint
    function(object) {
        keepRows <- apply(X = object, MARGIN = 1L, FUN = .allNonNA)
        keepCols <- apply(X = object, MARGIN = 2L, FUN = .allNonNA)
        object[keepRows, keepCols, drop = FALSE]
    }



## Updated 2021-02-02.
`removeNA,Matrix` <-  # nolint
    `removeNA,matrix`



## Updated 2019-07-19.
`removeNA,data.frame` <-  # nolint
    `removeNA,matrix`



## Updated 2019-07-19.
`removeNA,DFrame` <-  # nolint
    `removeNA,data.frame`



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature(object = "DFrame"),
    definition = `removeNA,DFrame`
)

#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature(object = "Matrix"),
    definition = `removeNA,Matrix`
)

#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature(object = "atomic"),
    definition = `removeNA,atomic`
)

#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature(object = "data.frame"),
    definition = `removeNA,data.frame`
)

#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature(object = "matrix"),
    definition = `removeNA,matrix`
)
