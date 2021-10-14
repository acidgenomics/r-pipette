#' Metadata
#'
#' Dynamically handles metadata assignment and extraction in a similar fashion
#' for both S3 and S4 objects.
#'
#' @name metadata2
#' @note Updated 2021-10-14.
#'
#' @section S3 class:
#'
#' Internally slots into `attributes()`.
#' Attribute is also accessible via `attr(object, which)`.
#'
#' @section S4 class:
#'
#' Requires that object extends `Annotated` class.
#' Internally slots into `metadata()`.
#' Attribute is also accessible via `metadata(object)[[which]]`.
#'
#' @inheritParams AcidRoxygen::params
#' @param which `character(1)`.
#'   A non-empty character string specifying which attribute is to be accessed.
#'   Note that positional `numeric` arguments are currently not allowed.
#' @param value `ANY`.
#'   Metadata values to assign into slot, defined by `which`.
#' @param ... Additional arguments.
#'
#' @return
#' - `metadata2()`: Metadata. Returns `NULL` on `which` match failure, similar
#'   to `attr()` and `metadata()`.
#' - `metadata2<-()`: Modified object.
#'
#' @examples
#' ## S3 ====
#' x <- data.frame()
#' metadata2(x, which = "A") <- "B"
#' metadata2(x, which = "A")
#' attr(x, which = "A")
#'
#' ## S4 (extending Annotated) ====
#' x <- DataFrame()
#' metadata2(x, which = "A") <- "B"
#' metadata2(x, which = "A")
#' metadata(x)[["A"]]
NULL



## Intended for S3 class, or S4 class that doesn't support `metadata()`.
## Updated 2019-10-24.
`metadata2,ANY` <-  # nolint
    function(x, which) {
        assert(isString(which))
        attr(x = x, which = which, exact = TRUE)
    }



## Intended for S4 class supporting `metadata()`.
## Updated 2019-10-24.
`metadata2,Annotated` <-  # nolint
    function(x, which) {
        assert(isString(which))
        metadata(x)[[which]]
    }



## Intended for S3 class, or S4 class that doesn't support `metadata()`.
## Updated 2019-10-24.
`metadata2<-,ANY,ANY` <-  # nolint
    function(x, which, value) {
        attr(x = x, which = which) <- value
        x
    }



## Intended for S3 class, or S4 class that doesn't support `metadata()`.
## Updated 2019-10-24.
`metadata2<-,Annotated,ANY` <-  # nolint
    function(x, which, value) {
        metadata(x)[[which]] <- value
        x
    }



#' @rdname metadata2
#' @export
setMethod(
    f = "metadata2",
    signature = signature(
        x = "ANY",
        which = "character"
    ),
    definition = `metadata2,ANY`
)

#' @rdname metadata2
#' @export
setMethod(
    f = "metadata2",
    signature = signature(
        x = "Annotated",
        which = "character"
    ),
    definition = `metadata2,Annotated`
)



#' @rdname metadata2
#' @export
setReplaceMethod(
    f = "metadata2",
    signature = signature(
        x = "ANY",
        which = "character",
        value = "ANY"
    ),
    definition = `metadata2<-,ANY,ANY`
)

#' @rdname metadata2
#' @export
setReplaceMethod(
    f = "metadata2",
    signature = signature(
        x = "Annotated",
        which = "character",
        value = "ANY"
    ),
    definition = `metadata2<-,Annotated,ANY`
)
