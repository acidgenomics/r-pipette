#' Extract dots from function
#'
#' @note This function uses non-standard evaluation.
#' @note Updated 2019-09-06.
#' @export
#'
#' @param ... Objects as dots.
#' @param character `logical(1)`.
#'   Return dots (`...`) as `character`.
#'
#' @return
#' - `character = FALSE`: `list`.
#'   Objects as `name` class.
#'   Can return the object from the `name` with `eval`.
#' - `character = TRUE`: `character`.
#'   Names of the dots.
#'
#' @seealso
#' - `help("dotsMethods", "methods")`.
#' - [tidyverse](http://tidyverse.org) documentation:
#'   - [rlang](http://rlang.tidyverse.org).
#'   - [dplyr utils](https://goo.gl/fhAuak).
#'   - [devtools infrastructure](https://goo.gl/bM5TrP).
#'
#' @examples
#' dots(a, b, c, character = FALSE)
#' dots(a, b, c, character = TRUE)
dots <- function(..., character = FALSE) {
    ## Alternatively, can use `rlang::eval_bare()` here.
    dots <- eval(substitute(alist(...)))
    assert(
        is.list(dots),
        hasLength(dots),
        hasNoDuplicates(dots)
    )
    ## Provide an informative error message when a user attempts to accidentally
    ## use standard evaluation with quotation.
    if (!all(bapply(dots, is.symbol))) {
        stop(
            "This function uses non-standard evaluation (NSE).\n",
            "Dot objects must be unquoted.\n\n",
            "More details on NSE:\n",
            "- https://cran.r-project.org/package=lazyeval\n",
            "- https://dplyr.tidyverse.org/articles/programming.html\n",
            "- http://adv-r.had.co.nz/Computing-on-the-language.html"
        )
    }
    ## Convert names (symbols) to character.
    names <- vapply(dots, as.character, character(1L))
    assert(hasNoDuplicates(names))
    if (isTRUE(character)) {
        names
    } else {
        dots
    }
}
