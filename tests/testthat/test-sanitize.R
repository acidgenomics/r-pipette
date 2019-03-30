context("Sanitize")

# nolint start
DataFrame <- S4Vectors::DataFrame
rowRanges <- SummarizedExperiment::rowRanges
tibble <- tibble::tibble
# nolint end

data(rse, package = "acidtest", envir = environment())



# atomize ======================================================================
test_that("atomize : DataFrame", {
    data <- rowData(rse)
    # Note that older versions of SummarizedExperiment (e.g. 3.6) don't assign
    # rownames correctly to `rowData`, so we're fixing this here.
    rownames(data) <- rownames(rse)

    object <- atomize(data)
    expect_s4_class(object, "DataFrame")
    expect_true(hasRownames(object))
    expect_identical(
        object = lapply(object, class),
        expected = list(
            geneID = "character",
            geneName = "factor",
            geneBiotype = "factor",
            broadClass = "factor"
        )
    )
})

test_that("atomize : GRanges", {
    object <- atomize(rowRanges(rse))
    expect_s4_class(object, "GRanges")
    expect_true(hasNames(object))
    expect_identical(
        object = lapply(S4Vectors::mcols(object), class),
        expected = list(
            geneID = "character",
            geneName = "factor",
            geneBiotype = "factor",
            broadClass = "factor"
        )
    )
})



# removeNA =====================================================================
# Support for vectors (using `stats::na.omit`).
# This will return structure attributes about original size, with class omit.
with_parameters_test_that(
    "removeNA", {
        expect_identical(
            object = removeNA(object),
            expected = expected
        )
    },
    object = list(
        character = c("hello", "world", NA),
        numeric = c(1L, 2L, NA),
        DataFrame = DataFrame(
            a = c("A", NA, "C"),
            b = c(NA, NA, NA),
            c = c("B", NA, "D"),
            row.names = c("x", "y", "z")
        )
    ),
    expected = list(
        character = structure(
            .Data = c("hello", "world"),
            na.action = structure(3L, class = "omit")
        ),
        numeric = structure(
            .Data = c(1L, 2L),
            na.action = structure(3L, class = "omit")
        ),
        DataFrame = DataFrame(
            a = c("A", "C"),
            c = c("B", "D"),
            row.names = c("x", "z")
        )
    )
)



# sanitizeNA ===================================================================
with_parameters_test_that(
    "sanitizeNA", {
        expect_identical(
            object = sanitizeNA(object),
            expected = expected
        )
    },
    object = list(
        character = c(1L, "x", "", "NA"),
        data.frame = data.frame(
            a = c("foo", ""),
            b = c(NA, "bar"),
            stringsAsFactors = FALSE
        ),
        DataFrame1 = DataFrame(
            a = c("foo", ""),
            b = c(NA, "bar"),
            row.names = c("c", "d")
        ),
        DataFrame2 = DataFrame(
            a = c("foo", ""),
            b = c(NA, "bar")
        ),
        tbl_df = tibble(
            a = c("foo", ""),
            b = c(NA, "bar")
        )
    ),
    expected = list(
        character = c("1", "x", NA, NA),
        data.frame = data.frame(
            a = c("foo", NA),
            b = c(NA, "bar"),
            stringsAsFactors = FALSE
        ),
        DataFrame1 = DataFrame(
            a = c("foo", NA),
            b = c(NA, "bar"),
            row.names = c("c", "d")
        ),
        DataFrame2 = DataFrame(
            a = c("foo", NA),
            b = c(NA, "bar")
        ),
        tbl_df = tibble(
            a = c("foo", NA),
            b = c(NA, "bar")
        )
    )
)



# sanitizePercent ==============================================================
# FIXME Need to add this functionality
