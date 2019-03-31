context("import : GSEA")

with_parameters_test_that(
    "MSigDB hallmark", {
        file <- file.path("cache", file)
        object <- import(file)
        expect_identical(length(object), 50L)
        expect_identical(names(object)[[1L]], "HALLMARK_TNFA_SIGNALING_VIA_NFKB")
        expect_identical(length(object[[1L]]), 200L)
        expect_identical(head(object[[1L]]), ids)
    },
    file = c(
        symbols = "h.all.v6.2.symbols.gmt",
        entrez = "h.all.v6.2.entrez.gmt"
    ),
    ids = list(
        symbols = c("JUNB", "CXCL2", "ATF3", "NFKBIA", "TNFAIP3", "PTGS2"),
        entrez = c("3726", "2920", "467", "4792", "7128", "5743")
    )
)

with_parameters_test_that(
    "T_CELL_ACTIVATION", {
        file <- file.path("cache", file)
        object <- import(file)
        expect_identical(names(object), "T_CELL_ACTIVATION")
        expect_identical(length(object[[1L]]), 44L)
        expect_identical(
            head(object[[1L]]),
            c("CADM1", "CD1D", "CD2", "CD24", "CD276", "CD28")
        )
    },
    file = c(
        gmt = "geneset.gmt",
        gmx = "geneset.gmx",
        grp = "geneset.grp"
    )
)
