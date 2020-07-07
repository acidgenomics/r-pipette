context("import : invalid input")

## As of v0.3.17, file metadata is disabled by default.
options("acid.import.metadata" = TRUE)

test_that("Invalid extension", {
    file.create("file.XXX")
    expect_error(
        object = import(file = "file.XXX"),
        regexp = "XXX"
    )
    unlink("file.XXX")
})

test_that("No extension", {
    unlink("example", recursive = TRUE)
    file.create("example")
    expect_error(
        object = import("example"),
        regexp = "does not contain file type extension"
    )
    unlink("example")
})



context("import : data frame")

skip_if_not(hasInternet())

with_parameters_test_that(
    "Delimited files", {
        file <- file.path(file = "cache", paste0("example.", ext))
        object <- import(file)
        expect_is(object, "data.frame")
        expect_identical(
            object = attributes(object)[["import"]][["file"]],
            expected = realpath(file)
        )
        expect_identical(
            object = attributes(object)[["import"]][["importer"]],
            expected = "vroom::vroom"
        )
    },
    ext = c("csv", "csv.gz", "tsv")
)

test_that("data.table mode", {
    options("acid.import.engine" = "data.table")
    file <- file.path("cache", "example.csv.gz")
    object <- import(file)
    expect_is(object, "data.frame")
    expect_identical(
        object = attributes(object)[["import"]][["importer"]],
        expected = "data.table::fread"
    )
})

test_that("readr mode", {
    options("acid.import.engine" = "readr")
    file <- file.path("cache", "example.csv.gz")
    object <- import(file)
    expect_is(object, "data.frame")
    expect_identical(
        object = attributes(object)[["import"]][["importer"]],
        expected = "readr::read_csv"
    )
})

test_that("vroom mode", {
    options("acid.import.engine" = "vroom")
    file <- file.path("cache", "example.csv.gz")
    object <- import(file)
    expect_is(object, "data.frame")
    expect_identical(
        object = attributes(object)[["import"]][["importer"]],
        expected = "vroom::vroom"
    )
})



context("import : rtracklayer (GFF/GTF)")

skip_if_not(hasInternet())

test_that("GFF3", {
    object <- import(file = file.path("cache", "example.gff3"))
    expect_s4_class(object, "GenomicRanges")
    expect_identical(
        object = levels(seqnames(object)),
        expected = "1"
    )
    expect_identical(
        object = colnames(mcols(object)),
        expected = c(
            "source",
            "type",
            "score",
            "phase",
            "ID",
            "Alias",
            "external_name",
            "logic_name",
            "Name",
            "biotype",
            "description",
            "gene_id",
            "havana_gene",
            "havana_version",
            "version",
            "Parent",
            "havana_transcript",
            "tag",
            "transcript_id",
            "transcript_support_level",
            "constitutive",
            "ensembl_end_phase",
            "ensembl_phase",
            "exon_id",
            "rank",
            "ccdsid",
            "protein_id"
        )
    )
    expect_identical(
        object = metadata(object)[["import"]][["importer"]],
        expected = "rtracklayer::import"
    )
})

test_that("GTF", {
    object <- import(file = file.path("cache", "example.gtf"))
    expect_s4_class(object, "GenomicRanges")
    expect_identical(
        object = levels(seqnames(object)),
        expected = "1"
    )
    expect_identical(
        object = colnames(mcols(object)),
        expected = c(
            "source",
            "type",
            "score",
            "phase",
            "gene_id",
            "gene_version",
            "gene_name",
            "gene_source",
            "gene_biotype",
            "transcript_id",
            "transcript_version",
            "transcript_name",
            "transcript_source",
            "transcript_biotype",
            "transcript_support_level",
            "exon_number",
            "exon_id",
            "exon_version",
            "tag",
            "ccds_id",
            "protein_id",
            "protein_version"
        )
    )
    expect_identical(
        object = metadata(object)[["import"]][["importer"]],
        expected = "rtracklayer::import"
    )
})



context("import : MTX")

skip_if_not(hasInternet())

test_that("MTX", {
    object <- import(file = file.path("cache", "single_cell_counts.mtx.gz"))
    expect_s4_class(object, "sparseMatrix")
    expect_identical(
        object = lapply(dimnames(object), head, n = 2L),
        expected = list(
            c("gene0001", "gene0002"),
            c("cell001", "cell002")
        )
    )
    ## Note that sparseMatrix S4 class doesn't support `metadata()`.
    expect_identical(
        object = attributes(object)[["import"]][["importer"]],
        expected = "Matrix::readMM"
    )
})



context("import : R script")

skip_if_not(hasInternet())

test_that("R script", {
    expect_is(
        object = import(file = file.path("cache", "example.R")),
        class = "character"
    )
})



context("import : R data")

skip_if_not(hasInternet())

## AppVeyor has a cryptic failure here.
## cannot read workspace version 167772160 written by R 512.3.5;
## need R 256.2.3 or newer

test_that("R data", {
    skip_on_appveyor()
    object <- import(file = file.path("cache", "example.rda"))
    expect_s4_class(object, "DataFrame")
    expect_null(metadata(object)[["import"]])
    expect_null(attr(object, which = "import"))
})

test_that("R data serialized", {
    skip_on_appveyor()
    object <- import(file = file.path("cache", "example.rds"))
    expect_s4_class(object, "DataFrame")
    expect_null(metadata(object)[["import"]])
    expect_null(attr(object, which = "import"))
})

test_that("Error on RDA containing multiple objects.", {
    expect_error(
        object = import(file = file.path("cache", "multi.rda")),
        regexp = "'multi.rda' does not contain a single object"
    )
})



context("import : GSEA")

skip_if_not(hasInternet())

with_parameters_test_that(
    "MSigDB hallmark", {
        file <- file.path("cache", file)
        object <- import(file)
        expect_identical(length(object), 50L)
        expect_identical(
            object = names(object)[[1L]],
            expected = "HALLMARK_TNFA_SIGNALING_VIA_NFKB"
        )
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



context("import : JSON/YAML")

skip_if_not(hasInternet())

with_parameters_test_that(
    "JSON/YAML", {
        object <- import(file = file.path("cache", paste0("example.", ext)))
        expect_is(object, "list")
    },
    ext = c("json", "yml")
)

test_that("'rio::import()', e.g. Stata DTA file", {
    skip_if_not_installed("haven")
    skip_if_not_installed("rio")
    file <- system.file("examples/iris.dta", package = "haven")
    x <- import(file)
    expect_is(x, "data.frame")
    expect_identical(
        colnames(x),
        c("sepallength", "sepalwidth", "petallength", "petalwidth", "species")
    )
})



context("import : PZFX")

skip_if_not_installed(pkg = "pzfx")
skip_if_not(hasInternet())

file <- system.file("extdata", "exponential_decay.pzfx", package = "pzfx")
stopifnot(file.exists(file))

test_that("PZFX", {
    x <- import(file, sheet = 1L)
    colnames(x)
    expect_identical(
        colnames(x),
        c(
            "Minutes",
            "Control_1", "Control_2", "Control_3",
            "Treated_1", "Treated_2", "Treated_3"
        )
    )
})



context("import : Excel")

skip_if_not(hasInternet())
skip_if_not_installed(pkg = "readxl")
skip_on_appveyor()

test_that("XLSX", {
    file <- file.path("cache", "example.xlsx")
    object <- import(file)
    expect_is(object, "data.frame")
})

test_that("XLS", {
    file <- file.path("cache", "example.xls")
    object <- import(file)
    expect_is(object, "data.frame")
})



context("import : bcbio files")

skip_if_not(hasInternet())

test_that("bcbio counts", {
    object <- import(file = file.path("cache", "example.counts"))
    expect_is(object, "matrix")
    expect_true(is.integer(object))
    expect_identical(
        object = head(rownames(object), n = 5L),
        expected = c(
            "ENSMUSG00000102693",
            "ENSMUSG00000064842",
            "ENSMUSG00000051951",
            "ENSMUSG00000102851",
            "ENSMUSG00000103377"
        )
    )
    expect_identical(
        head(colSums(object)),
        c(
            ## nolint start
            sample01 = 262,
            sample02 = 502,
            sample03 = 299,
            sample04 = 318,
            sample05 = 123,
            sample06 = 620
            ## nolint end
        )
    )
})
