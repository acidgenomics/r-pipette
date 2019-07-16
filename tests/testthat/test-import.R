context("import")

test_that("Invalid extension", {
    expect_error(
        import(file = "file.XXX"),
        "XXX extension is not supported."
    )
})

with_parameters_test_that(
    "Delimited", {
        file <- file.path(file = "cache", paste0("example.", ext))
        object <- import(file)
        expect_is(object, "data.frame")
        expect_identical(
            object = attr(object, "brio")[["file"]],
            expected = realpath(file)
        )
    },
    ext = c("csv", "csv.gz", "tsv")
)

# AppVeyor chokes on XLSX.
test_that("XLSX", {
    skip_on_appveyor()
    file <- file.path("cache", "example.xlsx")
    object <- import(file)
    expect_is(object, "data.frame")
})

# Both Travis and AppVeyor choke on XLS.
test_that("XLS", {
    # nolint start
    # > skip_if_not(interactive())
    # > skip_on_appveyor()
    # > skip_on_travis()
    # nolint end
    file <- file.path("cache", "example.xls")
    object <- import(file)
    expect_is(object, "data.frame")
})

test_that("GFF3", {
    object <- import(file = file.path("cache", "example.gff3"))
    expect_s4_class(object, "GRanges")
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
        object = metadata(object)[["brio"]][["rtracklayer"]],
        expected = packageVersion("rtracklayer")
    )
})

test_that("GTF", {
    object <- import(file = file.path("cache", "example.gtf"))
    expect_s4_class(object, "GRanges")
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
        object = metadata(object)[["brio"]][["rtracklayer"]],
        expected = packageVersion("rtracklayer")
    )
})

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
    # Note that sparseMatrix S4 class doesn't support `metadata()`.
    expect_identical(
        object = attr(object, "brio")[["importer"]],
        expected = "Matrix::readMM"
    )
})

test_that("bcbio counts", {
    object <- import(file = file.path("cache", "example.counts"))
    expect_is(object, "matrix")
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
        object = attr(object, "brio")[["importer"]],
        expected = "readr::read_tsv"
    )
})

test_that("R script", {
    expect_is(
        object = import(file = file.path("cache", "example.R")),
        class = "character"
    )
})

# AppVeyor has a cryptic failure here.
# cannot read workspace version 167772160 written by R 512.3.5;
# need R 256.2.3 or newer
test_that("R data", {
    skip_on_appveyor()
    object <- import(file = file.path("cache", "example.rda"))
    expect_s4_class(object, "DataFrame")
})

test_that("R data serialized", {
    # RDS file loading currently has issues on AppVeyor.
    skip_on_appveyor()
    object <- import(file = file.path("cache", "example.rds"))
    expect_s4_class(object, "DataFrame")
})

with_parameters_test_that(
    "JSON/YAML", {
        object <- import(file = file.path("cache", paste0("example.", ext)))
        expect_is(object, "list")
    },
    ext = c("json", "yml")
)

test_that("No extension", {
    unlink("example", recursive = TRUE)
    file.create("example")
    expect_error(
        object = import("example"),
        regexp = "missing value where TRUE/FALSE needed"
    )
    unlink("example")
})

test_that("Error on RDA containing multiple objects.", {
    expect_error(
        object = import(file = file.path("cache", "multi.rda")),
        regexp = "File does not contain a single object"
    )
})

test_that("rio::import(), e.g. Stata DTA file", {
    skip_if_not_installed("haven")
    file <- system.file("examples/iris.dta", package = "haven")
    x <- import(file)
    expect_is(x, "data.frame")
    expect_identical(
        colnames(x),
        c("sepallength", "sepalwidth", "petallength", "petalwidth", "species")
    )
})

test_that("acid.data.frame global option", {
    file <- file.path(file = "cache", "example.csv")

    options("acid.data.frame" = "data.frame")
    object <- import(file)
    expect_s3_class(object, "data.frame")
    expect_true(hasRownames(object))
    expect_false(isSubset("rowname", colnames(object)))

    options("acid.data.frame" = "DataFrame")
    object <- import(file)
    expect_s4_class(object, "DataFrame")
    expect_true(hasRownames(object))
    expect_false(isSubset("rowname", colnames(object)))

    options("acid.data.frame" = "data.table")
    object <- import(file)
    expect_s3_class(object, "data.table")
    expect_false(hasRownames(object))
    expect_true(isSubset("rowname", colnames(object)))

    options("acid.data.frame" = "tbl_df")
    object <- import(file)
    expect_s3_class(object, "tbl_df")
    expect_false(hasRownames(object))
    expect_true(isSubset("rowname", colnames(object)))

    options("acid.data.frame" = NULL)
})
