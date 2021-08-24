## FIXME Need to add code coverage for FASTA and FASTQ files.
## FIXME Need to check `rownames`, `colnames`, `makeNames`, and `metadata`
## handling here consistently.



context("import : invalid input")

test_that("Invalid extension", {
    file <- file.path(tempdir(), "file.XXX")
    unlink(file, recursive = TRUE)
    file.create(file)
    expect_error(
        object = import(file = file),
        regexp = "xxx"
    )
    unlink(file, recursive = TRUE)
})

test_that("No extension", {
    file <- file.path(tempdir(), "example")
    unlink(file, recursive = TRUE)
    file.create(file)
    expect_error(
        object = import(file),
        regexp = "extension"
    )
    unlink(file, recursive = TRUE)
})



context("import : data frame")

test_that("Delimited files", {
    for (ext in c("csv", "csv.gz", "tsv")) {
        file <- file.path(file = "cache", paste0("example.", ext))
        object <- import(
            file = file,
            metadata = TRUE
        )
        expect_is(object, "data.frame")
        expect_is(
            object = attributes(object)[["import"]][["file"]],
            class = "character"
        )
    }
})

test_that("Custom engine support", {
    lapply(
        X = c(
            "base::read.table",
            "data.table::fread",
            "readr::read_delim",
            "vroom::vroom"
        ),
        FUN = function(x) {
            split <- strsplit(x = x, split = "::", fixed = TRUE)[[1L]]
            whatPkg <- split[[1L]]
            file <- file.path("cache", "example.csv.gz")
            object <- import(
                file = file,
                engine = whatPkg,
                metadata = TRUE
            )
            expect_is(object, "data.frame")
            expect_identical(
                object = attributes(object)[["import"]][["importerName"]],
                expected = x
            )
        }
    )
})



context("import : rtracklayer (GFF/GTF)")

test_that("GFF3", {
    object <- import(
        file = file.path("cache", "example.gff3"),
        metadata = TRUE
    )
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
        object = metadata(object)[["import"]][["importerName"]],
        expected = "rtracklayer::import"
    )
})

test_that("GTF", {
    object <- import(
        file = file.path("cache", "example.gtf"),
        metadata = TRUE
    )
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
        object = metadata(object)[["import"]][["importerName"]],
        expected = "rtracklayer::import"
    )
})



context("import : MTX")

test_that("MTX", {
    object <- import(
        file = file.path("cache", "single_cell_counts.mtx.gz"),
        metadata = TRUE
    )
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
        object = attributes(object)[["import"]][["importerName"]],
        expected = "Matrix::readMM"
    )
})



context("import : R script")

test_that("R script", {
    expect_is(
        object = import(file = file.path("cache", "example.R")),
        class = "character"
    )
})



context("import : R data")

test_that("R data", {
    object <- import(file = file.path("cache", "example.rda"))
    expect_s4_class(object, "DataFrame")
    expect_null(metadata(object)[["import"]])
    expect_null(attr(object, which = "import"))
})

test_that("R data serialized", {
    object <- import(file = file.path("cache", "example.rds"))
    expect_s4_class(object, "DataFrame")
    expect_null(metadata(object)[["import"]])
    expect_null(attr(object, which = "import"))
})

test_that("Error on RDA containing multiple objects.", {
    expect_error(
        object = import(file = file.path("cache", "multi.rda")),
        regexp = "single"
    )
})



context("import : GSEA")

test_that("MSigDB hallmark", {
    mapply(
        file = c(
            symbols = "h.all.v6.2.symbols.gmt",
            entrez = "h.all.v6.2.entrez.gmt"
        ),
        ids = list(
            symbols = c("JUNB", "CXCL2", "ATF3", "NFKBIA", "TNFAIP3", "PTGS2"),
            entrez = c("3726", "2920", "467", "4792", "7128", "5743")
        ),
        FUN = function(file, ids) {
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
        SIMPLIFY = FALSE
    )
})

test_that("T_CELL_ACTIVATION", {
    for (file in c(
        gmt = "geneset.gmt",
        gmx = "geneset.gmx",
        grp = "geneset.grp"
    )) {
        file <- file.path("cache", file)
        object <- import(file)
        expect_identical(names(object), "T_CELL_ACTIVATION")
        expect_identical(length(object[[1L]]), 44L)
        expect_identical(
            head(object[[1L]]),
            c("CADM1", "CD1D", "CD2", "CD24", "CD276", "CD28")
        )
    }
})



context("import : JSON/YAML")

test_that("JSON/YAML", {
    for (ext in c("json", "yml")) {
        file <- file.path("cache", paste0("example.", ext))
        object <- import(file)
        expect_is(object, "list")
    }
})

test_that("'rio::import()', e.g. Stata DTA file", {
    skip_if_not_installed(pkg = "haven")
    skip_if_not_installed(pkg = "rio")
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

test_that("PZFX", {
    file <- system.file("extdata", "exponential_decay.pzfx", package = "pzfx")
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

skip_if_not_installed(pkg = "readxl")

test_that("XLSX", {
    file <- file.path("cache", "example.xlsx")
    expect_identical(
        object = import(
            file = file,
            rownames = TRUE,
            colnames = TRUE,
            metadata = FALSE
        ),
        expected = data.frame(
            "genotype" = c(
                "wildtype",
                "knockout",
                "wildtype",
                "knockout"
            ),
            "treatment" = c(
                "control",
                "control",
                "treated",
                "treated"
            ),
            row.names = c(
                "sample1",
                "sample2",
                "sample3",
                "sample4"
            )
        )
    )
})

test_that("XLS", {
    file <- file.path("cache", "example.xls")
    expect_identical(
        object = import(
            file = file,
            rownames = TRUE,
            colnames = TRUE,
            metadata = FALSE
        ),
        expected = data.frame(
            "genotype" = c(
                "wildtype",
                "knockout",
                "wildtype",
                "knockout"
            ),
            "treatment" = c(
                "control",
                "control",
                "treated",
                "treated"
            ),
            row.names = c(
                "sample1",
                "sample2",
                "sample3",
                "sample4"
            )
        )
    )
})



context("import : bcbio files")

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



context("import : FASTA / FASTQ")

test_that("FASTA", {
    file <- file.path("cache", "example.fa.gz")
    object <- import(
        file = file,
        moleculeType = "DNA",
        metadata = TRUE
    )
    expect_is(object, "DNAStringSet")
    expect_identical(
        object = names(object)[[1L]],
        expected = "ENST00000456328.2"
    )
    expect_length(object[[1L]], 1657L)
    expect_is(metadata(object)[["attributes"]], "SimpleList")
    expect_identical(
        object = names(object),
        expected = names(metadata(object)[["attributes"]])
    )

})

test_that("FASTQ", {
    file <- file.path("cache", "example.fq.gz")
    object <- import(
        file = file,
        moleculeType = "DNA",
        metadata = TRUE
    )
    expect_is(object, "DNAStringSet")
    expect_identical(
        object = names(object)[[1L]],
        expected = "NS500233:572:H25VKBGX2:1:11101:16195:1041 3:N:0:1"
    )
    expect_length(object[[1L]], 8L)
})
