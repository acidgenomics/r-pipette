context("Import")

load(system.file("extdata", "rse.rda", package = "brio"))
load(system.file("extdata", "sce.rda", package = "brio"))

assay <- SummarizedExperiment::assay
mcols <- S4Vectors::mcols
metadata <- S4Vectors::metadata
seqnames <- GenomicRanges::seqnames

mat <- assay(rse)
sparse <- assay(sce)



# import =======================================================================
# AppVeyor chokes on XLSX file.
with_parameters_test_that(
    "import : data frame", {
        if (ext == "xlsx") skip_on_appveyor()
        file <- paste0("example.", ext)
        object <- import(file)
        expect_is(object, "data.frame")
        expect_identical(
            object = attr(object, "brio")[["file"]],
            expected = realpath(file)
        )
    },
    ext = c("csv", "csv.gz", "tsv", "xlsx")
)

test_that("import : GFF3", {
    object <- import("example.gff3")
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

test_that("import : GTF", {
    object <- import("example.gtf")
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

test_that("import : MatrixMarket file (.mtx)", {
    object <- import("single_cell_counts.mtx.gz")
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

test_that("import : Counts file (.counts)", {
    object <- import("example.counts")
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

test_that("import : R script", {
    expect_is(
        object = import(file = "example.R"),
        class = "character"
    )
})

# AppVeyor has a cryptic failure here.
# cannot read workspace version 167772160 written by R 512.3.5;
# need R 256.2.3 or newer
test_that("import : R Data", {
    skip_on_appveyor()

    # R data.
    object <- import("example.rda")
    expect_s4_class(object, "DataFrame")

    # R data serialized.
    object <- import("example.rds")
    expect_s4_class(object, "DataFrame")

    # Error on object containing multiple data.
    expect_error(
        object = import("multi.rda"),
        regexp = "File does not contain a single object"
    )
})

with_parameters_test_that(
    "import : list", {
        object <- import(paste0("example.", ext))
        expect_is(object, "list")
    },
    ext = c("json", "yml")
)

test_that("import : No extension", {
    # Missing extension.
    file.create("example")
    expect_error(
        object = import("example"),
        regexp = "missing value where TRUE/FALSE needed"
    )
    unlink("example")
})







# localOrRemoteFile ============================================================
test_that("localOrRemoteFile : Vectorized", {
    urls <- paste(brioCacheURL, c("example.csv", "example.rda"), sep = "/")
    files <- localOrRemoteFile(urls)
    expect_is(files, "character")
    expect_identical(basename(urls), basename(files))
})

# `normalizePath() returns different error messages depending on the R version.
# Current: No such file or directory
# AppVeyor: The system cannot find the file specified
test_that("localOrRemoteFile : Missing file", {
    expect_error(
        object = localOrRemoteFile("XXX.csv"),
        regexp = "path\\[1\\]"
    )
})



# saveData =====================================================================
test_that("saveData", {
    dir <- "example"
    paths <- file.path(
        getwd(),
        "example",
        c("rse.rda", "sce.rda")
    )
    names(paths) <- c("rse", "sce")

    # R data.
    object <- saveData(
        rse, sce,
        ext = "rda",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(object, paths)

    # R data serialized.
    object <- saveData(
        rse, sce,
        ext = "rds",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(
        object = basename(object),
        expected = c("rse.rds", "sce.rds")
    )

    # Check `overwrite = FALSE` mode.
    expect_warning(
        object = saveData(
            rse, sce,
            dir = dir, overwrite = FALSE
        ),
        regexp = "No files were saved."
    )

    unlink(dir, recursive = TRUE)
})

test_that("saveData : Invalid parameters", {
    expect_error(
        object = saveData(XXX),
        regexp = "object 'XXX' not found"
    )
    expect_error(
        object = saveData("example"),
        regexp = "non-standard evaluation"
    )
    expect_error(
        object = saveData(rse, dir = NULL),
        regexp = "isString"
    )
})



# transmit =====================================================================
# Note that only FTP is currently supported.
remoteDir <- paste(
    "ftp://ftp.pantherdb.org",
    "sequence_classifications",
    "current_release",
    sep = "/"
)

test_that("transmit", {
    # Travis is still having issues with FTP connections.
    # Test these steps locally instead.
    skip_on_travis()

    object <- transmit(
        remoteDir = remoteDir,
        pattern = "README",
        compress = FALSE
    )
    expected <- file.path(getwd(), "README")
    names(expected) <- "README"
    expect_identical(object, expected)

    # Check that function skips on existing.
    expect_message(
        object = transmit(
            remoteDir = remoteDir,
            pattern = "README",
            compress = FALSE
        ),
        regexp = "All files are already downloaded."
    )

    unlink("README")
})

test_that("transmit : Rename and compress", {
    skip_on_travis()

    object <- transmit(
        remoteDir = remoteDir,
        pattern = "README",
        rename = "readme.txt",
        compress = TRUE
    )
    expected <- file.path(getwd(), "readme.txt.gz")
    names(expected) <- "README"
    expect_identical(object, expected)

    unlink("readme.txt.gz")
})

# FIXME Improve the error messages for these.
test_that("transmit : Invalid parameters", {
    skip_on_travis()
    expect_error(
        object = transmit(
            remoteDir = "http://steinbaugh.com",
            pattern = "README"
        ),
        regexp = "ftp"
    )
    expect_error(
        object = transmit(
            remoteDir = "ftp://ftp.wormbase.org/pub/",
            pattern = "README"
        ),
        regexp = "remoteFiles"
    )
    expect_error(
        object = transmit(
            remoteDir = remoteDir,
            pattern = "XXX"
        ),
        regexp = "match"
    )
    expect_error(
        object = transmit(
            remoteDir = remoteDir,
            pattern = "README",
            rename = c("XXX", "YYY")
        ),
        regexp = "areSameLength"
    )
})



# writeCounts ==================================================================
test_that("writeCounts", {
    dir <- "example"
    expect_message(
        object = writeCounts(mat, sparse, dir = dir, compress = TRUE),
        regexp = "Writing mat, sparse"
    )
    expect_identical(
        object = list.files(dir),
        expected = c(
            "mat.csv.gz",
            "sparse.mtx.gz",
            "sparse.mtx.gz.colnames",
            "sparse.mtx.gz.rownames"
        )
    )
    # Require a matrix, and don't allow data frames.
    expect_error(
        object = writeCounts(mtcars),
        regexp = "mtcars is not a matrix"
    )
    # Check that `eval_bare` call errors on missing object.
    expect_error(
        object = writeCounts(XXX),
        regexp = "object 'XXX' not found"
    )
    unlink(dir, recursive = TRUE)
})
