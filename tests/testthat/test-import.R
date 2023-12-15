test_that("Invalid extension", {
    con <- file.path(tempdir2(), "file.XXX")
    file.create(con)
    expect_error(
        object = import(con),
        regexp = "XXX"
    )
    unlink2(con)
})

test_that("No extension", {
    con <- file.path(tempdir2(), "example")
    file.create(con)
    expect_error(
        object = import(con),
        regexp = "extension"
    )
    unlink2(con)
})

test_that("R data", {
    object <- import(file.path(cacheDir, "example.rda"))
    expect_s4_class(object, "DFrame")
    expect_null(metadata(object)[["import"]])
    expect_null(attr(object, which = "import"))
})

test_that("R data serialized", {
    object <- import(file.path(cacheDir, "example.rds"))
    expect_s4_class(object, "DFrame")
    expect_null(metadata(object)[["import"]])
    expect_null(attr(object, which = "import"))
})

test_that("Error on RDA containing multiple objects.", {
    expect_error(
        object = import(file.path(cacheDir, "multi.rda")),
        regexp = "single"
    )
})

for (engine in engines) {
    test_that(
        desc = paste("R script", engine, sep = " : "),
        code = {
            con <- file.path(cacheDir, "example.R")
            object <- import(
                con = con,
                engine = engine
            )
            expect_type(object, "character")
        }
    )
    test_that(
        desc = paste("Empty file", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            file.create(con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    engine = engine
                ),
                expected = character(0L)
            )
            unlink2(con)
        }
    )
    test_that(
        desc = paste("'comment' argument", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            vec <- c(
                "# comment 1",
                "aaa",
                "## comment 2",
                "bbb",
                "# comment 3",
                "ccc"
            )
            writeLines(text = vec, con = con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    comment = "",
                    engine = engine
                ),
                expected = vec
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    comment = "#",
                    engine = engine
                ),
                expected = c(
                    "aaa",
                    "bbb",
                    "ccc"
                )
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    comment = "# ",
                    engine = engine
                ),
                expected = c(
                    "aaa",
                    "## comment 2",
                    "bbb",
                    "ccc"
                )
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    comment = "##",
                    engine = engine
                ),
                expected = c(
                    "# comment 1",
                    "aaa",
                    "bbb",
                    "# comment 3",
                    "ccc"
                )
            )
            unlink2(con)
        }
    )
    test_that(
        desc = paste("'nMax' argument", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            vec <- c("aaa", "bbb", "ccc")
            writeLines(text = vec, con = con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    nMax = Inf,
                    engine = engine
                ),
                expected = vec
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    nMax = 2L,
                    engine = engine
                ),
                expected = vec[seq_len(2L)]
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    nMax = 1L,
                    engine = engine
                ),
                expected = vec[[1L]]
            )
            unlink2(con)
        }
    )
    test_that(
        desc = paste("'removeBlank' argument", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            vec <- c(
                "  aaa",
                "bbb  ",
                "   ",
                ""
            )
            writeLines(text = vec, con = con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    removeBlank = FALSE,
                    engine = engine
                ),
                expected = vec
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    removeBlank = TRUE,
                    stripWhitespace = FALSE,
                    engine = engine
                ),
                expected = c(
                    "  aaa",
                    "bbb  ",
                    "   "
                )
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    removeBlank = TRUE,
                    stripWhitespace = TRUE,
                    engine = engine
                ),
                expected = c(
                    "aaa",
                    "bbb"
                )
            )
            unlink2(con)
        }
    )
    test_that(
        desc = paste("'skip' argument", engine, sep = " : "),
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            vec <- c("aaa", "bbb", "ccc", "ddd")
            writeLines(text = vec, con = con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    skip = 0L,
                    engine = engine
                ),
                expected = vec
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    skip = length(vec) - 1L,
                    engine = engine
                ),
                expected = vec[[length(vec)]]
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    skip = 2L,
                    nMax = 1L,
                    engine = engine
                ),
                expected = vec[[3L]]
            )
            expect_error(
                object = import(
                    con = con,
                    format = "lines",
                    skip = 1L,
                    comment = "#",
                    engine = engine
                ),
                regexp = "comment"
            )
            expect_error(
                object = import(
                    con = con,
                    format = "lines",
                    skip = 1L,
                    removeBlank = TRUE,
                    engine = engine
                ),
                regexp = "removeBlank"
            )
            unlink2(con)
        }
    )
    test_that(
        desc = "'stripWhitespace' argument",
        code = {
            con <- file.path(tempdir2(), "lines.txt")
            vec <- c(
                "  aaa",
                "bbb  ",
                " ccc ",
                "  ddd  ",
                "eee",
                "   "
            )
            writeLines(text = vec, con = con)
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    stripWhitespace = FALSE,
                    engine = engine
                ),
                expected = vec
            )
            expect_identical(
                object = import(
                    con = con,
                    format = "lines",
                    stripWhitespace = TRUE,
                    engine = engine
                ),
                expected = c(
                    "aaa",
                    "bbb",
                    "ccc",
                    "ddd",
                    "eee",
                    ""
                )
            )
            unlink2(con)
        }
    )
}

for (engine in engines) {
    for (ext in c("csv", "csv.gz", "tsv")) {
        test_that(
            desc = paste(ext, engine, sep = " : "),
            code = {
                con <- file.path(cacheDir, paste0("example.", ext))
                object <- import(
                    con = con,
                    engine = engine,
                    metadata = TRUE
                )
                expect_s3_class(object, "data.frame")
                expect_true(hasRownames(object))
                expect_type(
                    object = attributes(object)[["import"]][["file"]],
                    type = "character"
                )
                expect_match(
                    object = attributes(object)[["import"]][["importerName"]],
                    regexp = engine
                )
                object <- import(
                    con = con,
                    engine = engine,
                    rownameCol = "rowname"
                )
                expect_true(hasRownames(object))
            }
        )
    }
}

test_that("Deprecated 'file' argument", {
    object <- import(file.path(cacheDir, "example.csv"))
    expect_s3_class(object, "data.frame")
})

test_that("GCT", {
    object <- import(
        con = file.path(cacheDir, "example.gct"),
        metadata = TRUE,
        return = "matrix"
    )
    expect_type(object, "double")
    expect_identical(
        object = dim(object),
        expected = c(10L, 2L)
    )
    expect_identical(
        object = rownames(object)[[1L]],
        expected = "1007_s_at"
    )
    expect_identical(
        object = colnames(object),
        expected = c("Tumor_One", "Normal_One")
    )
    expect_identical(
        object = attr(object, "import")[["importerName"]],
        expected = "readr::read_delim"
    )
    object <- import(
        con = file.path(cacheDir, "example.gct"),
        metadata = TRUE,
        return = "data.frame"
    )
    expect_s3_class(object, "data.frame")
    expect_identical(
        object = dim(object),
        expected = c(10L, 4L)
    )
    expect_identical(
        object = rownames(object)[[1L]],
        expected = "1007_s_at"
    )
    expect_identical(
        object = colnames(object),
        expected = c("Name", "Description", "Tumor_One", "Normal_One")
    )
    expect_identical(
        object = attr(object, "import")[["importerName"]],
        expected = "readr::read_delim"
    )
})

test_that("GFF3", {
    object <- import(
        con = file.path(cacheDir, "example.gff3"),
        metadata = TRUE
    )
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
        object = metadata(object)[["import"]][["importerName"]],
        expected = "rtracklayer::import"
    )
})

test_that("GTF", {
    object <- import(
        con = file.path(cacheDir, "example.gtf"),
        metadata = TRUE
    )
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
        object = metadata(object)[["import"]][["importerName"]],
        expected = "rtracklayer::import"
    )
})

test_that("MTX", {
    object <- import(
        con = file.path(cacheDir, "single_cell_counts.mtx.gz"),
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

test_that("MSigDB hallmark", {
    Map(
        file = c(
            "symbols" = "h.all.v6.2.symbols.gmt",
            "entrez" = "h.all.v6.2.entrez.gmt"
        ),
        ids = list(
            "symbols" = c("JUNB", "CXCL2", "ATF3", "NFKBIA", "TNFAIP3"),
            "entrez" = c("3726", "2920", "467", "4792", "7128")
        ),
        f = function(file, ids) {
            file <- file.path(cacheDir, file)
            object <- import(file)
            expect_length(object, 50L)
            expect_identical(
                object = names(object)[[1L]],
                expected = "HALLMARK_TNFA_SIGNALING_VIA_NFKB"
            )
            expect_length(object[[1L]], 200L)
            expect_identical(head(object[[1L]], n = 5L), ids)
        }
    )
})

test_that("MSigDB T_CELL_ACTIVATION", {
    for (file in c(
        gmt = "geneset.gmt",
        gmx = "geneset.gmx",
        grp = "geneset.grp"
    )) {
        file <- file.path(cacheDir, file)
        object <- import(file)
        expect_named(object, "T_CELL_ACTIVATION")
        expect_length(object[[1L]], 44L)
        expect_identical(
            head(object[[1L]]),
            c("CADM1", "CD1D", "CD2", "CD24", "CD276", "CD28")
        )
    }
})

test_that("JSON/YAML", {
    for (ext in c("json", "yml")) {
        file <- file.path(cacheDir, paste0("example.", ext))
        object <- import(file)
        expect_type(object, "list")
    }
})

test_that("'rio::import()', e.g. Stata DTA file", {
    skip_if_not_installed(pkg = "haven")
    skip_if_not_installed(pkg = "rio")
    file <- system.file("examples/iris.dta", package = "haven")
    x <- import(file)
    expect_s3_class(x, "data.frame")
    expect_identical(
        colnames(x),
        c("sepallength", "sepalwidth", "petallength", "petalwidth", "species")
    )
})

test_that("GAF", {
    file <- file.path(cacheDir, "example.gaf.gz")
    x <- import(file)
    expect_s4_class(x, "TidySet")
    expect_length(x, 880L)
})

test_that("OBO", {
    skip_if_not_installed(pkg = "ontologyIndex")
    file <- file.path(cacheDir, "example.obo")
    x <- import(file)
    expect_s3_class(x, "ontology_index")
    expect_length(x, 25L)
})

test_that("PZFX", {
    skip_if_not_installed(pkg = "pzfx")
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

test_that("XLSX", {
    skip_if_not_installed(pkg = "readxl")
    skip_on_os("windows")
    file <- file.path(cacheDir, "example.xlsx")
    expect_identical(
        object = import(
            con = file,
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
    expect_true(hasRownames(import(con = file, rownameCol = "rowname")))
})

test_that("XLS", {
    file <- file.path(cacheDir, "example.xls")
    skip_on_os("windows")
    expect_identical(
        object = import(
            con = file,
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
    expect_true(hasRownames(import(con = file, rownameCol = "rowname")))
})

test_that("bcbio counts", {
    object <- import(
        con = file.path(cacheDir, "example.counts"),
        metadata = TRUE
    )
    expect_type(object, "integer")
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

test_that("FASTA", {
    file <- file.path(cacheDir, "example.fa.gz")
    object <- import(
        con = file,
        moleculeType = "DNA",
        metadata = TRUE
    )
    expect_s4_class(object, "DNAStringSet")
    expect_identical(
        object = names(object)[[1L]],
        expected = "ENST00000456328.2"
    )
    expect_length(object[[1L]], 1657L)
    expect_s4_class(metadata(object)[["attributes"]], "SimpleList")
    expect_named(
        object = object,
        expected = names(metadata(object)[["attributes"]])
    )
})

test_that("FASTQ", {
    file <- file.path(cacheDir, "example.fq.gz")
    object <- import(
        con = file,
        moleculeType = "DNA",
        metadata = TRUE
    )
    expect_s4_class(object, "DNAStringSet")
    expect_identical(
        object = names(object)[[1L]],
        expected = "NS500233:572:H25VKBGX2:1:11101:16195:1041 3:N:0:1"
    )
    expect_length(object[[1L]], 8L)
})

test_that("Google Sheets", {
    url <- pasteUrl(
        "docs.google.com",
        "spreadsheets",
        "d",
        "1U6Cf_qEOhiR9AZqTqS3mbMF3zt2db48ZP5v3rkrAEJY",
        "edit#gid=780868077",
        protocol = "https"
    )
    object <- import(url)
    expect_s3_class(object, "data.frame")
})

test_that("MAF", {
    file <- file.path(cacheDir, "example.maf")
    object <- import(file)
    expect_s4_class(object, "MAF")
    expect_identical(
        object = dim(object@data),
        expected = c(64L, 12L)
    )
})

test_that("BAM/SAM (and CRAM)", {
    skip_on_os("windows")
    files <- file.path(cacheDir, c("example.bam", "example.sam"))
    for (file in files) {
        object <- import(file)
        expect_type(object, "list")
        expect_named(
            object = object,
            expected = c(
                "qname",
                "flag",
                "rname",
                "strand",
                "pos",
                "qwidth",
                "mapq",
                "cigar",
                "mrnm",
                "mpos",
                "isize",
                "seq",
                "qual"
            )
        )
    }
})

test_that("BCF/VCF", {
    skip_on_os("windows")
    files <- file.path(cacheDir, c("example.bcf.gz", "example.vcf.gz"))
    for (file in files) {
        object <- import(file)
        expect_type(object, "list")
        expect_named(
            object = object,
            expected = c(
                "CHROM",
                "POS",
                "ID",
                "REF",
                "ALT",
                "QUAL",
                "FILTER",
                "INFO",
                "FORMAT",
                "GENO",
                "RecordsPerRange"
            )
        )
    }
})

test_that("textConnection : CSV", {
    object <- c("\"a\",\"b\"", "\"c\",\"d\"", "\"e\",\"f\"")
    con <- textConnection(object)
    expect_identical(
        object = import(
            con = con,
            format = "csv",
            colnames = TRUE
        ),
        expected = data.frame(
            "a" = c("c", "e"),
            "b" = c("d", "f")
        )
    )
    close(con)
    con <- textConnection(object)
    expect_identical(
        object = import(
            con = con,
            format = "csv",
            colnames = FALSE
        ),
        expected = data.frame(
            "V1" = c("a", "c", "e"),
            "V2" = c("b", "d", "f")
        )
    )
    close(con)
})

test_that("textConnection : TSV", {
    object <- c("a\tb", "c\td", "e\tf")
    con <- textConnection(object)
    expect_identical(
        object = import(
            con = con,
            format = "tsv",
            colnames = TRUE
        ),
        expected = data.frame(
            "a" = c("c", "e"),
            "b" = c("d", "f")
        )
    )
    close(con)
    con <- textConnection(object)
    expect_identical(
        object = import(
            con = con,
            format = "tsv",
            colnames = FALSE
        ),
        expected = data.frame(
            "V1" = c("a", "c", "e"),
            "V2" = c("b", "d", "f")
        )
    )
    close(con)
})

test_that("textConnection : JSON", {
    file <- file.path(cacheDir, "example.json")
    lines <- import(file, format = "lines")
    con <- textConnection(lines)
    x <- import(file)
    y <- import(con, format = "json")
    expect_identical(x, y)
    close(con)
})

test_that("textConnection : YAML", {
    file <- file.path(cacheDir, "example.yml")
    lines <- import(file, format = "lines")
    con <- textConnection(lines)
    x <- import(file)
    y <- import(con, format = "yaml")
    expect_identical(x, y)
    close(con)
})
