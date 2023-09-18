test_that("NCBI FTP", {
    url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/"
    skip_if_not(isAnExistingURL(url))
    expect_identical(
        object = with_collate(
            new = "C",
            code = {
                getURLDirList(url, type = "all")
            }
        ),
        expected = c(
            "ASSEMBLY_REPORTS",
            "CLUSTERS",
            "GENOME_REPORTS",
            "HUMAN_MICROBIOM",
            "INFLUENZA",
            "MapView",
            "README.txt",
            "README_GFF3.txt",
            "README_assembly_summary.txt",
            "README_change_notice.txt",
            "TARGET",
            "TOOLS",
            "Viruses",
            "all",
            "archive",
            "check.txt",
            "genbank",
            "refseq",
            "species.diff.txt"
        )
    )
    expect_identical(
        object = with_collate(
            new = "C",
            code = {
                getURLDirList(url, type = "dirs")
            }
        ),
        expected = c(
            "ASSEMBLY_REPORTS",
            "CLUSTERS",
            "GENOME_REPORTS",
            "HUMAN_MICROBIOM",
            "INFLUENZA",
            "MapView",
            "TARGET",
            "TOOLS",
            "Viruses",
            "all",
            "archive"
        )
    )
    expect_identical(
        object = with_collate(
            new = "C",
            code = {
                getURLDirList(url, type = "files")
            }
        ),
        expected = c(
            "README.txt",
            "README_GFF3.txt",
            "README_assembly_summary.txt",
            "README_change_notice.txt",
            "check.txt",
            "genbank",
            "refseq",
            "species.diff.txt"
        )
    )
    expect_identical(
        object = getURLDirList(
            url = url,
            pattern = "^refseq$",
            absolute = TRUE
        ),
        expected = paste0(url, "refseq")
    )
    expect_identical(
        object = getURLDirList(url = url, pattern = "^refseq$"),
        expected = "refseq"
    )
    expect_error(
        object = getURLDirList(
            url = url,
            pattern = "^refseq/$",
            absolute = TRUE
        ),
        regexp = "No files matched pattern"
    )
})

test_that("NCBI HTTPS", {
    url <- "https://ftp.ncbi.nlm.nih.gov/genomes/"
    skip_if_not(isAnExistingURL(url))
    expect_identical(
        object = with_collate(
            new = "C",
            code = {
                getURLDirList(url, type = "all")
            }
        ),
        expected = c(
            "ASSEMBLY_REPORTS",
            "CLUSTERS",
            "GENOME_REPORTS",
            "HUMAN_MICROBIOM",
            "INFLUENZA",
            "MapView",
            "README.txt",
            "README_GFF3.txt",
            "README_assembly_summary.txt",
            "README_change_notice.txt",
            "TARGET",
            "TOOLS",
            "Viruses",
            "all",
            "archive",
            "check.txt",
            "genbank",
            "refseq",
            "species.diff.txt"
        )
    )
    expect_identical(
        object = with_collate(
            new = "C",
            code = {
                getURLDirList(url, type = "dirs")
            }
        ),
        expected = c(
            "ASSEMBLY_REPORTS",
            "CLUSTERS",
            "GENOME_REPORTS",
            "HUMAN_MICROBIOM",
            "INFLUENZA",
            "MapView",
            "TARGET",
            "TOOLS",
            "Viruses",
            "all",
            "archive",
            "genbank",
            "refseq"
        )
    )
    expect_identical(
        object = with_collate(
            new = "C",
            code = {
                getURLDirList(url, type = "files")
            }
        ),
        expected = c(
            "README.txt",
            "README_GFF3.txt",
            "README_assembly_summary.txt",
            "README_change_notice.txt",
            "check.txt",
            "species.diff.txt"
        )
    )
    expect_identical(
        object = getURLDirList(
            url = url,
            pattern = "^refseq$",
            absolute = TRUE
        ),
        expected = paste0(url, "refseq")
    )
    expect_identical(
        object = getURLDirList(url = url, pattern = "^refseq$"),
        expected = "refseq"
    )
    expect_error(
        object = getURLDirList(
            url = url,
            pattern = "^refseq/$",
            absolute = TRUE
        ),
        regexp = "No files matched pattern"
    )
})

## This is prone to timeouts and can fail.
test_that("Ensembl FTP", {
    url <- "ftp://ftp.ensembl.org/pub/release-110/mysql/"
    skip_if_not(isAnExistingURL(url))
    expect_identical(
        object = with_collate(
            new = "C",
            code = {
                getURLDirList(url)
            }
        ),
        expected = c(
            "ASSEMBLY_REPORTS",
            "CLUSTERS",
            "GENOME_REPORTS",
            "HUMAN_MICROBIOM",
            "INFLUENZA",
            "MapView",
            "README.txt",
            "README_GFF3.txt",
            "README_assembly_summary.txt",
            "README_change_notice.txt",
            "TARGET",
            "TOOLS",
            "Viruses",
            "all",
            "archive",
            "check.txt",
            "genbank",
            "refseq",
            "species.diff.txt"
        )
    )
})

test_that("Ensembl HTTPS", {
    url <- "https://ftp.ensembl.org/pub/release-110/mysql/"
    skip_if_not(isAnExistingURL(url))
    expect_identical(
        object = with_collate(
            new = "C",
            code = {
                getURLDirList(url)
            }
        ),
        expected = c(
            "ASSEMBLY_REPORTS",
            "CLUSTERS",
            "GENOME_REPORTS",
            "HUMAN_MICROBIOM",
            "INFLUENZA",
            "MapView",
            "README.txt",
            "README_GFF3.txt",
            "README_assembly_summary.txt",
            "README_change_notice.txt",
            "TARGET",
            "TOOLS",
            "Viruses",
            "all",
            "archive",
            "check.txt",
            "genbank",
            "refseq",
            "species.diff.txt"
        )
    )
})
