test_that("NCBI FTP", {
    url <- pasteURL(
        "ftp.ncbi.nlm.nih.gov", "genomes",
        protocol = "ftp"
    )
    skip_if_not(isAnExistingURL(paste0(url, "/")))
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
        expected = pasteURL(url, "refseq")
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
    url <- pasteURL(
        "ftp.ncbi.nlm.nih.gov", "genomes",
        protocol = "https"
    )
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
        expected = pasteURL(url, "refseq")
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

## Ensembl server listing is prone to timeouts over FTP protocol, so just
## checking HTTPS protocol here for time being.

test_that("Ensembl HTTPS", {
    url <- pasteURL(
        "ftp.ensembl.org",
        "pub",
        "release-110",
        "fasta",
        "homo_sapiens",
        "cdna",
        protocol = "https"
    )
    skip_if_not(isAnExistingURL(url))
    expect_identical(
        object = with_collate(
            new = "C",
            code = {
                getURLDirList(url)
            }
        ),
        expected = c(
            "CHECKSUMS",
            "Homo_sapiens.GRCh38.cdna.abinitio.fa.gz",
            "Homo_sapiens.GRCh38.cdna.all.fa.gz",
            "README"
        )
    )
})
