# Extracted from test-cacheUrl.R:48

# test -------------------------------------------------------------------------
url1 <- pasteUrl(
        "ftp.ensembl.org",
        "pub",
        "release-87",
        "fasta",
        "homo_sapiens",
        "cdna",
        "Homo_sapiens.GRCh38.cdna.all.fa.gz",
        protocol = "ftp"
    )
url2 <- pasteUrl(
        "ftp.ensembl.org",
        "pub",
        "release-102",
        "fasta",
        "homo_sapiens",
        "cdna",
        "Homo_sapiens.GRCh38.cdna.all.fa.gz",
        protocol = "ftp"
    )
cacheFile1 <- cacheUrl(url1)
