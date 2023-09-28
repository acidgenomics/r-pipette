if (isFALSE(goalie::hasInternet())) {
    warning("No Internet connection detected.")
    return(invisible(NULL))
}
cacheDir <- file.path(
    tools::R_user_dir(package = .pkgName, which = cacheDir),
    "testthat"
)
dir.create(cacheDir, showWarnings = FALSE)
files <- c(
    "example.counts",
    "example.csv",
    "example.csv.gz",
    "example.csv.zip",
    ## Subset of transcripts from `gencode.v38.transcripts.fa.gz` file.
    "example.fa.gz",
    ## Klein Lab inDrops R3 FASTQ file (from bcbio-nextgen unit tests).
    "example.fq.gz",
    ## https://software.broadinstitute.org/software/igv/GCT
    "example.gct",
    "example.gtf",
    "example.gff3",
    "example.json",
    # DepMap example.
    "example.maf",
    ## Example gene ontology OBO file from BiocSet package.
    "example.obo",
    "example.R",
    "example.rda",
    "example.rds",
    "example.tsv",
    "example.txt",
    "example.txt.bz2",
    "example.txt.gz",
    "example.txt.xz",
    "example.txt.zip",
    "example.xls",
    "example.xlsx",
    "example.yml",
    "geneset.gmt",
    "geneset.gmx",
    "geneset.grp",
    "gr.rda",
    "h.all.v6.2.entrez.gmt",
    "h.all.v6.2.symbols.gmt",
    "multi.rda",
    "renamed.rda",
    "rnaseq_counts.csv.gz",
    "serialized.rds",
    "single_cell_counts.mtx.gz",
    "single_cell_counts.mtx.gz.colnames",
    "single_cell_counts.mtx.gz.rownames",
    ## Rsamtools extdata files.
    "example.bam",
    "example.bam.bai",
    "example.bcf.gz",
    "example.bcf.gz.csi",
    "example.sam",
    "example.vcf.gz",
    "example.vcf.gz.csi"
)
invisible(Map(
    f = function(remoteDir, file, envir) {
        destfile <- file.path(cacheDir, file)
        if (!file.exists(destfile)) {
            utils::download.file(
                url = paste(remoteDir, file, sep = "/"),
                destfile = destfile
            )
        }
    },
    file = files,
    MoreArgs = list(
        "envir" = environment(),
        "remoteDir" = pipetteTestsUrl
    )
))
rm(files)
