dir.create("cache", showWarnings = FALSE)
files <- c(
    "example.counts",
    "example.csv",
    "example.csv.gz",
    "example.csv.zip",
    "example.gtf",
    "example.gff3",
    "example.json",
    "example.R",
    "example.rda",
    "example.rds",
    "example.tsv",
    "example.txt",
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
    "single_cell_counts.mtx.gz.rownames"
)
mapply(
    FUN = function(remoteDir, file, envir) {
        destfile <- file.path("cache", file)
        if (!file.exists(destfile)) {
            utils::download.file(
                url = paste(remoteDir, file, sep = "/"),
                destfile = destfile
            )
        }
    },
    file = files,
    MoreArgs = list(
        remoteDir = brioTestsURL,
        envir = environment()
    )
)
