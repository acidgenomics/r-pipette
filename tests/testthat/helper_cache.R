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
    "example.xlsx",
    "example.yml",
    "gr.rda",
    "multi.rda",
    "renamed.rda",
    "rnaseq_counts.csv.gz",
    "serialized.rds",
    "single_cell_counts.mtx.gz",
    "single_cell_counts.mtx.gz.colnames",
    "single_cell_counts.mtx.gz.rownames"
)
mapply(
    FUN = function(cacheURL, file, envir) {
        if (!file.exists(file)) {
            utils::download.file(
                url = paste(cacheURL, file, sep = "/"),
                destfile = file
            )
        }
    },
    file = files,
    MoreArgs = list(
        cacheURL = brioCacheURL,
        envir = environment()
    )
)
