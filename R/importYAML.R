importYAML <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using yaml::yaml.load_file()."
    ))
    requireNamespace("yaml", quietly = TRUE)
    yaml::yaml.load_file(input = file, ...)
}
