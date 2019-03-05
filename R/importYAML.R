importYAML <- function(file, ...) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using yaml::yaml.load_file()."))
    requireNamespace("yaml", quietly = TRUE)
    object <- yaml::yaml.load_file(input = file, ...)
    object <- .slotMetadata(object, pkg = "yaml", fun = "yaml.load_file")
    object
}
