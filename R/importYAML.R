## Updated 2019-07-30.
importYAML <- function(file) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "yaml::yaml.load_file"
    ))
    requireNamespace("yaml", quietly = TRUE)
    object <- yaml::yaml.load_file(input = file)
    object <- .slotMetadata(object, pkg = "yaml", fun = "yaml.load_file")
    object
}
