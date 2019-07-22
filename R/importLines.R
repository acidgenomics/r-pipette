## Import source code lines.
importLines <- function(file) {
    file <- localOrRemoteFile(file)
    message(paste("Importing", basename(file), "using readr::read_lines()."))
    object <- read_lines(file = file)
    object <- .slotMetadata(object, pkg = "readr", fun = "read_lines")
    object
}
