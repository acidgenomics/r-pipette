# Import source code lines.
importLines <- function(file, ...) {
    file <- localOrRemoteFile(file)
    message(paste(
        "Importing", basename(file), "using readr::read_lines()."
    ))
    read_lines(file, ...)
}
