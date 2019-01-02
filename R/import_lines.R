# Import source code lines.
import_lines <- function(file, ...) {
    message(paste(
        "Importing", basename(file), "using readr::read_lines()."
    ))
    read_lines(file, ...)
}
