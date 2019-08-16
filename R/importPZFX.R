#' @describeIn import Import a GraphPad Prism file.
#' Note that this function doesn't support optional column names.
#' @export
importPZFX <- function(file, sheet = 1L) {
    file <- localOrRemoteFile(file)
    message(sprintf(
        "Importing '%s' using '%s()'.",
        basename(file), "pzfx::read_pzfx"
    ))
    requireNamespace("pzfx", quietly = TRUE)
    object <- pzfx::read_pzfx(
        path = file,
        table = sheet
    )
    object <- .slotMetadata(object, pkg = "pzfx", fun = "read_pzfx")
    object
}
