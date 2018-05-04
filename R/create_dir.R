


#' Create a directory
#'
#' @description Creates the directory if it does not exist.
#' @details Used by other functions in this package.
#' Note that this function is probably more complex than it really should be.
#' Re-factoring it should be considered.
#' @param dir character. Folder name (with a trailing slash).
#' @param overwrite logical.
#' @param backup logical.
#' @param create logical.
#' @param ... dots. Not currently used.
#' @return character. Filepath.
#' @export
create_dir <- function(dir = paste0(getwd(), "/"),
                       overwrite = FALSE,
                       backup = TRUE,
                       create = TRUE,
                       ...) {
  if (!create) {
    print_argfalse_msg("create")
    return(invisible())
  }

  if (is.null(dir)) {
    print_isnull_msg()
    return(invisible())
  }

  # Check for the existence of a drive.
  if (!grepl(":", dir)) {
    # message("Appending the drive of the current working directory.")
    dir <- paste0(getwd(), "/", dir)
  }

  if (substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }

  if (dir.exists(dir)) {
    # message(sprintf("%s already exists.", dir))
    if (overwrite) {
      # unlink(list.files(dir, full.names = TRUE), recursive = TRUE, force = TRUE)
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      message(sprintf("Overwriting %s (because `overwrite == TRUE`)."),
              dir)
    }
  } else {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message(sprintf("Creating %s.", dir))
  }
  invisible(dir)
}
