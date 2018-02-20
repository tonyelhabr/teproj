

#' @title Create a directory.
#' @description Creates the directory if it does not exist.
#' @details Used by other functions in this package.
#' @param dir character. Folder name (with a trailing slash).
#' @param overwrite boolean.
#' @param backup boolean.
#' @param create boolean.
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
    if (getOption("teproj.print.msg"))
      message(dir, " already exists.")
    if(overwrite) {
      # unlink(list.files(dir, full.names = TRUE), recursive = TRUE, force = TRUE)
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      if(getOption("teproj.print.msg"))
        message("Overwrote ", dir, " (because `overwrite == TRUE`).")
    }
  } else {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (getOption("teproj.print.msg")) {
      message("Created ", dir, ".")
    }
  }
  invisible(dir)
}

