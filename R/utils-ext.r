
#' @title Control package messages.
#' @description Sets print message parameters for `teproj` functions.
#' @details Intended to be used as a wrapper to `options(...`).
#' @param msg Boolean. Default: FALSE (getOption("teproj.print_msg")).
#' @param wrn Boolean. Default: TRUE (getOption("teproj.print_wrn")).
#' @param err Boolean. Default: TRUE (getOption("teproj.print_err"))
#' @export
set_pkg_print_opts <- function(msg = getOption("teproj.print_msg"),
                               wrn = getOption("teproj.print_wrn"),
                               err = getOption("teproj.print_err")) {
  options(teproj.print_msg = msg)
  options(teproj.print_wrn = wrn)
  options(teproj.print_err = err)
}

#' @title Create a directory.
#' @description Creates the directory if it does not exist.
#' @details Used by other functions in this package.
#' @param dir Character. Folder name (with a trailing slash), Default: paste0(getwd(), "/").
#' @param overwrite Boolean. Default: FALSE.
#' @param backup Boolean. Only relevant if `dir` \code{dir} exists and `overwrite == TRUE` \code{overwrite == TRUE}. Default: TRUE.
#' @param create Boolean. Intended to be used with a global `keep_*` parameter. Default: TRUE.
#' @param ... Dots. Additional parameters to pass to `dir.create()`.
#' @return Character. Filepath.
#' @export
create_dir <- function(dir = paste0(getwd(), "/"),
                       overwrite = FALSE,
                       backup = TRUE,
                       create = TRUE,
                       ...) {
  if (!create) {
    .print_argfalse_msg("create")
    return(invisible())
  }

  if (is.null(dir)) {
    .print_isnull_msg(pkg_print_opts)
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
    if (getOption("teproj.print_msg"))
      message(dir, " already exists.")
    if(overwrite) {
      # unlink(list.files(dir, full.names = TRUE), recursive = TRUE, force = TRUE)
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      if(getOption("teproj.print_msg"))
        message("Overwrote ", dir, " (because `overwrite == TRUE`).")
    }
  } else {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (getOption("teproj.print_msg")) {
      message("Created ", dir, ".")
    }
  }
  invisible(dir)
}


.get_filepath <-
  function(filename,
           dir,
           ext,
           filepath) {
    if (is.null(filepath)) {
      filepath <- paste0(dir, filename, ".", ext)
    }
    filepath
  }

