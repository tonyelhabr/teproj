
#' @title Control `tetuils` messages.
#' @description Sets print message parameters for `teutils` functions.
#' @details The `set_` version is intended to be used by the user
#' to set the options (as a wrapper to `options(...`), while the
#' `get_` version is intended for use only internally.
#' @param msg Boolean. Default: FALSE (getOption("teutils.print_msg")).
#' @param wrn Boolean. Default: TRUE (getOption("teutils.print_wrn")).
#' @param err Boolean. Default: TRUE (getOption("teutils.print_err"))
#' @export
set_pkg_print_opts <- function(msg = getOption("teutils.print_msg"),
                               wrn = getOption("teutils.print_wrn"),
                               err = getOption("teutils.print_err")) {
  options(teutils.print_msg = msg)
  options(teutils.print_wrn = wrn)
  options(teutils.print_err = err)
}

#' @title Create a directory.
#' @description Creates the directory if it does not exist.
#' @details Used by other functions in this package.
#' @param dir Character. Folder name (with a trailing slash), Default: paste0(getwd(), "/").
#' @param overwrite Boolean. Default: FALSE.
#' @param create Boolean. Intended to be used with a global `keep_*` parameter. Default: TRUE.
#' @param pkg_print_opts List with print settings. Default: get_pkg_print_opts().
#' @param ... Dots. Additional parameters to pass to `dir.create()`.
#' @return Character. Filepath.
#' @export
create_dir <- function(dir = paste0(getwd(), "/"),
                       overwrite = FALSE,
                       create = TRUE,
                       ...) {
  if (!create) {
    .print_argfalse_msg("create", pkg_print_opts)
    return()
  }

  if (is.null(dir)) {
    .print_isnull_msg(pkg_print_opts)
    return()
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
    if (pkg_print_opts$msg)
      message(dir, " already exists.")
    if(overwrite == TRUE)
      unlink(list.files(dir, full.names = TRUE), recursive = TRUE, force = TRUE)
    if(pkg_print_opts$msg)
      message("Overwrote ", dir, " (because `overwrite == TRUE`).")
  } else {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (pkg_print_opts$msg) {
      message("Created ", dir, ".")
    }
  }
  invisible(dir)
}
# NOTE
# These functions may not use each of the print_* booleans.
# Nonetheless, they may be implemented in the future.
.get_filepath <-
  function(dir,
           filename,
           ext,
           filepath) {
    if (is.null(filepath)) {
      filepath <- paste0(dir, filename, ".", ext)
    }
    filepath
  }

.convert_to_df <- function(xs) {
  if (is.null(x)) {
    x <- NULL
    return(x)
  }
  if (!is.data.frame(x) & !is.matrix(x)) {
    x <- NULL
    if (pkg_print_opts$wrn)
      warning("`x` should be a data.frame or matrix.")
    return(x)
  } else if (is.matrix(x)) {
    if (pkg_print_opts$msg)
      message("Coercing `x` from a matrix to a data.frame.")
    x <- as.data.frame(x)
  }
  x
}


.get_filepath_backup <-
  function(dir,
           filename,
           ext,
           filepath_backup,
           backup) {
    # This seemingly repetitive clause allows this function to be called
    # elsewhere from .export_backup (which also does this check).
    # if (!backup) {
    #   .print_argfalse_msg("backup", pkg_print_opts)
    #   return(NULL)
    # }
    if (is.null(filepath_backup)) {
      filepath_backup <- paste0(dir,
                                filename,
                                "-",
                                strftime(Sys.time(), "%Y-%m-%d:%H-%M-%S"),
                                ".",
                                ext)
    }
    filepath_backup
  }

.export_backup <- function(dir,
                         filename,
                         ext,
                         filepath,
                         filepath_backup,
                         backup,
                         overwrite,
                         pkg_print_opts) {
  if (!backup) {
    .print_argfalse_msg("backup", pkg_print_opts)
    return()
  }

  filepath_backup <-
    .get_filepath_backup(dir,
                         filename,
                         ext,
                         filepath_backup,
                         backup,
                         pkg_print_opts)
  if (file.exists(filepath_backup) & !overwrite) {
    .print_argfalse_msg("overwrite", pkg_print_opts)
    return()
  }

  file.copy(from = filepath, to = filepath_backup)
  .print_export_msg(filepath_backup, pkg_print_opts)
}

