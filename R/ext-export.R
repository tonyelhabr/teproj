
.print_export_msg <- function(filepath) {
  if (getOption("teproj.print_msg"))
    message("Saved ", basename(filepath), " as ", filepath, ".")
}

.get_filepath_backup <-
  function(filename,
           dir,
           ext,
           filepath_backup,
           backup) {
    if (is.null(filepath_backup)) {
      filepath_backup <- paste0(dir,
                                filename,
                                "-",
                                strftime(Sys.time(), "%Y-%m-%d:%H-%M-%S"),
                                ".",
                                ext)
    }
    invisible(filepath_backup)
  }

.export_backup <- function(
                           filename,
                           dir,
                           ext,
                           filepath,
                           filepath_backup,
                           backup,
                           overwrite) {
  if (!backup) {
    .print_argfalse_msg("backup")
    return(invisible())
  }

  filepath_backup <-
    .get_filepath_backup(
                         filename,
                         dir,
                         ext,
                         filepath_backup,
                         backup,
                         pkg_print_opts)
  if (file.exists(filepath_backup) & !overwrite) {
    .print_argfalse_msg("overwrite")
    return(invisible())
  }

  file.copy(from = filepath, to = filepath_backup)
  .print_export_msg(filepath_backup)
  invisible(filepath_backup)
}

#' @title Save to a filepath.
#' @description Saves an object to a filepath.
#' @details Object to save must be a data.frame (or matrix) for most formats.
#' @inheritParams create_dir
#' @param x Data.frame (or matrix) for most formats. Default: NULL.
#' @param filename Character. Bare filename (i.e. without folderor extension), Default: deparse(substitute(x)).
#' @param ext Character. Bare extension (i.e. without a dot). Must be one of valid formats. Default: NULL.
#' @param filepath Character. Concatenation of `filename`, `dir`, and `ext`, Default: paste0(dir, filename, ".", ext).
#' @param overwrite Boolean. Default: TRUE.
#' @param backup Boolean. Default: FALSE.
#' @param filepath_backup like `filepath`, Default: Same as `filepath`, but with timestamp appended.
#' @param export Boolean. Indicates whether to actually carry out action. Intended to be used as a "catch all". Default: TRUE.
#' @param ... Dots. May (or may not) be passed on to specific exporting functions.
#' @return Character. Filepath.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # These may be defined elsewhere in a project.
#'  dir_csv <- "csvs/"
#'  export_csv <- TRUE
#'  # ...
#'  export_ext(iris, ext = "csv", dir = dir_csv, export = export_csv)
#'  # Or...
#'  export_ext_csv(iris, dir = dir_csv, export = export_csv)
#'  }
#' }
#' @export
#' @rdname export_ext
#' @importFrom rio export
export_ext <-
  function(x = NULL,
           filename = deparse(substitute(x)),
           dir = paste0(getwd(), "/"),
           ext = NULL,
           filepath = paste0(dir, filename, ".", ext),
           overwrite = TRUE,
           backup = FALSE,
           filepath_backup = NULL,
           export = TRUE,
           ...) {
    # browser()
    if (!export) {
      .print_argfalse_msg("export")
      return(invisible())
    }

    filepath <-
      .get_filepath(filename, dir, ext, filepath)
    if (file.exists(filepath) & overwrite == FALSE) {
      .print_argfalse_msg("overwrite")
      return(invisible())
    }

    create_dir(dir, overwrite, export)

    rio::export(filepath, x, ...)
    .print_export_msg(filepath)
    .export_backup(
                   filename,
                   dir,
                   ext,
                   filepath,
                   filepath_backup,
                   backup,
                   overwrite)
    invisible(filepath)
  }

#' @export
#' @rdname export_ext
export_ext_csv <- function(...)
  export_ext(ext = "csv", ...)

#' @export
#' @rdname export_ext
export_ext_xlsx <- function(...)
  export_ext(ext = "xlsx", ...)

#' @export
#' @rdname export_ext
export_excel <- export_ext_xlsx

#' @export
#' @rdname export_ext
export_ext_rda <- function(...)
  export_ext(ext = "rda", ...)

#' @export
#' @rdname export_ext
export_ext_rdata <- function(...)
  export_ext(ext = "RData", ...)

#' @export
#' @rdname export_ext
export_ext_RData <- export_ext_rdata

#' @export
#' @rdname export_ext
export_ext_rds <- function(...)
  export_ext(ext = "rds", ...)

#' @export
#' @rdname export_ext
export_ext_feather <- function(...)
  export_ext(ext = "feather", ...)


#' @export
#' @rdname export_ext
export_ext_png <- function(...)
  export_ext(ext = "png", ...)

#' @export
#' @rdname export_ext
export_fig <- export_ext_png

#' @export
#' @rdname export_ext
export_viz <- export_ext_png
# export_viz <- function(...) {
#   pkg_print_opts <- get_pkg_print_opts()
#   .print_dpc_msg("export_ext_png")
#   export_ext(ext = "png", ...)
# }
