

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
#' @param filepath_backup like `filepath`, Default: Same as `filepath`, but with timestamp appended
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
#'  export_ext_csv(iris, dir = dir_csv, export = export_csv)
#'  }
#' }
#' @export
#' @rdname export_ext
#' @importFrom ggplot2 ggsave
#' @importFrom readr write_csv
#' @importFrom openxlsx write.xlsx
#' @importFrom utils capture.output
#' @importFrom session save.session
#' @importFrom feather write_feather
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
      .print_argfalse_msg("export", pkg_print_opts)
      return()
    }

    filepath <-
      .get_filepath(dir, filename, ext, filepath, pkg_print_opts)
    if (file.exists(filepath) & overwrite == FALSE) {
      .print_argfalse_msg("overwrite")
      return()
    }

    create_dir(dir, overwrite, export)

    rio::export(filepath, x, ...)
    .print_export_msg(filepath)
    .export_backup(dir,
                 filename,
                 ext,
                 filepath,
                 filepath_backup,
                 backup,
                 overwrite)
    invisible(filepath)
  }

#' @export
#' @rdname export_ext
export_ext_csv <- function(...) export_ext(ext = "csv", ...)

#' @export
#' @rdname export_ext
export_ext_xlsx <- function(...) export_ext(ext = "xlsx", ...)

#' @export
#' @rdname export_ext
export_excel <- export_ext_xlsx

#' @export
#' @rdname export_ext
export_ext_rda <- function(...) export_ext(ext = "rda", ...)

#' @export
#' @rdname export_ext
export_ext_rdata <- function(...) export_ext(ext = "RData", ...)

#' @export
#' @rdname export_ext
export_ext_RData <- export_ext_rdata

#' @export
#' @rdname export_ext
export_ext_rds <- function(...) export_ext(ext = "rds", ...)

#' @export
#' @rdname export_ext
export_ext_feather <- function(...) export_ext(ext = "feather", ...)


#' @export
#' @rdname export_ext
export_ext_png <- function(...) export_ext(ext = "png", ...)

#' @export
#' @rdname export_ext
export_fig <- export_ext_png

#' @export
#' @rdname export_ext
export_viz <- export_ext_png
# export_viz <- function(...) {
#   pkg_print_opts <- get_pkg_print_opts()
#   .print_dpc_msg("export_ext_png", pkg_print_opts)
#   export_ext(ext = "png", ...)
# }

