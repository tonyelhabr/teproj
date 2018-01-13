
# Because this is used in more than one place...
.print_export_msg <- function(filepath) {
  if (getOption("teproj.print.msg"))
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

.create_backup <- function(filename,
                           dir,
                           ext,
                           filepath,
                           filepath_backup,
                           backup,
                           overwrite) {
  if (!backup) {
    # .print_argfalse_msg("backup")
    return(invisible())
  }

  filepath_backup <-
    .get_filepath_backup(filename,
                         dir,
                         ext,
                         filepath_backup,
                         backup)
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
#' @param x data.frame (or matrix) for most formats.
#' @param filename character. Bare filename (i.e. without folderor extension),
#' @param ext character. Bare extension (i.e. without a dot). Must be one of valid formats.
#' @param filepath character. Concatenation of `filename`, `dir`, and \code{ext},
#' @param overwrite boolean.
#' @param backup boolean.
#' @param filepath_backup like \code{filepath},
#' @param export boolean. Indicates whether to actually carry out action. Intended to be used as a "catch all".
#' @param return boolean.  Relevant ONLY if \code{export == FALSE}.
#' Set to \code{TRUE} in order to preview what would be rendered.
#' @return character. Filepath.
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
#' @importFrom ggplot2 ggsave
#' @importFrom session save.session
#' @importFrom utils capture.output
export_ext <-
  function(x = NULL,
           filename = deparse(substitute(x)),
           dir = getwd(),
           ext = NULL,
           filepath = file.path(dir, paste0(filename, ".", ext)),
           overwrite = TRUE,
           backup = FALSE,
           filepath_backup = NULL,
           export = TRUE,
           return = TRUE,
           ...) {
    # browser()
    if (!export & !return) {
      .print_argfalse_msg("export")
      return(invisible())
    }

    if(is.null(x) & is.null(ext)) {
      .print_isnull_msg()
      return(invisible())
    }

    filepath <-
      .get_filepath(filename, dir, ext, filepath)

    if(!export & return) {
      return(invisible(filepath))
    }

    if (file.exists(filepath) & overwrite == FALSE) {
      .print_argfalse_msg("overwrite")
      return(invisible())
    }

    # Don't overwrite the directory, even if overwrite == TRUE for this function.
    create_dir(dir, overwrite = FALSE, backup = backup)

    if (ext %in% c("png")) {
      units <- getOption("teproj.ggsave.units")
      width <- getOption("teproj.ggsave.width")
      height <- getOption("teproj.ggsave.height")
      ggplot2::ggsave(
        filename = filepath,
        units = units,
        width = width,
        height = height,
        ...
      )
      .print_nonreadr_msg("ggplot2")
    } else if (grepl("rda", tolower(ext))) {
      # x <- ls(parent.frame())
      # filepath <- gsub(ext, "rdata", filepath)
      # rio::export(x, filepath, ...)
      suppressWarnings(utils::capture.output(session::save.session(filepath)))
    } else {

      out <- try({
        fun_readr <- paste0("write_", ext)
        .do_call(fun_readr, list(x = x, path = filepath, ...))
      }, silent = TRUE)

      if(inherits(out, "try-error")) {
        out <- rio::export(x, filepath, ...)
        if(!inherits(out, "try-error")) {
          .print_nonreadr_msg("rio")
        }
      }

    }

    .print_export_msg(filepath)
    .create_backup(filename,
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
export_ext_rdata <- function(...)
  export_ext(ext = "RData", ...)

#' @export
#' @rdname export_ext
export_ext_RData <- export_ext_rdata

#' @export
#' @rdname export_ext
export_ext_rda <- export_ext_rdata

#' @export
#' @rdname export_ext
export_ext_rds <- function(...)
  export_ext(ext = "rds", ...)

#' @export
#' @rdname export_ext
export_ext_png <- function(...)
  export_ext(ext = "png", ...)

#' @export
#' @rdname export_ext
# export_fig <- export_ext_png
# Or...
export_fig <- function(...) {
  .print_dpc_msg("export_ext_png")
  export_ext(ext = "png", ...)
}

#' @export
#' @rdname export_ext
# export_viz <- export_ext_png
# Or...
export_viz <- function(...) {
  .print_dpc_msg("export_ext_png")
  export_ext(ext = "png", ...)
}
