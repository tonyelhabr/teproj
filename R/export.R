



get_path_backup_safely <-
  function(path = NULL,
           path_backup = NULL,
           backup = FALSE) {
    if (is.null(path_backup)) {
      ext <- tools::file_ext(path)
      path_noext <- tools::file_path_sans_ext(path)
      path_backup <- file.path(paste0(
        path_noext,
        "-",
        strftime(Sys.time(), "%Y-%m-%d@%H-%M-%S"),
        ".",
        ext
      ))
    }
    invisible(path_backup)
  }

create_backup <-
  function(path = NULL,
           path_backup = NULL,
           backup = NULL,
           overwrite = FALSE) {
    if (!backup) {
      # print_argfalse_msg("backup")
      return(invisible())
    }

    path_backup <-
      get_path_backup_safely(path = path,
                             path_backup = path_backup,
                             backup = backup)
    if (file.exists(path_backup) & !overwrite) {
      print_argfalse_msg("overwrite")
      return(invisible())
    }

    file.copy(from = path, to = path_backup)
    print_export_msg(path_backup)
    invisible(path_backup)
  }


guess_ext <-
  function(x = NULL, ...) {
    if (is.null(x)) {
      ret <- try({
        ggplot2::last_plot()
      }, silent = TRUE)
      print_guess_msg()
      if (is.null(ret)) {
        print_isnull_msg()
        return(invisible(NULL))
      }
    } else if (ggplot2::is.ggplot(x)) {
      print_autoexport_msg(".png")
      return(export_ext_png(x, ...))
    } else if (is.data.frame(x)) {
      print_autoexport_msg(".csv")
      return(export_ext_csv(x, ...))
    }
  }

export_ggplot <-
  function(x = NULL, path = NULL, ...) {
    ggplot2::ggsave(filename = path,
                    plot = x,
                    ...)
    invisible(path)
  }

export_readr_or_rio <-
  function(x = NULL,
           path = NULL,
           ext = ext,
           ...) {
    ret <- try({
      fun_readr <- paste0("readr::write_", ext)
      do_call_with(fun_readr, list(x = x, path = path, ...))
    }, silent = TRUE)

    if (inherits(ret, "try-error")) {
      ret <- rio::export(x, path, ...)
      if (!inherits(ret, "try-error")) {
        print_nonreadr_msg("rio")
      }
    }

    invisible(ret)
  }

export_common <-
  function(x = NULL,
           path = NULL,
           ext = NULL,
           overwrite = TRUE,
           backup = FALSE,
           path_backup = NULL,
           export = TRUE,
           return = TRUE,
           ...) {
    if (!export & return) {
      return(invisible(path))
    }

    if (file.exists(path) & !overwrite) {
      print_argfalse_msg("overwrite")
      return(invisible())
    }

    # Don't overwrite the directory, even if overwrite == TRUE for this function.
    # message("Ignoring `overwrite == TRUE` to prevent an accidental overwrite.\n",
    #         "The user should rename the existing directory explicitly.")
    create_dir(dir, overwrite = FALSE, backup = backup)

    if (ext %in% c("png")) {
      print_nonreadr_msg("ggplot2")

      if (is.null(x)) {
        x <- ggplot2::last_plot()
      }
      ret <- export_ggplot(x = x, path = path, ...)

    } else if (grepl("rda", tolower(ext))) {
      # x <- ls(parent.frame())
      # path <- gsub(ext, "rdata", path)
      # rio::export(x, path, ...)
      ret <-
        suppressWarnings(utils::capture.output(session::save.session(path)))
    } else {
      ret <- export_readr_or_rio(x = x, path = path, ...)
    }

    print_export_msg(ret)
    path_backup <-
      create_backup(file,
                    dir,
                    ext,
                    path,
                    path_backup,
                    backup,
                    overwrite)
    invisible(ret)
  }


#' Export an object
#'
#' @description Saves data given a directory, file name, and extension, or a full path directly.
#' @details Object to save must be a data.frame (or matrix) for most formats.
#'
#' This function is intended to be used in a NSE manner, where the file name
#' is derived from the name of the object passed to the function (via \code{deparse(substitute(x))}).
#' \code{export_path()} may be a suitable alternative if the user wants to specify
#' the path name directly, although such an action can also be done with this function.
#'
#' Note that the user may not typically be concerned several arguments, including
#' \code{overwrite}, \code{backup}, \code{path_backup}, \code{export}, and \code{return}.
#' Nonetheless, these are provided in order to facilitate usage
#' with scripts run using "meta"-parameters that determine what data to export, whether or not
#' to overwrite, etc.
#'
#' Internally, a distinct method (\code{session::save.session})
#' is used for the RData-type file extension in order
#' to allow for importing of packages. Also, \code{ggplot2} objects are treated in
#' a special manner.
#'
#' Many supplementary functions, using the format \code{export_ext_*()} are provided
#' for convenience. (e.g. \code{export_ext_csv()} instead of \code{export_ext(..., ext = 'csv').}
#'
#' @inheritParams create_dir
#' @param x data.frame (or matrix) for most formats.
#' @param file character. Bare file (i.e. without folderor extension),
#' @param ext character. Bare extension (i.e. without a dot). Must be one of valid formats.
#' @param path character. Concatenation of \code{file}, \code{dir}, and \code{ext},
#' @param overwrite logical.
#' @param backup logical.
#' @param path_backup like \code{path},
#' @param export logical. Indicates whether to actually carry ret action. Intended to be used as a "catch all".
#' @param return logical. Relevant ONLY if \code{export == FALSE}.
#' Set to \code{TRUE} in order to preview what would be rendered.
#' @param ... dots. Parameters to pass directly to the internally used export function.
#' Important for explicitly specifying non-default \code{ggplot2::ggsave()} parameters,
#' as well as specifying worksheets, etc. for an Excel-based export method.
#' @return character. Path.
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
#' @importFrom ggplot2 last_plot ggsave
#' @importFrom session save.session
#' @importFrom utils capture.output
#' @importFrom tools file_ext
export_ext <-
  function(x = NULL,
           file = deparse(substitute(x)),
           dir = getwd(),
           ext = guess_ext(x),
           path = file.path(dir, paste0(file, ".", ext)),
           overwrite = TRUE,
           backup = FALSE,
           path_backup = NULL,
           export = TRUE,
           return = TRUE,
           ...) {
    # browser()
    if (!export & !return) {
      print_argfalse_msg("export")
      return(invisible())
    }

    if (is.null(x) & is.null(ext)) {
      print_isnull_msg()
      return(invisible())
    }

    path <-
      get_path_safely(dir, file, ext, path)

    ret <- export_common()
    invisible(ret)
  }

export_path <-
  function(x = NULL,
           path = NULL,
           overwrite = TRUE,
           backup = FALSE,
           path_backup = NULL,
           export = TRUE,
           return = TRUE,
           ...) {
    if (!export & !return) {
      print_argfalse_msg("export")
      return(invisible())
    }

    if (is.null(x)) {
      print_isnull_msg()
      return(invisible())
    }

    ext <- guess_ext(x)
    if (is.null(ext)) {
      print_isnull_msg()
      return(invisible())
    }

    if (!export & return) {
      return(invisible(path))
    }

    if (file.exists(path) & !overwrite) {
      print_argfalse_msg("overwrite")
      return(invisible())
    }
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
# export_viz <- export_ext_png
export_gg <- function(...) {
  # print_dpc_msg("export_ext_png")
  export_ext(ext = "png", ...)
}

#' @export
#' @rdname export_ext
# export_viz <- export_ext_png
export_viz <- function(...) {
  # print_dpc_msg("export_ext_png")
  export_ext(ext = "png", ...)
}
