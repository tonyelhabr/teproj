
.get_path_backup_safely <-
  function(path = NULL,
           path_backup = NULL,
           backup = FALSE) {
    if(!backup) {
      # .print_argfalse_msg("overwrite")
      return(invisible())
    }
    if(is.null(path_backup) & is.null(path)) {
      .print_isnull_msg()
      return(invisible())
    } else if (is.null(path_backup) & !is.null(path)) {
      ext <- tools::file_ext(path)
      path_noext <- tools::file_path_sans_ext(path)
      path_backup <- file.path(paste0(
        path_noext,
        "-",
        strftime(Sys.time(), "%Y-%m-%d@%H-%M-%S"),
        ".",
        ext
      ))
    } else if(!is.null(path_backup)) {

    }
    invisible(path_backup)
  }

.create_backup <-
  function(path = NULL,
           path_backup = NULL,
           backup = NULL,
           overwrite = FALSE) {
    if (!backup) {
      # .print_argfalse_msg("backup")
      return(invisible())
    }

    path_backup <-
      .get_path_backup_safely(path = path,
                             path_backup = path_backup,
                             backup = backup)
    if (file.exists(path_backup) & !overwrite) {
      .print_argfalse_msg("overwrite")
      return(invisible())
    }

    file.copy(from = path, to = path_backup)
    .print_export_msg(path_backup)
    invisible(path_backup)
  }


.guess_ext <-
  function(x = NULL, ...) {
    if (is.null(x)) {
      res <- try({
        ggplot2::last_plot()
      }, silent = TRUE)
      .print_guess_msg()
      if (is.null(res)) {
        .print_isnull_msg()
        return(invisible(NULL))
      }
    } else if (ggplot2::is.ggplot(x)) {
      .print_autoexport_msg(".png")
      res <- "png"
    } else if (is.data.frame(x)) {
      .print_autoexport_msg(".csv")
      res <- "csv"
    }
    res
  }

.export_ggplot <-
  function(x = NULL, path = NULL, ...) {
    ggplot2::ggsave(filename = path,
                    plot = x,
                    ...)
    invisible(path)
  }

.export_readr_or_rio <-
  function(x = NULL,
           path = NULL,
           ext = NULL,
           ...) {

    fun_readr <- sprintf("readr::write_%s", ext)
    # browser()
    res <- try({
      do_call_with(fun_readr, list(x = x, path = path, ...))
    }, silent = TRUE)

    if (inherits(res, "try-error")) {
      res <- rio::export(x, path, ...)
      if (!inherits(res, "try-error")) {
        .print_nonreadr_msg("rio")
      }
    }

    invisible(path)
  }

.export_xxx <-
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
      .print_argfalse_msg("overwrite")
      return(invisible())
    }

    # Don't overwrite the directory, even if overwrite == TRUE for this function.
    # message("Ignoring `overwrite == TRUE` to prevent an accidental overwrite.\n",
    #         "The user should rename the existing directory explicitly.")
    # browser()
    dir <- dirname(path)
    create_dir(dir, overwrite = FALSE, backup = backup)

    if (ext %in% c("png")) {
      .print_nonreadr_msg("ggplot2")

      if (is.null(x)) {
        x <- ggplot2::last_plot()
      }
      res <- .export_ggplot(x = x, path = path, ...)

    } else if (grepl("rda", tolower(ext))) {
      # x <- ls(parent.frame())
      # path <- gsub(ext, "rdata", path)
      # rio::export(x, path, ...)
      res <-
        suppressWarnings(utils::capture.output(session::save.session(path)))
      res <- path
    } else {
      res <- .export_readr_or_rio(x = x, path = path, ext = ext, ...)

    }
    .print_export_msg(res)
    path_backup <-
      .create_backup(path = path,
                    path_backup = path_backup,
                    backup = backup,
                    overwrite = overwrite)
    invisible(res)
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
#' @param export logical. Indicates whether to actually carry res action. Intended to be used as a "catch all".
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
           ext = .guess_ext(x),
           path = file.path(dir, paste0(file, ".", ext)),
           overwrite = TRUE,
           backup = FALSE,
           path_backup = NULL,
           export = TRUE,
           return = TRUE,
           ...) {
    # browser()
    if (!export & !return) {
      .print_argfalse_msg("export")
      return(invisible())
    }

    if (is.null(x) & is.null(ext)) {
      .print_isnull_msg()
      return(invisible())
    }

    path <-
      get_path_safely(dir = dir, file = file, ext = ext, path = path)

    res <-
      .export_xxx(
        x = x,
        path = path,
        ext = ext,
        overwrite = overwrite,
        backup = backup,
        path_backup = path_backup,
        export = export,
        return = return,
        ...
        )
    invisible(res)
  }

#' Export an object
#'
#' @description Saves data given a full path.
#' @details This function works similarly to \code{export_ext()} internally, but the
#' order of the arguments are presented in a different order because it is assumed
#' that \code{path} will be supplied. This not the assumption with the \code{export_ext()} function.
#' If for some reason \code{path} is not provided, the \code{dir},
#' \code{file}, and \code{ext} defaults are used.
#'
#' Note that the difference between \code{export_path()} and \code{export_ext()}
#' is not as significant as it is for \code{import_path()} and \code{import_ext()}.
#'
#' Also, \code{export_path()} does not have an analogue to \code{import_path_cleanly()}.
#'
#' @inheritParams export_ext
#' @return object.
#' @export
#' @importFrom tools file_ext
export_path <-
  function(x = NULL,
           path = NULL,
           overwrite = TRUE,
           backup = FALSE,
           path_backup = NULL,
           export = TRUE,
           return = TRUE,
           dir = getwd(),
           file = deparse(substitute(x)),
           ext = .guess_ext(x),
           ...) {

    if (!export & !return) {
      .print_argfalse_msg("export")
      return(invisible())
    }

    if (is.null(x)) {
      .print_isnull_msg()
      return(invisible())
    }

    # if (is.null(ext)) {
    #   .print_isnull_msg()
    #   return(invisible())
    # }

    if(is.null(path)) {
      path <-
        get_path_safely(dir = dir, file = file, ext = ext, path = path)
    }

    if (!export & return) {
      return(invisible(path))
    }

    if (file.exists(path) & !overwrite) {
      .print_argfalse_msg("overwrite")
      return(invisible())
    }

    res <-
      .export_xxx(
        x = x,
        path = path,
        ext = ext,
        overwrite = overwrite,
        backup = backup,
        path_backup = path_backup,
        export = export,
        return = return,
        ...
      )
    invisible(res)
  }

#' @export
#' @rdname export_ext
export_ext_csv <- function(...)
  export_ext(ext = "csv", ...)

#' @export
#' @rdname export_ext
export_ext_rdata <- function(...)
  export_ext(ext = "RData", ...)

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
export_gg <- export_ext_png

