
.import_readr_or_rio <-
  function(path = NULL, ext =ext, ...) {

    fun_readr <- sprintf("readr::read_%s", ext)
    res <- try({
      do_call_with(fun_readr, list(file = path))
    }, silent = TRUE)

    if(inherits(res, "try-error")) {
      res <- rio::import(path, ...)
      if(!inherits(res, "try-error")) {
        .print_nonreadr_msg("rio")
      }

      res <- try(tibble::as_tibble(res), silent = TRUE)

      if(inherits(res, "try-error")) {
        .print_tibblefail_msg()
      }
    }

    invisible(res)
  }

#' Import an object
#'
#' @description Reads in data given a file, directory, and extension, or a full path directly.
#' @details This function is intended to be a direct counterpart to the similarly
#' named export function. It should be used in a NSE manner. However, this may not be what the user wants.
#' (The NSE aspect is perhaps more appropriate for exporting, where choices about file name
#' may be determined from the object name.)
#' \code{import_path()} may be a suitable alternative.
#'
#' Note that the user may not typically be concerned with the \code{import} and
#' \code{return} parameters. Nonetheless, these are provided in order to facilitate usage
#' with scripts run using "meta"-parameters that determine what data to import.
#'
#' Internally, a distinct method (\code{session::restore.session})
#' is used for the RData-type file extension in order
#' to allow for importing of packages.
#'
#' Many supplementary functions, using the format \code{import_ext_*()} are provided
#' for convenience. (e.g. \code{import_ext_csv()} instead of \code{import_ext(..., ext = 'csv').}
#'
#' @inheritParams export_ext
#' @param import logical. Indicates whether to actually execute function.
#' @param ... dots. Arguments to pass dircetly to internally used import function.
#' @return object.
#' @export
#' @importFrom rio import
#' @importFrom session restore.session
#' @importFrom utils capture.output
#' @importFrom tibble as_tibble
import_ext <-
  function(file,
           dir = getwd(),
           ext,
           path = file.path(dir, paste0(file, ".", ext)),
           import = TRUE,
           return = TRUE,
           ...) {
    # browser()
    if (!import & !return) {
      .print_argfalse_msg("import")
      return(invisible())
    }

    file_try <- try(file, silent = TRUE)
    if (!inherits(file_try, "try-error") & is.character(file_try)) {
      file <- file_try
    } else {
      file <- deparse(substitute(file))
    }

    if(missing(file) & missing(ext)) {
      .print_ismiss_msg()
      return(invisible())
    }

    path <- get_path_safely(dir, file, ext, path)

    if(!file.exists(path)) {
      .print_filenotexist_msg(path)
      return(invisible(path))
    }

    if(!import & return) {
      return(invisible(path))
    }

    if(grepl("rda", tolower(ext))) {
      # x <- ls(parent.frame())
      res <- suppressWarnings(utils::capture.output(session::restore.session(path)))
    } else {
      res <- .import_readr_or_rio(path = path, ext = ext)
    }

    invisible(res)
  }

#' Import an object
#'
#' @description Reads in data given a full path.
#' @details Works similarly to \code{import_ext()} internally, but may be considered simpler.
#' @inheritParams import_ext
#' @return object.
#' @export
#' @importFrom tools file_ext
import_path <-
  function(path = NULL, import = TRUE, return = TRUE, ...) {

    if (!import & !return) {
      .print_argfalse_msg("import")
      return(invisible())
    }

    if(!file.exists(path)) {
      .print_filenotexist_msg(path)
      return(invisible(path))
    }

    if(!import & return) {
      return(invisible(path))
    }

    stopifnot(!is.null(path))
    ext <- tools::file_ext(path)
    res <- .import_readr_or_rio(path = path, ext = ext)
    invisible(res)
  }

#' Import an object
#'
#' @description Reads in data given a full path and 'cleans' the names.
#' @details Calls \code{import_path} internally.
#' @inheritParams import_ext
#' @return object.
#' @export
#' @importFrom janitor clean_names
import_path_cleanly <-
  function(path = NULL, ...) {
    res <- import_path(path = path, ...)
    res <- try({janitor::clean_names(res)}, silent = TRUE)
    invisible(res)
  }

#' @export
#' @rdname import_ext
import_ext_csv <- function(...) import_ext(ext = "csv", ...)

#' @export
#' @rdname import_ext
import_ext_xlsx <- function(...) import_ext(ext = "xlsx", ...)

#' @export
#' @rdname import_ext
import_ext_rdata <- function(...) import_ext(ext = "RData", ...)

#' @export
#' @rdname import_ext
import_ext_rda <- import_ext_rdata

#' @export
#' @rdname import_ext
import_ext_rds <- function(...) import_ext(ext = "rds", ...)

