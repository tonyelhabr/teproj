
#' @title Import an object.
#' @description Reads in an object from a filepath.
#' @details None.
#' @inheritParams export_ext
#' @param import boolean. Indicates whether to actually carry out function.
#' @export
#' @importFrom rio import
#' @importFrom session restore.session
#' @importFrom utils capture.output
#' @importFrom tibble as_tibble
import_ext <-
  function(filename = NULL,
           dir = paste0(getwd(), "/"),
           ext = NULL,
           filepath = paste0(dir, filename, ".", ext),
           import = TRUE,
           ...) {
    if (!import) {
      .print_argfalse_msg("import")
      return(invisible())
    }

    if(is.null(filename) && is.null(ext)) {
      .print_isnull_msg()
      return(invisible())
    }

    # browser()
    if(!is.character(filename))
      filename <- deparse(substitute(filename))
    filepath <- .get_filepath(filename, dir, ext, filepath)

    if(grepl("rda", tolower(ext))) {
      # browser()
      # x <- ls(parent.frame())
      out <- suppressWarnings(utils::capture.output(session::restore.session(filepath)))
    } else {
      out <- rio::import(filepath, ...)
      try({out <- tibble::as_tibble(out)}, silent = TRUE)
    }
    invisible(out)
  }

#' @export
#' @rdname import_ext
import_ext_csv <- function(...) import_ext(ext = "csv", ...)

#' @export
#' @rdname import_ext
import_ext_xlsx <- function(...) import_ext(ext = "xlsx", ...)

#' @export
#' @rdname import_ext
import_excel <- import_ext_xlsx

#' @export
#' @rdname import_ext
import_ext_rdata <- function(...) import_ext(ext = "RData", ...)

#' @export
#' @rdname import_ext
import_ext_RData <- import_ext_rdata

#' @export
#' @rdname import_ext
import_ext_rda <- import_ext_rdata

#' @export
#' @rdname import_ext
import_ext_rds <- function(...) import_ext(ext = "rds", ...)

