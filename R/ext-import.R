
#' @title Import an object.
#' @description Reads in an object from a filepath.
#' @details None.
#' @inheritParams export_ext
#' @export
#' @importFrom rio import
#' @importFrom session restore.session
#' @importFrom utils capture.output
import_ext <-
  function(x = NULL,
           filename = deparse(substitute(x)),
           dir = paste0(getwd(), "/"),
           ext = NULL,
           filepath = paste0(dir, filename, ".", ext),
           ...) {

    # browser()
    filepath <-
      .get_filepath(filename, dir, ext, filepath)

    if(grepl("rda", tolower(ext))) {
      # browser()
      # x <- ls(parent.frame())
      out <- suppressWarnings(utils::capture.output(session::restore.session(filepath)))
    } else {
      out <- rio::import(filepath, ...)
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

