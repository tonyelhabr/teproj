
#' @title Import an object.
#' @description Reads in an object from a filepath.
#' @details None.
#' @inheritParams export_ext
#' @export
#' @importFrom rio import
import_ext <-
  function(x = NULL,
           filename = deparse(substitute(x)),
           dir = paste0(getwd(), "/"),
           ext = NULL,
           filepath = paste0(dir, filename, ".", ext),
           ...) {

    # browser()
    filepath <-
      .get_filepath(dir, filename, ext, filepath)

    # out <- .import_ext(filepath, x, ...)
    out <- rio::import(filepath, x, ...)
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
import_ext_rda <- function(...) import_ext(ext = "rda", ...)

#' @export
#' @rdname import_ext
import_ext_rdata <- function(...) import_ext(ext = "RData", ...)

#' @export
#' @rdname import_ext
import_ext_RData <- import_ext_rdata

#' @export
#' @rdname import_ext
import_ext_rds <- function(...) import_ext(ext = "rds", ...)

#' @export
#' @rdname import_ext
import_ext_feather <- function(...) import_ext(ext = "feather", ...)
