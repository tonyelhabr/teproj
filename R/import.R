
# # See https://stackoverflow.com/questions/15811305/how-can-i-have-this-deparse-function-working.
# g = function(x) {
#   x.try <- try(x, silent = TRUE)
#   if (!inherits(x.try, "try-error") & is.character(x.try)) x.try
#   else deparse(substitute(x))
# }
#
# # test it out
# if (exists("test")) rm(test)
#
# g(test) # "test"
# g("test") # "test"
#
# test <- "xyz"
# g(test) # "xyz"
# g("test") # "test"
#
# test <- 3
# g(test) # "test"
# g("test") # "test"

# z <- try(import_ext(iris, ext = "csv"))
# z

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
  function(filename,
           dir = getwd(),
           ext,
           filepath = file.path(dir, paste0(filename, ".", ext)),
           import = TRUE,
           return = TRUE,
           ...) {
    # browser()
    if (!import & !return) {
      .print_argfalse_msg("import")
      return(invisible())
    }

    # browser()
    filename_try <- try(filename, silent = TRUE)
    if (!inherits(filename_try, "try-error") & is.character(filename_try)) {
      filename <- filename_try
    } else {
      filename <- deparse(substitute(filename))
    }

    if(missing(filename) & missing(ext)) {
      .print_ismiss_msg()
      return(invisible())
    }

    # if(!missing(filename) & !is.character(filename)) {
    #   # browser()
    #   filename <- deparse(substitute(filename))
    #
    # } else {
    #   filename_info <- pryr::promise_info(filename)
    #   filename <- as.character(filename_info$code)
    #   # filename <- NULL
    # }


    # z <- file.path(dir, paste0(filename, ".", ext))
    # browser()
    filepath <- .get_filepath(filename, dir, ext, filepath)

    if(!file.exists(filepath)) {
      if(getOption("teproj.print.wrn")) warning("Cannot find file at ", filepath, ".")
      return(invisible(filepath))
    }

    if(!import & return) {
      return(invisible(filepath))
    }

    # browser()
    if(grepl("rda", tolower(ext))) {
      # browser()
      # x <- ls(parent.frame())
      out <- suppressWarnings(utils::capture.output(session::restore.session(filepath)))
    } else {
      out <- rio::import(filepath, ...)
      out <- try(tibble::as_tibble(out), silent = TRUE)

      if(inherits(out, "try-error")) {
        if(getOption("teproj.print.msg")) message("Could not convert data imported to tibble.")
      }

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

