
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



#' Import an object
#'
#' @description Reads in an object from a path.
#' @details None.
#' @inheritParams export_ext
#' @param import boolean. Indicates whether to actually carry out function.
#' @param ... dots. Parameters to pass on to internally used import function.
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
      print_argfalse_msg("import")
      return(invisible())
    }

    basename_try <- try(file, silent = TRUE)
    if (!inherits(basename_try, "try-error") & is.character(basename_try)) {
      file <- basename_try
    } else {
      file <- deparse(substitute(file))
    }

    if(missing(file) & missing(ext)) {
      print_ismiss_msg()
      return(invisible())
    }

    path <- get_path(dir, file, ext, path)

    if(!file.exists(path)) {
      if(getOption("teproj.print.wrn")) warning("Cannot find file at ", path, ".")
      return(invisible(path))
    }

    if(!import & return) {
      return(invisible(path))
    }

    if(grepl("rda", tolower(ext))) {
      # x <- ls(parent.frame())
      out <- suppressWarnings(utils::capture.output(session::restore.session(path)))
    } else {

      out <- try({
        fun_readr <- paste0("read_", ext)
        do_call_with(fun_readr, list(file = path))
      }, silent = TRUE)

      if(inherits(out, "try-error")) {
        out <- rio::import(path, ...)
        if(!inherits(out, "try-error")) {
          print_nonreadr_msg("rio")
        }

        out <- try(tibble::as_tibble(out), silent = TRUE)

        if(inherits(out, "try-error")) {
          if(getOption("teproj.print.msg")) message("Could not convert data imported to tibble.")
        }
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

