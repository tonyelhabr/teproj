
# # https://stackoverflow.com/questions/7597559/grep-using-a-character-vector-with-multiple-patterns?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
# #Returns all items in a list that are not contained in toMatch
# #toMatch can be a single item or a list of items
# exclude <- function (theList, toMatch){
#   return(setdiff(theList,include(theList,toMatch)))
# }
#
# #Returns all items in a list that ARE contained in toMatch
# #toMatch can be a single item or a list of items
# include <- function (theList, toMatch){
#   matches <- unique (grep(paste(toMatch,collapse="|"),
#                           theList, value=TRUE))
#   return(matches)
# }

# Modified from https://github.com/yihui/knitr/blob/master/R/utils.R.
# .is_windows <- function() .Platform$OS.type == "windows"
# .is_abs_path <- function(x) {
#   if (.is_windows())
#     grepl(':', x, fixed = TRUE) || grepl('^\\\\', x) else grepl('^[/~]', x)
# }

# NOTE: This is used by *_proj_io() functions.
# Modified from https://github.com/rstudio/rmarkdown/blob/81c209271e06266115e08263dbcde5d007e4d77c/R/includes.R
# NOTE: Not sure why, but need to set mustWork = FALSE) explicitly, otherwise
# warnings appear. (This behavior is not observed when calling normalizePath directly.
# Howver, normalizePath() specifies `winslash = "\\"` by default, which is annoying.)
normalize_path <- function(path = NULL, winslash = "/", mustWork = NA) {
  if (!is.null(path))
    normalizePath(path, winslash = winslash, mustWork = mustWork)
}

#' Construct a file path
#'
#' @description Construct a file path given a directory, file, and extension.
#' @details This function is notable because it has a \code{path} argument
#' that can be used as a quick "out".
#' This helps with argument checking with package functions.
#' @inheritParams export_ext
#' @return character. Path.
#' @export
get_path_safely <-
  function(dir = NULL,
           file = NULL,
           ext = NULL,
           path = NULL) {
    if (is.null(path)) {
      path <- file.path(dir, paste0(file, ".", ext))
    }
    # path <- normalize_path(path)
    path
  }

#' Construct a file path
#'
#' @description Constructs a file path given a directory as the first input and an extension as the last input.
#' @details Assumes that the last arguments is the path extension. If \code{ext} is not
#' specified explicitly, it is assumed to be the last argument passed to the function.
#' This format allows for 'lazy' construction of the basename of the file (sans extension)
#' given all arguments that are not the first nor the last.
#' @inheritParams export_ext
#' @param ... dots. Arguments parssed to construct name of file path without the directory nor
#' the extension. Uses \code{paste()} to collapse all arguments between \code{dir} and \code{ext}.
#' @return character. Path.
#' @export
get_path_lazily <-
  function(dir = NULL, ..., ext = NULL) {
    dots <- list(...)
    if (is.null(ext)) {
      ext <- rev(unlist(dots))[1]
      dots <- dots[-c(length(dots))]
    }
    file <- paste(unlist(dots), collapse = "", sep = "")
    file.path(dir, paste0(file, ".", ext))
  }

#' Sort a named list
#' @description Sort a named list.
#' @details None.
#' @param x list.
#' @return list.
#' @export
sort_named_list <- function(x) {
  x[order(names(x))]
}

#' \code{do.call()} with package namespace
#'
#' @description Allows a package to explicitly included in \code{do.call()}
#' @details It's probably better to just use \code{purrr::invoke()}, since it
#' does the same thing that this function was crated to do.
#' @param what character. package name and function specified in explicit
#' namespace notation (i.e. \code{package::function}).
#' @param args list. Arguments passed to function parsed from \code{what}.
#' @param ... dots. Arguments passed to \code{do.call()}.
#' @source \url{https://stackoverflow.com/questions/10022436/do-call-in-combination-with}.
#' @export
do_call_with <- function(what, args, ...) {
  if (is.character(what)) {
    fn <- strsplit(what, "::")[[1]]
    what <- if (length(fn) == 1) {
      get(fn[[1]], envir = parent.frame(), mode = "function")
    } else {
      get(fn[[2]], envir = asNamespace(fn[[1]]), mode = "function")
    }
  }

  do.call(what, as.list(args), ...)
}


# .test <- function(filter = NULL, pkg = ".", ...) {
#   test(pkg = pkg, filter = filter, ...)
# }

# TODO: Implement proper message/warning/error wrappers.
#' \code{warning()} + \code{sprintf()}
#'
#' @description Implements \code{warning()} and \code{sprintf()} such that the specified
#' environment (of a function) is properly shown in the message.
#' @details None.
#' @param ... dots. Arguments pased to \code{sprintf()}.
#' @param n integer. Number of calling environment frame.
#' @source \url{https://stackoverflow.com/questions/9596918/r-warning-wrapper-raise-to-parent-function}.
#' @export
warningf <- function(..., n = 1L){
  parent_call <- sys.call(sys.nframe() - n)
  warning(paste("In", parent_call, ":", sprintf(...)), call. = FALSE)
}

#' Remove rownames from data.frame
#'
#' @param data data frame
#' @return data.frame
#' @export
#' @source \url{https://github.com/tidyverse/broom/blob/master/R/utilities.R}.
unrowname <- function(data = NULL) {
  stopifnot(!is.null(data), is.data.frame(data))
  rownames(data) <- NULL
  data
}

