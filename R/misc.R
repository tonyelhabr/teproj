
#' Construct a file path
#'
#' @description Construct a file path given a directory, file, and extension.
#' @details This function is notable because it has a `path` argument
#' that can be used as a quick "out".
#' This helps with argument checking with package functions.
#' @inheritParams export_ext
#' @return character. Path.
#' @keywords internal
.get_path_safely <-
  function(dir,
           file,
           ext,
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
#' @details Assumes that the last arguments is the path extension. If `ext` is not
#' specified explicitly, it is assumed to be the last argument passed to the function.
#' This format allows for 'lazy' construction of the basename of the file (sans extension)
#' given all arguments that are not the first nor the last.
#' @inheritParams export_ext
#' @param ... dots. Arguments parssed to construct name of file path without the directory nor
#' the extension. Uses `paste()` to collapse all arguments between `dir` and `ext`.
#' @return character. Path.
#' @keywords internal
.get_path_lazily <-
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
#' @keywords internal
.sort_named_list <- function(x) {
  x[order(names(x))]
}

#' `do.call()` with package namespace
#'
#' @description Allows a package to explicitly included in `do.call()`
#' @details It's probably better to just use `purrr::invoke()`, since it
#' does the same thing that this function was crated to do.
#' @param what character. package name and function specified in explicit
#' namespace notation (i.e. `package::function`).
#' @param args list. Arguments passed to function parsed from `what`.
#' @param ... dots. Arguments passed to `do.call()`.
#' @source <https://stackoverflow.com/questions/10022436/do-call-in-combination-with>.
#' @keywords internal
.do_call_with <- function(what, args, ...) {
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
#' `warning()` + `sprintf()`
#'
#' @description Implements `warning()` and `sprintf()` such that the specified
#' environment (of a function) is properly shown in the message.
#' @details None.
#' @param ... dots. Arguments pased to `sprintf()`.
#' @param n integer. Number of calling environment frame.
#' @source <https://stackoverflow.com/questions/9596918/r-warning-wrapper-raise-to-parent-function>.
#' @keywords internal
.warningf <- function(..., n = 1L){
  parent_call <- sys.call(sys.nframe() - n)
  warning(paste("In", parent_call, ":", sprintf(...)), call. = FALSE)
}

#' Remove rownames from data.frame
#'
#' @param data data frame
#' @return data.frame
#' @source <https://github.com/tidyverse/broom/blob/master/R/utilities.R>.
#' @keywords internal
.unrowname <- function(data = NULL) {
  stopifnot(!is.null(data), is.data.frame(data))
  rownames(data) <- NULL
  data
}

