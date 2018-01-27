
#' @title Control package messages.
#' @description Sets options for relevant \code{teproj} functions.
#' @details Intended to be used as a wrapper to \code{options(...)}.
#' @param msg,wrn,err booleans. Indiciates whether to show messages, warnings, and errors for package functions.
#' @export
set_pkg_print_opts <- function(msg = getOption("teproj.print.msg"),
                               wrn = getOption("teproj.print.wrn"),
                               err = getOption("teproj.print.err")) {
  options(teproj.print.msg = msg)
  options(teproj.print.wrn = wrn)
  options(teproj.print.err = err)
}

#' @title Control package render settings.
#' @description Sets options for relevant \code{teproj} functions.
#' @details Intended to be used as a wrapper to \code{options(...)}.
#' @param echo,cache,results,width,fig.align,fig.show,fig.width,fig.height,warning,message Arguments passed to the \code{knitr_opts$set()} arguments of their same namesake.
#' @export
set_pkg_render_opts <-
  function(echo = getOption("teproj.render.echo"),
           cache = getOption("teproj.render.cache"),
           results = getOption("teproj.render.results"),
           width = getOption("teproj.render.width"),
           fig.align = getOption("teproj.render.fig.align"),
           fig.show = getOption("teproj.render.fig.show"),
           fig.width = getOption("teproj.render.fig.width"),
           fig.height = getOption("teproj.render.fig.height"),
           warning = getOption("teproj.render.warning"),
           message = getOption("teproj.render.message")) {
  options(teproj.render.echo = FALSE)
  options(teproj.render.cache = FALSE)
  options(teproj.render.results = "hide")
  options(teproj.render.width = 100)
  options(teproj.render.fig.align = "center")
  options(teproj.render.fig.show = "hide")
  options(teproj.render.fig.width = 10)
  options(teproj.render.fig.height = 10)
  options(teproj.render.warning = FALSE)
  options(teproj.render.message = FALSE)
}

#' @title Create a directory.
#' @description Creates the directory if it does not exist.
#' @details Used by other functions in this package.
#' @param dir character. Folder name (with a trailing slash).
#' @param overwrite boolean.
#' @param backup boolean.
#' @param create boolean.
#' @param ... dots. Not currently used.
#' @return character. Filepath.
#' @export
create_dir <- function(dir = paste0(getwd(), "/"),
                       overwrite = FALSE,
                       backup = TRUE,
                       create = TRUE,
                       ...) {
  if (!create) {
    .print_argfalse_msg("create")
    return(invisible())
  }

  if (is.null(dir)) {
    .print_isnull_msg()
    return(invisible())
  }

  # Check for the existence of a drive.
  if (!grepl(":", dir)) {
    # message("Appending the drive of the current working directory.")
    dir <- paste0(getwd(), "/", dir)
  }

  if (substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }

  if (dir.exists(dir)) {
    if (getOption("teproj.print.msg"))
      message(dir, " already exists.")
    if(overwrite) {
      # unlink(list.files(dir, full.names = TRUE), recursive = TRUE, force = TRUE)
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      if(getOption("teproj.print.msg"))
        message("Overwrote ", dir, " (because `overwrite == TRUE`).")
    }
  } else {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (getOption("teproj.print.msg")) {
      message("Created ", dir, ".")
    }
  }
  invisible(dir)
}


.get_filepath <-
  function(filename,
           dir,
           ext,
           filepath) {
    if (is.null(filepath)) {
      # filepath <- paste0(dir, filename, ".", ext)
      filepath <- file.path(dir, paste0(filename, ".", ext))
    }
    filepath
  }


.check_files_exist <- function(filepaths, dir, pattern, ...) {
  # Debugging...
  # path = "."

  # Assumption...
  exist <- FALSE
  out <- ""
  # browser()
  if(missing(filepaths) & missing(dir)) {
    .print_ismiss_msg()

  } else if (missing(filepaths) & !missing(dir)) {
    # TODO: For some reason include.dirs = FALSE does not produce expected output?
    filepaths <-
      list.files(
        path = dir,
        # ...,
        pattern = pattern,
        # recursive = FALSE,
        full.names = TRUE,
        ignore.case = TRUE,
        include.dirs = FALSE
      )
    dirs <-
      list.dirs(path = dir,
                # ...,
                recursive = FALSE,
                full.names = TRUE
      )
    filepaths <- setdiff(filepaths, dirs)
    # Or...
    # # Add a file extension to the pattern.
    # filepaths <-
    #   list.files(
    #     path = dir,
    #     # ...,
    #     # pattern = paste0(pattern, "(\\.*$"),
    #     # recursive = FALSE,
    #     full.names = TRUE,
    #     ignore.case = TRUE,
    #     include.dirs = FALSE
    #   )
    if(length(filepaths) == 0) {
      .print_nofile_msg()
    } else {
      exist <- TRUE
      out <- filepaths
    }
  } else {
    filepaths_exist <- as.logical(lapply(filepaths, file.exists))
    if(!any(filepaths_exist)) {
      if(getOption("teutils.print_wrn")) {
        warning("Specified files do not exist.")
      }
    } else {
      exist <- TRUE
      out <- filepaths
    }
  }
  list(exist = exist, filepaths = out)
}

# Modified from https://stackoverflow.com/questions/10022436/do-call-in-combination-with.
.do_call <- function(what, args, ...) {
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
# See https://stackoverflow.com/questions/9596918/r-warning-wrapper-raise-to-parent-function.
.warningf <- function(..., n = 1L){
  parent_call <- sys.call(sys.nframe() - n)
  warning(paste("In", parent_call, ":", sprintf(...)), call. = FALSE)
}

.print_isnull_msg <- function(...,  msg_input = "", n = 2) {
  # browser()
  # parent.call <- sys.call(sys.nframe() - 1L)
  dots <- list(...)
  if(length(dots) > 0) msg_input <- gsub(",$", "", paste(dots, collapse = ","))
  if(getOption("teproj.print.wrn")) .warningf("Required input `%s`is NULL.", msg_input, n = n)
}

.print_ismiss_msg <- function(...,  msg_input = "", n = 2) {
  dots <- list(...)
  if(length(dots) > 0) msg_input <- gsub(",$", "", paste(dots, collapse = ","))
  if(getOption("teproj.print.wrn")) .warningf("Required input `%s`is missing.", msg_input, n = n)
}

.print_nofile_msg <- function(...,  msg_input = "", n = 2) {
  dots <- list(...)
  if(length(dots) > 0) msg_input <- gsub(",$", " ", paste(dots, collapse = ","))
  if(getOption("teproj.print.wrn")) .warningf("Could not find any files meeting criteria `%s`.", msg_input, n = n)
}

.print_argfalse_msg <- function(arg) {
  parent.call <- sys.call(sys.nframe() - 1L)
  if(getOption("teproj.print.msg")) message("Returning nothing because `", arg, " = FALSE`.")
}

.print_dpc_msg <- function(f) {
  parent.call <- sys.call(sys.nframe() - 1L)
  if(missing(f)) f <- as.character(NULL)
  if(getOption("teproj.print.msg")) message("This function is deprecated. Use ", f, "instead.")
}

.print_usedefault_msg <-
  function(arg, arg_name = deparse(substitute(arg))) {
    parent.call <- sys.call(sys.nframe() - 1L)
    if(getOption("teproj.print.msg")) message("Using ", arg, " for ", arg_name, ".")
  }

.print_ignore_msg <-
  function(..., msg_input = "") {
    parent.call <- sys.call(sys.nframe() - 1L)
    dots <- list(...)
    if(length(dots) > 0) msg_input <- gsub(",$", " ", paste(names(dots), collapse = ","))
    if(getOption("teproj.print.msg")) message("Ingoring parameters: ", msg_input, ".")
  }

.print_nonreadr_msg <- function(pkg, ..., msg_input = "", n = 2) {
  dots <- list(...)
  if(length(dots) > 0) msg_input <- gsub(",$", " ", paste(dots, collapse = ","))
  if(getOption("teproj.print.msg")) message("Used `", pkg, "` method instead of `readr` method.")
}

.print_export_msg <- function(filepath) {
  if (getOption("teproj.print.msg"))  message("Saved ", basename(filepath), " as ", filepath, ".")
}

