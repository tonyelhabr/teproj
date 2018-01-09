
#' @title Control package messages.
#' @description Sets options for relevant `teproj` functions.
#' @details Intended to be used as a wrapper to `options(...`).
#' @param msg boolean.
#' @param wrn boolean.
#' @param err boolean.
#' @export
set_pkg_print_opts <- function(msg = getOption("teproj.print_msg"),
                               wrn = getOption("teproj.print_wrn"),
                               err = getOption("teproj.print_err")) {
  options(teproj.print_msg = msg)
  options(teproj.print_wrn = wrn)
  options(teproj.print_err = err)
}

#' @title Control package render settings.
#' @description Sets options for relevant `teproj` functions.
#' @details Intended to be used as a wrapper to `options(...`).
#' @export
set_pkg_render_opts <-
  function() {
  options(teproj.render_echo = FALSE)
  options(teproj.render_cache = FALSE)
  options(teproj.render_results = "hide")
  options(teproj.render_fig_align = "center")
  options(teproj.render_fig_show = "hide")
  options(teproj.render_width = 100)
  options(teproj.render_fig.width = 10)
  options(teproj.render_fig_height = 10)
  options(teproj.render_warning = FALSE)
  options(teproj.render_message = FALSE)
}

#' @title Create a directory.
#' @description Creates the directory if it does not exist.
#' @details Used by other functions in this package.
#' @param dir character. Folder name (with a trailing slash),
#' @param overwrite boolean.
#' @param backup boolean. Only relevant if `dir` exists and `overwrite == TRUE`.
#' @param create boolean. Intended to be used with a global `keep_*` parameter.
#' @param ... dots. Additional parameters to pass to `dir.create()`.
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
    if (getOption("teproj.print_msg"))
      message(dir, " already exists.")
    if(overwrite) {
      # unlink(list.files(dir, full.names = TRUE), recursive = TRUE, force = TRUE)
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      if(getOption("teproj.print_msg"))
        message("Overwrote ", dir, " (because `overwrite == TRUE`).")
    }
  } else {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (getOption("teproj.print_msg")) {
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
      filepath <- paste0(dir, filename, ".", ext)
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
  if(missing(filepaths) && missing(dir)) {
    .print_ismiss_msg()

  } else if (missing(filepaths) && !missing(dir)) {
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

# TODO: Implement proper message/warning/error wrappers.
# See https://stackoverflow.com/questions/9596918/r-warning-wrapper-raise-to-parent-function.
warningf <- function(..., n = 2L){
  parent_call <- sys.call(sys.nframe() - n)
  warning(paste("In", parent_call, ":", sprintf(...)), call. = FALSE)
}

# messagef <- function(..., n = 2L){
#   parent.call <- sys.call(sys.nframe() - n)
#   message(paste("In", parent.call, ":", sprintf(...)))
# }

.print_isnull_msg <- function(...,  msg_input = "") {
  parent.call <- sys.call(sys.nframe() - 1L)
  dots <- list(...)
  if(length(dots) > 0) msg_input <- gsub(", $", "", paste(dots, collpase = ", "))
  if(getOption("teproj.print_wrn")) warningf("Required input %sis null.", msg_input, n = 2)
}

.print_ismiss_msg <- function(...,  msg_input = "") {
  parent.call <- sys.call(sys.nframe() - 1L)
  dots <- list(...)
  if(length(dots) > 0) msg_input <- gsub(", $", "", paste(dots, collpase = ", "))
  if(getOption("teproj.print_wrn")) warningf("Required input %sis missing.", msg_input, n = 2)
}

.print_nofile_msg <- function(...,  msg_input = "") {
  parent.call <- sys.call(sys.nframe() - 1L)
  dots <- list(...)
  if(length(dots) > 0) msg_input <- gsub(", $", "", paste(c(" ", dots), collpase = ", "))
  if(getOption("teproj.print_wrn")) warningf("Could not find any files meeting criteria%s.", msg_input, n = 2)
}

.print_argfalse_msg <- function(arg) {
  parent.call <- sys.call(sys.nframe() - 1L)
  if(getOption("teproj.print_msg")) message("Returning nothing because `", arg, " = FALSE`.")
}

.print_dpc_msg <- function(f) {
  parent.call <- sys.call(sys.nframe() - 1L)
  if(missing(f)) f <- as.character(NULL)
  if(getOption("teproj.print_msg")) message("This function is deprecated. Use ", f, "instead.")
}

.print_usedefault_msg <-
  function(arg, arg_name = deparse(substitute(arg))) {
    parent.call <- sys.call(sys.nframe() - 1L)
    if(getOption("teproj.print_msg")) message("Using ", arg, " for ", arg_name, ".")
  }

