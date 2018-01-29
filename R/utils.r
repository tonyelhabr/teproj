

# Modified from https://github.com/yihui/knitr/blob/master/R/utils.R.
# .is_windows <- function() .Platform$OS.type == "windows"
# .is_abs_path <- function(x) {
#   if (.is_windows())
#     grepl(':', x, fixed = TRUE) || grepl('^\\\\', x) else grepl('^[/~]', x)
# }

# NOTE: This is used by *_proj_io() functions.
# Modified from https://github.com/rstudio/rmarkdown/blob/81c209271e06266115e08263dbcde5d007e4d77c/R/includes.R
.normalize_path <- function(path, winslash = "/", mustWork = NA) {
  if (!is.null(path))
    normalizePath(path, winslash = winslash, mustWork = mustWork)
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
      if(getOption("teutils.print.wrn")) {
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

