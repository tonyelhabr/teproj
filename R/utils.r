

# NOTE: This function is to be used only internally by some functions in this package.
check_files_exist <- function(paths, dir, pattern, ...) {
  # Debugging...
  # path = "."

  # Assumption...
  exist <- FALSE
  ret <- ""
  # browser()
  if(missing(paths) & missing(dir)) {
    print_ismiss_msg()

  } else if (missing(paths) & !missing(dir)) {
    # TODO: For some reason include.dirs = FALSE does not produce expected output?
    paths <-
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
    paths <- setdiff(paths, dirs)
    # Or...
    # # Add a file extension to the pattern.
    # paths <-
    #   list.files(
    #     path = dir,
    #     # ...,
    #     # pattern = paste0(pattern, "(\\.*$"),
    #     # recursive = FALSE,
    #     full.names = TRUE,
    #     ignore.case = TRUE,
    #     include.dirs = FALSE
    #   )
    if(length(paths) == 0) {
      print_nofile_msg()
    } else {
      exist <- TRUE
      ret <- paths
    }
  } else {
    paths_exist <- as.logical(lapply(paths, file.exists))
    if(!any(paths_exist)) {
      warning("Specified files do not exist.")
    } else {
      exist <- TRUE
      ret <- paths
    }
  }
  list(exist = exist, paths = ret)
}
