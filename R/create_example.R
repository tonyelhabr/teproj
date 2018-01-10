


.get_valid_example_styles <- function() {
  c("ercot", "personal")
}

.get_valid_example_templates <- function() {
  c("all", "analysis")
}

# References:
# 1. https://github.com/ropensci/ezknitr/blob/master/R/setup_src.R.
# 2. https://github.com/rstudio/blogdown/blob/master/R/hugo.R
# 3. create.project(), .create.project.new(), .get.template.dir(), .list.files.and.dirs(), etc.
# at https://github.com/johnmyleswhite/ProjectTemplate

#' @title Create an example project/file.
#' @description Creates an example project or file.
#' @details TODO.
#' @param style Character. Default: "personal"
#' @param template Character. Default: "all"
#' @param dir Character. Name of directory to create. Default = "project_exmample"
#' @param ... Dots. Parameters passed to ???
#' @param overwrite Boolean. Indicates whether to overwrite or not.
#' @return Character. Directory filepath.
create_example_OLD <-
  function(style = "personal",
           template = "analysis",
           dir = "project_example",
           ...,
           overwrite = TRUE) {
    # # Debugging...
    # style = "personal"
    # template = "all"
    # dir = "../project_example/"
    # overwrite = FALSE
    # # For debugging...
    # dir_src <- "inst/examples/"
    # # For real...
    dir_src <- "examples/"

    style <- match.arg(style, .get_valid_example_styles())
    template <- match.arg(template, .get_valid_example_templates())

    dir_trg <- dir
    create_dir(dir_trg, overwrite = FALSE)

    list.files(dir_src, include.dirs = TRUE, recursive = TRUE)
    list.dirs(dir_src, recursive = TRUE, full.names = FALSE)

    # dir_src_filenames <- list.files(dir_src, recursive = TRUE)
    dir_src_filenames <-
      list.files(system.file(dir_src, package = "teproj"), recursive = TRUE)
    dir_src_dirnames <-
      list.dirs(
        system.file(dir_src, package = "teproj"),
        recursive = TRUE,
        full.names = FALSE
      )
    # dir_src_dirnames <- setdiff(dir_src_dirnames, "")

    if (template ==  "analysis") {
      grep("analy|www/", dir_src_filenames, value = TRUE)
      dir_src_filenames <-
        grep("analy|www/", dir_src_filenames, value = TRUE)
    }
    file.path(dir_src, dir_src_dirnames)
    gsub("inst\\/", "", dir_src)
    system.file(
      gsub("inst\\/", "", dir_src),
      dir_src_filenames,
      package = "teproj",
      mustWork = TRUE
    )

    gsub("inst\\/|examples\\/", "", dir_src)
    dir_src_dirs <-
      file.path(gsub("inst\\/|examples\\/", "", dir_src), dir_src_dirnames)
    gsub("inst\\/|examples\\/", "", dir_src_dirs)
    dir_trg_dirs <-
      file.path(dir_trg, gsub("inst\\/|examples\\/", "", dir_src_dirs))
    dir_src_filepaths <- file.path(dir_src, dir_src_filenames)

    browser()
    create_dir(dir_trg_dirs[1])
    file.copy(
      from = normalizePath(getwd(), dir_src_dirs[1]),
      to = dir_trg_dirs[1],
      recursive = TRUE
    )
    # unlink(dir_trg_filenames_extra, recursive = TRUE, force = TRUE)
    invisible(dir_trg)
  }

#' @inheritParams create_example_OLD
#' @export
create_example <- function(style = "personal",
                           template = "analysis",
                           dir = "project_example",
                           ...,
                           overwrite = TRUE) {
  # # Debugging...
  # style = "personal"
  # template = "all"
  # dir = "../project_example/"
  # overwrite = FALSE
  # # For debugging...
  # dir_src <- "inst/examples/"
  # # For real...
  dir_src <- "examples/"

  style <- match.arg(style, .get_valid_example_styles())
  template <- match.arg(template, .get_valid_example_templates())

  dir_trg <- dir
  create_dir(dir_trg, overwrite = FALSE)

  list.files(dir_src, include.dirs = TRUE, recursive = TRUE)
  list.dirs(dir_src, recursive = TRUE, full.names = FALSE)


}
