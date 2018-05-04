

get_valid_example_styles <- function() {
  c("ercot", "personal")
}

get_valid_example_templates <- function() {
  c("all", "analysis")
}

# References:
# 1. https://github.com/ropensci/ezknitr/blob/master/R/setup_src.R.
# 2. https://github.com/rstudio/blogdown/blob/master/R/hugo.R

#' Create an example project/file
#'
#' @description Creates an example project or file.
#' @details TODO. It is intended that this function can be used to automatically
#' create a project directory based on this package's `inst/` files.
#' @param style character. Valid arguments include "ercot".
#' @param template character. Valid arguments include "analysis".
#' @param dir character. Name of directory to create. Default is provided.
#' @param ... dots. Not currently used.
#' @param overwrite logical. Indicates whether to overwrite existing directory or not.
#' @return character. directory path.
create_example <- function(style = "personal",
                           template = "analysis",
                           dir = "teproj_test",
                           ...,
                           overwrite = FALSE) {
  dir_trg <- file.path(dir)
  if (dir.exists(dir_trg) & !overwrite) {
    # print_argfalse_msg("overwrite"))
    warning(sprintf(
      "Returning nothing because `overwrite == FALSE` and %s exists.",
      dir_trg
    ))
    return(invisible())
  }

  dir_src <- "examples"
  style <- match.arg(style, get_valid_example_styles())
  template <- match.arg(template, get_valid_example_templates())

  basenames_src <-
    list.files(system.file(dir_src, package = "teproj"), recursive = TRUE)
  # browser()

  paths_src <-
    try(system.file(dir_src,
                    basenames_src,
                    package = "teproj",
                    mustWork = TRUE),
        silent = TRUE)
  if (class(file) == "try-error") {
    stop("Could not find example file(s).")
  }
  paths_src_r <- grep("\\.R$", paths_src, value = TRUE)
  paths_src_other <-
    grep("\\.R$", paths_src, value = TRUE, invert = TRUE)


  if (overwrite == TRUE) {
    unlink(list.files(dir_trg, full.names = TRUE),
           recursive = TRUE,
           force = TRUE)
    message(sprintf("Overwriting %s (because `overwrite == TRUE`).", dir_trg))
  }

  dir_www <- file.path(dir_trg, "www")
  dir.create(dir_www, showWarnings = FALSE, recursive = TRUE)

  file.copy(from = paths_src_r, to = dir_trg)
  file.copy(from = paths_src_other, to = dir_www)
  invisible(dir_trg)
}
