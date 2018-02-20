
.get_valid_example_styles <- function() {
  c("ercot", "personal")
}

.get_valid_example_templates <- function() {
  c("all", "analysis")
}

# References:
# 1. https://github.com/ropensci/ezknitr/blob/master/R/setup_src.R.
# 2. https://github.com/rstudio/blogdown/blob/master/R/hugo.R

#' Create an example project/file.
#'
#' @description Creates an example project or file.
#' @details TODO.
#' @param style character. Valid arguments include "ercot".
#' @param template character. Valid arguments include "analysis".
#' @param dir character. Name of directory to create. Default is provided.
#' @param ... dots. Not currently used.
#' @param overwrite logical. Indicates whether to overwrite existing directory or not.
#' @return character. directory filepath.
create_example <- function(style = "personal",
                           template = "analysis",
                           dir = "teproj_test",
                           ...,
                           overwrite = FALSE) {

  dir_trg <- file.path(dir)
  if(dir.exists(dir_trg) & !overwrite) {
    # print_argfalse_msg("overwrite"))
    if(getOption("teproj.print.wrn"))
       warning("Returning nothing because `overwrite == FALSE` and ", dir_trg, " exists.")
    return(invisible())
  }

  dir_src <- "examples"
  style <- match.arg(style, .get_valid_example_styles())
  template <- match.arg(template, .get_valid_example_templates())

  filenames_src <-
    list.files(system.file(dir_src, package = "teproj"), recursive = TRUE)
  # browser()

  filepaths_src <- try(system.file(dir_src, filenames_src, package = "teproj", mustWork = TRUE), silent = TRUE)
  if (class(file) == "try-error") {
    stop("Could not find example file(s).")
  }
  filepaths_src_r <- grep("\\.R$", filepaths_src, value = TRUE)
  filepaths_src_other <- grep("\\.R$", filepaths_src, value = TRUE, invert = TRUE)


  if(overwrite == TRUE) {
    unlink(list.files(dir_trg, full.names = TRUE), recursive = TRUE, force = TRUE)
    if(getOption("teproj.print.msg"))
       message("Overwrote ", dir_trg, " (because `overwrite == TRUE`).")
  }

  dir_www <- file.path(dir_trg, "www")
  dir.create(dir_www, showWarnings = FALSE, recursive = TRUE)

  file.copy(from = filepaths_src_r, to = dir_trg)
  file.copy(from = filepaths_src_other, to = dir_www)
  invisible(dir_trg)
}


