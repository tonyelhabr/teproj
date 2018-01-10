

context("project-related functions")
require("bindrcpp")
require("dplyr")

test_that("parse_proj_io", {
  dir <- file.path("tests", "project-parse")
  filename <- "script.R"
  if(interactiv()) {
    dir.exists(file.path("inst", dir))
  } else {
    print(dir.exists(dir))
  }
  filepath <-
    system.file(dir, filename, package = "teproj", mustWork = TRUE)
  file.exists(filepath)

  actual_df <- parse_proj_io(filepath)
  if (interactive()) {
    actual_df
    dplyr::select(actual_df, io, var, filename, ext, accuracy, comment)
  }

  expect <- 0
  actual <- nrow(actual_df)
  expect_gt(actual, expect)

  # No test for this yet.
  actual_df <-
    parse_proj_io(filepath, rgx_input = "read", rgx_output = "write")
  actual <- nrow(actual_df)
  expect_gt(actual, expect)
})

test_that("render_proj_io", {
  dir <- file.path("tests", "project-render")
  filename <- "default_template_rmd2r.R"
  if(interactiv()) {
    dir.exists(file.path("inst", dir))
  } else {
    print(dir.exists(dir))
  }
  filepath <-
    system.file(dir, filename, package = "teproj", mustWork = TRUE)
  file.exists(filepath)

  actual_df <-
    render_proj_io(
      filepaths_input = filepath,
      # dir_input = dir,
      dir_output =  "output",
      # dir_output = file.path(dir, "output"), # Use this if interactive().
      rgx_input = ".",
      rgx_output = ".",
      render = FALSE,
      return = TRUE
    )
  print(actual_df)

  expect <- 0
  # actual <- nrow(actual_df)
  # expect_gt(actual, expect)
  # unlink(list.files(dir = file.path(filepath, "output")))
})
