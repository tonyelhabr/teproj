

context("project-related functions")
require("bindrcpp")
require("dplyr")

test_that("render_proj_io", {
  dir <- file.path("tests", "project-render")
  file <- "default_template_rmd2r.R"
  if(interactive()) {
    dir.exists(file.path("inst", dir))
  } else {
    print(dir.exists(dir))
  }
  path <-
    system.file(dir, file, package = "teproj", mustWork = TRUE)
  file.exists(path)

  actual_df <-
    render_proj_io(
      path_input = path,
      # dir_input = dir,
      dir_output =  "output",
      # dir_output = file.path(dir, "output"), # Use this if interactive().
      basename_output = "deleteme",
      rgx_input = ".",
      rgx_output = ".",
      keep_rmd = TRUE,
      render = FALSE,
      return = TRUE
    )
  print(actual_df)

  expect <- 0
  # actual <- nrow(actual_df)
  # expect_gt(actual, expect)
  # unlink(list.files(dir = file.path(path, "output")))
})
#
# test_that("parse_proj_io", {
#   dir <- file.path("tests", "project-parse")
#   file <- "script.R"
#   if(interactive()) {
#     dir.exists(file.path("inst", dir))
#   } else {
#     print(dir.exists(dir))
#   }
#   path <-
#     system.file(dir, file, package = "teproj", mustWork = TRUE)
#   file.exists(path)
#
#   actual_df <- parse_proj_io(path)
#   if (interactive()) {
#     actual_df
#     dplyr::select(actual_df, io, var, file, ext, accuracy, comment)
#   }
#
#   expect <- 0
#   actual <- nrow(actual_df)
#   expect_gt(actual, expect)
#
#   # No test for this yet.
#   actual_df <-
#     parse_proj_io(path, rgx_input = "read", rgx_output = "write")
#   actual <- nrow(actual_df)
#   expect_gt(actual, expect)
# })

# TODO:
# path <- "O:/_other/packages/teproj/inst/tests/project-parse/script.R"
# parsed <- utils::getParseData(parse(path))
# parsed_f <- parsed[parsed$parent %in% parsed$parent[grepl("SYMBOL_FUNCTION_CALL", parsed$token)], ]
# rgx_input <- "import"
# rgx_output <- "export"
# parsed[parsed$parent %in% parsed$parent[grepl("SYMBOL_PACKAGE", parsed$token)], ]
# parsed_f_input <- parsed_f[grepl(rgx_input, parsed_f$text), c("parent", "token", "text")]
# parsed_f_output <- parsed_f[grepl(rgx_output, parsed_f$text), c("parent", "token", "text")]
