

context("utils")

test_that("create_dir", {
  dir <- "temp"
  create_dir(dir)
  expect_true(dir.exists(dir))
  unlink(dir, recursive = TRUE)

})
