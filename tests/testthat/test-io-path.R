
# testthat::test_file(file.path("tests", "testthat", "test-io-path.R"))

context("io-path")
require("datasets")
suppressWarnings(require("janitor"))

test_that("csv", {

  # NOTE: Do this just to get the path string.
  path <- export_ext_csv(iris)
  unlink(path)

  path <- export_path(iris, path)
  expect_true(file.exists(path))
  unlink(path)

  path <- export_path(iris)
  expect_true(file.exists(path))
  # unlink(path)

  # path <- paste0(getwd(), "/tests/testthat/iris.csv")
  expected <- iris
  actual <- import_path(path)
  expect_identical(nrow(actual), nrow(expected))

  expected <- names(janitor::clean_names(iris))
  actual <- names(import_path_cleanly(path))
  expect_identical(expected, actual)

  unlink(path)
})

suppressWarnings(require("ggplot2"))
test_that("png", {

  viz_iris <-
    ggplot2::qplot(data = iris, x = Petal.Length, y = Petal.Width)

  # NOTE: Do this just to get the path string.
  path <- export_ext_png(viz_iris)
  unlink(path)

  path <- export_path(viz_iris, path)
  expect_true(file.exists(path))
  unlink(path)

  path <- export_path(viz_iris)
  expect_true(file.exists(path))
  unlink(path)
})