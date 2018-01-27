

context("ext")
require("datasets")
require("tibble")

test_that("import NSE", {

  # This is an explicity test not using an internal data set (i.e. iris, etc.)
  idx_1 <- 1
  idx_2 <- 3
  idxs <- c(idx_1:idx_2)
  df <- data.frame(one = idxs, two = letters[idxs], stringsAsFactors = FALSE)
  df2 <- tibble::as_tibble(df)
  filepath <- export_ext_csv(df)
  expect_true(file.exists(filepath))
  expected <- df
  rm("df")

  actual <- import_ext_csv(df)
  # expect_equal(actual, expected)
  expect_equivalent(actual, expected)
  # expected <- df2
  # expect_equal(actual, expected)
  unlink(filepath)

})

test_that("backup", {

  filepath <- export_ext_csv(iris)
  unlink(filepath)
  filepath_2 <- gsub("\\.csv", "_2\\.csv", filepath)
  filepath <- export_ext(iris, ext = "csv", backup = TRUE, filepath_backup = filepath_2)
  expect_true(file.exists(filepath))
  expect_true(file.exists(filepath_2))
  unlink(filepath)
  unlink(filepath_2)

})

test_that("overwrite", {

  filepath <- export_ext_csv(iris)
  expect_true(file.exists(filepath))
  filepath_2 <- export_ext(iris, ext = "csv", overwrite = TRUE)
  expect_equal(filepath, filepath_2)
  expect_true(file.exists(filepath))
  unlink(filepath)
  # unlink(filepath_2)

})

test_that("ggsave params", {

  viz_iris <-
    ggplot2::qplot(data = iris, x = Petal.Length, y = Petal.Width)
  filepath <- export_ext_png(viz_iris)
  # TODO: Need to figure out how to check for messages...
  expect_message(export_ext_png(viz_iris, units = "cm"))
  unlink(filepath)

})
