

context("io-ext-params")
require("datasets")
require("tibble")

test_that("import NSE", {

  idx_1 <- 1
  idx_2 <- 3
  idxs <- c(idx_1:idx_2)
  df <- data.frame(one = idxs, two = letters[idxs], stringsAsFactors = FALSE)
  df2 <- tibble::as_tibble(df)
  path <- export_ext_csv(df)
  expect_true(file.exists(path))
  expected <- df
  rm("df")

  actual <- import_ext_csv(df)
  # expect_equal(actual, expected)
  expect_equivalent(actual, expected)
  # expected <- df2
  # expect_equal(actual, expected)
  unlink(path)

})

test_that("backup", {

  path <- export_ext_csv(iris)
  unlink(path)
  path_2 <- gsub("\\.csv", "_2\\.csv", path)
  path <- export_ext(iris, ext = "csv", backup = TRUE, path_backup = path_2)
  expect_true(file.exists(path))
  expect_true(file.exists(path_2))
  unlink(path)
  unlink(path_2)

})

test_that("overwrite", {

  path <- export_ext_csv(iris)
  expect_true(file.exists(path))
  path_2 <- export_ext(iris, ext = "csv", overwrite = TRUE)
  expect_equal(path, path_2)
  expect_true(file.exists(path))
  unlink(path)
  # unlink(path_2)

})

test_that("ggsave params", {

  viz_iris <-
    ggplot2::qplot(data = iris, x = Petal.Length, y = Petal.Width)
  path <- export_ext_png(viz_iris)
  # TODO: Need to figure out how to check for messages...
  expect_message(export_ext_png(viz_iris, units = "cm"))
  unlink(path)

})


