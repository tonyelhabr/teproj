
context("dplyr-combos")
require("datasets")
# testthat::test_file("tests/testthat/test-dplyr-combos.R")

test_that("dplyr-combos", {
  actual <- pull_distinctly(mtcars, carb)
  expect <- sort(unique(mtcars$carb))
  expect_equal(actual, expect)

  actual <- arrange_distinctly(mtcars, carb)
  expect <- data.frame(carb = sort(unique(mtcars$carb)))
  expect_equal(actual, expect)

  expect_df <- mtcars
  actual_df <- summarise_stats(mtcars, mpg)
  expect_equal(c(as.matrix(actual_df["mean"])), mean(mtcars$mpg))

  actual_df <- summarise_stats_at(mtcars, "mpg")
  expect_equal(c(as.matrix(actual_df["mean"])), mean(mtcars$mpg))

  actual_df <- summarise_stats(mtcars, mpg, tidy = TRUE)
  expect_equal(ncol(actual_df), 2)

  expect_equal(names(actual_df)[1], "stat")

  actual_df <- summarise_stats_by_at(mtcars, col = "mpg", cols_grp = c("cyl"), tidy = TRUE)
  expect_equal(ncol(actual_df), 3)

  actual_df <- summarise_stats_by_at(mtcars, col = "mpg", cols_grp = c("cyl", "gear"), tidy = TRUE)
  expect_equal(ncol(actual_df), 4)

})