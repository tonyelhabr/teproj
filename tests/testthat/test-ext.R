
context("ext")

# if (interactive()) {
#   dir_before <- getwd()
#   dir_current <- "O:/_other/packages/teutils/tests/testthat/"
#   on.exit(setwd(dir_before), add = TRUE)
#   dir <- paste0(dir_current)
#   dir.exists(dir)
# }

test_that("import NSE", {

  df <- data.frame(one = c(1:3), two = letters[c(1:3)], stringsAsFactors = FALSE)
  filepath <- export_ext_csv(df)
  expect_true(file.exists(filepath))
  expected <- df
  rm("df")

  actual <- import_ext_csv(df)
  expect_equal(as.data.frame(actual), expected)
  unlink(filepath)

})

test_that("csv", {

  filepath <- export_ext_csv(iris)
  expect_true(file.exists(filepath))
  unlink(filepath)

  filepath <- export_ext(iris, ext = "csv")
  expect_true(file.exists(filepath))
  # unlink(filepath)

  # filepath <- paste0(getwd(), "/tests/testthat/iris.csv")
  expected <- iris
  actual <- import_ext_csv(iris)
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext(iris, ext = "csv")
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext_csv(filepath = filepath)
  expect_identical(nrow(actual), nrow(expected))

  unlink(filepath)
})

test_that("Excel", {
  filepath <- export_ext_xlsx(iris)
  expect_true(file.exists(filepath))
  unlink(filepath)

  filepath <- export_ext(iris, ext = "xlsx")
  expect_true(file.exists(filepath))
  # unlink(filepath)

  expected <- iris
  actual <- import_ext_xlsx(iris)
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_excel(iris)
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext(iris, ext = "xlsx")
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext_xlsx(filepath = filepath)
  expect_identical(nrow(actual), nrow(expected))

  unlink(filepath)
})

test_that("rds", {
  filepath <- export_ext_rds(iris)
  expect_true(file.exists(filepath))
  unlink(filepath)

  filepath <- export_ext(iris, ext = "rds")
  expect_true(file.exists(filepath))
  # unlink(filepath)

  expected <- iris
  actual <- import_ext_rds(iris)
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext(iris, ext = "rds")
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext_rds(filepath = filepath)
  expect_identical(nrow(actual), nrow(expected))

  unlink(filepath)
})

test_that("R data", {
  rm(list = ls())
  iris <- iris
  n <- c(1, 2)
  ch <- c("a", "b")
  m <- matrix(1:4, nrow = 2)
  l = list(a = 1, b = 2)
  filepath_rda <- export_ext_rda(filename = "iris")
  filepath_rdata <- export_ext_RData(filename = "iris")
  ls <- ls()

  expect_true(file.exists(filepath_rda))
  expect_true(file.exists(filepath_rdata))

  rm(list = setdiff(ls(), c("ls", "filepath_rda", "filepath_rdata")))
  import_ext_rda(filepath = filepath_rda)
  # setdiff(ls, ls())
  # setdiff(ls(), ls)
  setdiff(ls(), c(ls, "ls"))
  expect_true(length(setdiff(ls(), c(ls, "ls"))) == 0)

  rm(list = setdiff(ls(), c("ls", "filepath_rda", "filepath_rdata")))
  import_ext_RData(filepath = filepath_rdata)
  expect_true(length(setdiff(ls(), c(ls, "ls"))) == 0)

  rm(list = setdiff(ls(), c("ls", "filepath_rda", "filepath_rdata")))
  import_ext(filepath = filepath_rda, ext = "rda")
  expect_true(length(setdiff(ls(), c(ls, "ls"))) == 0)

  rm(list = setdiff(ls(), c("ls", "filepath_rda", "filepath_rdata")))
  import_ext(filepath = filepath_rdata, ext = "RData")
  expect_true(length(setdiff(ls(), c(ls, "ls"))) == 0)

  unlink(filepath_rda)
  unlink(filepath_rdata)
  rm(list = ls())

})

require("ggplot2")
test_that("png", {
  viz_iris <-
    ggplot2::qplot(data = iris, x = Petal.Length, y = Petal.Width)
  filepath <- export_ext_png(viz_iris)
  # TODO: Need to figure out how to check for messages...
  # evaluate_promise(export_ext_viz(viz_iris), print = TRUE)
  expect_true(file.exists(filepath))
  unlink(filepath)

  filepath <- export_viz(viz_iris)
  expect_true(file.exists(filepath))
  unlink(filepath)

  filepath <- export_fig(viz_iris)
  expect_true(file.exists(filepath))
  unlink(filepath)

  filepath <- export_ext(viz_iris, ext = "png")
  expect_true(file.exists(filepath))
  unlink(filepath)
})
