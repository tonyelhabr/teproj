
context('io-ext')
require('datasets')

test_that('csv', {

  path <- export_ext_csv(iris)
  expect_true(file.exists(path))
  unlink(path)

  path <- export_ext(iris, ext = 'csv')
  expect_true(file.exists(path))
  # unlink(path)

  # path <- paste0(getwd(), '/tests/testthat/iris.csv')
  expected <- iris
  actual <- import_ext_csv(iris)
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext(iris, ext = 'csv')
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext_csv(path = path)
  expect_identical(nrow(actual), nrow(expected))

  unlink(path)
})

test_that('xlsx', {
  # path <- export_ext_xlsx(iris)
  # expect_true(file.exists(path))
  # unlink(path)

  path <- export_ext(iris, ext = 'xlsx')
  expect_true(file.exists(path))
  # unlink(path)

  expected <- iris
  actual <- import_ext_xlsx(iris)
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext_xlsx(iris)
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext(iris, ext = 'xlsx')
  expect_identical(nrow(actual), nrow(expected))

  unlink(path)
})

test_that('rds', {
  path <- export_ext_rds(iris)
  expect_true(file.exists(path))
  unlink(path)

  path <- export_ext(iris, ext = 'rds')
  expect_true(file.exists(path))
  # unlink(path)

  expected <- iris
  actual <- import_ext_rds(iris)
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext(iris, ext = 'rds')
  expect_identical(nrow(actual), nrow(expected))

  actual <- import_ext_rds(path = path)
  expect_identical(nrow(actual), nrow(expected))

  unlink(path)
})

test_that('rda', {
  rm(list = ls())
  iris <- iris
  n <- c(1, 2)
  ch <- c('a', 'b')
  m <- matrix(1:4, nrow = 2)
  l <- list(a = 1, b = 2)
  path_rda <- export_ext_rda(file = 'iris')
  path_rdata <- export_ext_rdata(file = 'iris')
  ls <- ls()

  expect_true(file.exists(path_rda))

  rm(list = setdiff(ls(), c('ls', 'path_rda', 'path_rdata')))
  import_ext_rda(path = path_rda)
  # setdiff(ls, ls())
  # setdiff(ls(), ls)
  setdiff(ls(), c(ls, 'ls'))
  expect_true(length(setdiff(ls(), c(ls, 'ls'))) == 0)

  rm(list = setdiff(ls(), c('ls', 'path_rda', 'path_rdata')))
  import_ext_rdata(path = path_rdata)
  expect_true(length(setdiff(ls(), c(ls, 'ls'))) == 0)

  rm(list = setdiff(ls(), c('ls', 'path_rda', 'path_rdata')))
  import_ext(path = path_rda, ext = 'rda')
  expect_true(length(setdiff(ls(), c(ls, 'ls'))) == 0)

  rm(list = setdiff(ls(), c('ls', 'path_rda', 'path_rdata')))
  import_ext(path = path_rdata, ext = 'RData')
  expect_true(length(setdiff(ls(), c(ls, 'ls'))) == 0)

  unlink(path_rda)
  unlink(path_rdata)
  rm(list = ls())

})

suppressWarnings(require('ggplot2'))
test_that('png', {
  viz_iris <-
    ggplot2::qplot(data = iris, x = Petal.Length, y = Petal.Width)
  path <- export_ext_png(viz_iris)
  # TODO: Need to figure out how to check for messages...
  # evaluate_promise(export_ext_viz(viz_iris), print = TRUE)
  expect_true(file.exists(path))
  unlink(path)

  path <- export_gg(viz_iris)
  expect_true(file.exists(path))
  unlink(path)

  path <- export_ext(viz_iris, ext = 'png')
  expect_true(file.exists(path))
  unlink(path)
})
