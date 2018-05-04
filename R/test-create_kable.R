

context("create_kable")
require("datasets")

test_that("create_kable", {
  html_iris <- create_kable(iris)
  expect_equal(class(html_iris), c("kableExtra", "knitr_kable"))
  df <- iris
  md_iris <- create_kable_md(df)
  expect_equal(class(html_iris)[1], "kableExtra")

})