

context("create_kable")
require("datasets")

test_that("create_kable", {
  html_iris <- create_kable(iris)
  expect_equal(class(html_iris), c("kableExtra", "knitr_kable"))
  expect_equal(attr(html_iris, "format"), "html")

  md_iris <- create_kable_md(iris)
  expect_equal(attr(md_iris, "format"), "markdown")

  html_iris_n <- create_kable(iris, n_show = 2)
  # iris_arr <- dplyr::arrange(iris, dplyr::desc(Sepal.Length))
  # html_iris_filt <- create_kable_filt_at(iris_arr, col = "Species", rgx = "versicolor")
  # html_iris_filt
})