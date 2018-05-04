
context("utils")

test_that("create_dir", {
  dir <- "temp"
  create_dir(dir)
  expect_true(dir.exists(dir))
  unlink(dir, recursive = TRUE)

})

test_that("print_", {
  expect_warning(warningf("%i banana", 1, n = 1))
  expect_warning(print_isnull_msg("banana"))
  expect_warning(print_ismiss_msg("banana"))
  expect_warning(print_nofile_msg("banana"))
  expect_message(print_argfalse_msg("banana"))
  expect_message(print_dpc_msg("banana"))
  expect_message(print_usedefault_msg("banana"))

})
