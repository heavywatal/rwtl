context("test-print")

test_that("printdf works", {
  expect_output(printdf(iris), "data.frame")
  expect_output(printdf(dplyr::starwars), "tbl_df")
})
