context("test-diversity.R")

test_that("shannon_index", {
  expect_equal(shannon_index(c(1)), 0)
})

test_that("simpson_index", {
  expect_equal(simpson_index(c(1)), 1)
  expect_equal(simpson_index(c(1, 1)), 0.5)
})
