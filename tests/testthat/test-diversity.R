test_that("shannon_index works", {
  expect_equal(shannon_index(numeric(0L)), 0)
  expect_equal(shannon_index(c(1)), 0)
})

test_that("simpson_index works", {
  expect_equal(simpson_index(numeric(0L)), 0)
  expect_equal(simpson_index(c(1)), 1)
  expect_equal(simpson_index(c(1, 1)), 0.5)
})

test_that("evenness works", {
  expect_equal(evenness(numeric(0L)), 0)
  expect_equal(evenness(c(1)), NaN)
})
