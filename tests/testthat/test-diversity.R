test_that("shannon_index works", {
  expect_identical(shannon_index(numeric(0L)), 0)
  expect_identical(shannon_index(1), 0)
})

test_that("simpson_index works", {
  expect_identical(simpson_index(numeric(0L)), 0)
  expect_identical(simpson_index(1), 1)
  expect_identical(simpson_index(c(1, 1)), 0.5)
})

test_that("evenness works", {
  expect_identical(evenness(numeric(0L)), 0)
  expect_identical(evenness(1), NaN)
})
