test_that("moment functions work", {
  v = rnorm(100000L)
  expect_equal(moment(v, 1), mean(v))
  expect_equal(moment(v, 2), var(v), tolerance = 1e-2)
  expect_equal(skewness(v), 0, tolerance = 1e-2)
  expect_equal(kurtosis(v), 3, tolerance = 1e-2)
  expect_silent(bimodality(v))
})