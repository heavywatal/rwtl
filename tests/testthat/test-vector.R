test_that("twist() works", {
  x = seq_len(5L)
  expect_equal(twist(x, 0L), x)
  expect_equal(twist(x, 2L), c(3L, 4L, 5L, 1L, 2L))
  expect_equal(twist(x, -2L), c(4L, 5L, 1L, 2L, 3L))
})
