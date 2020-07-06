test_that("twist() works", {
  x = seq_len(5L)
  expect_equal(twist(x, 0L), x)
  expect_equal(twist(x, 2L), c(3L, 4L, 5L, 1L, 2L))
  expect_equal(twist(x, -2L), c(4L, 5L, 1L, 2L, 3L))
})

test_that("split_consecutive() works", {
  e = list(1:3, 5:8, 10:14)
  x = unlist(e)
  expect_equal(split_consecutive(x), e)
})
