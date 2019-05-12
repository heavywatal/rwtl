test_that("rle works", {
  v = c(0, 0, 0, 1, 1, 2)
  expected = tibble::tibble(
    value = c(0, 1, 2),
    start = c(1L, 4L, 6L),
    end = c(3L, 5L, 6L)
  )
  expect_identical(rle_df(v), expected)
})
