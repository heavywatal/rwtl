test_that("rle_df works", {
  v = c(0, 0, 0, 1, 1, 2)
  expected = tibble::tibble(lengths = c(3L, 2L, 1L), values = c(0, 1, 2))
  expect_identical(rle_df(v), expected)
})

test_that("rle_ranges works", {
  v = c(0, 0, 0, 1, 1, 2)
  expected = tibble::tibble(
    start = c(1L, 4L, 6L),
    end = c(3L, 5L, 6L),
    v = c(0, 1, 2)
  )
  expect_identical(rle_ranges(v), expected)
  expect_identical(rle_ranges(value = v), dplyr::rename(expected, value = v))
})

test_that("table_df works", {
  v = c(0, 0, 0, 1, 1, 2)
  expected = tibble::tibble(v = c(0, 1, 2), n = c(3L, 2L, 1L))
  expect_identical(table_df(v), expected)
  expect_identical(table_df(value = v), dplyr::rename(expected, value = v))
})
