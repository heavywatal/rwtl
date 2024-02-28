test_that("rle_df works", {
  v = c(0, 0, 0, 1, 1, 2)
  expected = tibble::new_tibble(list(lengths = c(3L, 2L, 1L), values = c(0, 1, 2)))
  expect_identical(rle_df(v), expected)
})

test_that("rle_ranges works", {
  v = c(0, 0, 0, 1, 1, 2)
  expected = tibble::new_tibble(list(
    start = c(1L, 4L, 6L),
    end = c(3L, 5L, 6L),
    v = c(0, 1, 2)
  ))
  expect_identical(rle_ranges(v), expected)
  expect_identical(rle_ranges(value = v), dplyr::rename(expected, value = v))
})

test_that("table_df works", {
  n = c(3L, 2L, 1L)
  x = c(0, 1, 2)
  v = rep(x, times = n)
  expect_identical(table_df(v), tibble::new_tibble(list(v = x, n = n)))
  expect_identical(table_df(value = v), tibble::new_tibble(list(value = x, n = n)))
  x = as.integer(x)
  v = rep(x, times = n)
  expect_identical(table_df(v), tibble::new_tibble(list(v = x, n = n)))
  expect_identical(table_df(value = v), tibble::new_tibble(list(value = x, n = n)))
  x = as.character(x)
  v = rep(x, times = n)
  expect_identical(table_df(v), tibble::new_tibble(list(v = x, n = n)))
  expect_identical(table_df(value = v), tibble::new_tibble(list(value = x, n = n)))
})

test_that("tabulate0 works", {
  x = c(2L, 3L, 3L, 5L)
  expect_identical(tabulate(x), c(0L, 1L, 2L, 0L, 1L))
  expect_identical(tabulate0(x), c(0L, tabulate(x)))
  expect_identical(tabulate0(x, nbins = 6L), c(0L, tabulate(x), 0L))
  e = tibble::new_tibble(list(x = seq.int(0L, max(x)), n = c(0L, tabulate(x))))
  expect_identical(tabulate0_df(x), e)
})
