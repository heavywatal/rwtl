test_that("class_at works", {
  e = c(name = "character", height = "integer", mass = "numeric")
  expect_equal(class_at(dplyr::starwars, dplyr::vars(1:3)), e)
})

test_that("crossing_rep works", {
  x = seq_len(3L)
  times = 2L
  expect_silent({
    y = crossing_rep(x, times)
  })
  expect_equal(dim(y), c(length(x) ** 2L, times))
})

test_that("rle works", {
  v = c(0, 0, 0, 1, 1, 2)
  expected = tibble::tibble(
    value = c(0, 1, 2),
    start = c(1L, 4L, 6L),
    end = c(3L, 5L, 6L)
  )
  expect_identical(rle_df(v), expected)
})
