test_that("*_left work", {
  x = data.frame(y = 2L, z = 3L)
  e = data.frame(z = 3L, y = 2L)
  expect_identical(move_left(x, z), e)
  e = data.frame(x = 1L, y = 2L, z = 3L)
  expect_identical(mutate_left(x, x = 1L), e)
})

test_that("append_df works", {
  x = data.frame(x = 1L, z = 3L)
  values = data.frame(y = 2L)
  e = data.frame(x = 1L, y = 2L, z = 3L)
  expect_identical(append_df(x, values, 1L), e)
})

test_that("class_at works", {
  e = c(name = "character", height = "integer", mass = "numeric")
  expect_identical(class_at(dplyr::starwars, 1:3), e)
})

test_that("crossing_rep works", {
  x = seq_len(3L)
  times = 2L
  expect_silent({
    y = crossing_rep(x, times)
  })
  expect_identical(dim(y), c(as.integer(length(x)**2L), times))
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
