test_that("class_at works", {
  e = c(name = "character", height = "integer", mass = "numeric")
  expect_identical(class_at(dplyr::starwars, 1:3), e)
})

test_that("filter_duplicated works", {
  df = data.frame(x = c(0, 0, 0), y = c(1, 1, 2))
  expect_identical(filter_duplicated(df), df[1:2, ])
})

test_that("crossing_rep works", {
  x = seq_len(3L)
  times = 2L
  expect_silent({
    y = crossing_rep(x, times)
  })
  expect_identical(dim(y), c(as.integer(length(x)**2L), times))
})
