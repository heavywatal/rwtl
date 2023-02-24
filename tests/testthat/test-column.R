test_that("append_df works", {
  x = data.frame(x = 1L, z = 3L)
  values = data.frame(y = 2L)
  e = data.frame(x = 1L, y = 2L, z = 3L)
  expect_identical(append_df(x, values, 1L), e)
})
