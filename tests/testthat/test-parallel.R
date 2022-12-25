test_that("mcmap works", {
  expect_identical(mcmap(1:2, identity), list(1L, 2L))
  expect_identical(mcmap_lgl(1:2, ~ .x > 1L), c(FALSE, TRUE))
  expect_identical(mcmap_int(1:2, identity), c(1L, 2L))
  expect_identical(mcmap_dbl(1:2, as.double), c(1, 2))
  expect_identical(mcmap_chr(1:2, as.character), c("1", "2"))
})
