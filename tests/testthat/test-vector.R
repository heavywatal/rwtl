test_that("twist() works", {
  x = seq_len(5L)
  expect_identical(twist(x, 0L), x)
  expect_identical(twist(x, 2L), c(3L, 4L, 5L, 1L, 2L))
  expect_identical(twist(x, -2L), c(4L, 5L, 1L, 2L, 3L))
})

test_that("sort_numeric() works", {
  x = c("chr10", "chr9", "chrMt", "chrPt")
  expect_identical(sort(x), x)
  expect_identical(sort_numeric(x), c("chr9", "chr10", "chrMt", "chrPt"))
  expect_identical(sort_numeric(3:1), 1:3)
})

test_that("as_factor_inseq() works", {
  x = c("chr10", "chr9", "chrMt", "chrPt")
  as.factor(x) |>
    levels() |>
    expect_identical(x)
  as_factor_numeric(x) |>
    expect_equal(c(2L, 1L, 3L, 4L), ignore_attr = TRUE) |>
    levels() |>
    expect_identical(c("chr9", "chr10", "chrMt", "chrPt"))
  as_factor_numeric(as.factor(x)) |>
    expect_equal(c(2L, 1L, 3L, 4L), ignore_attr = TRUE) |>
    levels() |>
    expect_identical(c("chr9", "chr10", "chrMt", "chrPt"))
})

test_that("split_consecutive() works", {
  e = list(1:3, 5:8, 10:14)
  x = unlist(e)
  expect_identical(split_consecutive(x), e)
})
