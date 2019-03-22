context("test-print")

test_that("printdf works", {
  expect_output(printdf(NULL), "NULL")
  expect_output(printdf(letters), "chr")
  expect_output(printdf(data.frame(x = numeric(0L))), "data.frame \\[0 x 1\\]")
  expect_output(printdf(VADeaths), "matrix \\[5 x 4\\]")
  expect_output(printdf(iris), "data.frame \\[150 x 5\\]")
  expect_output(printdf(iris %>% group_by(Species)), "Groups: Species \\[3\\]")
  expect_output(printdf(dplyr::starwars), "tbl_df")
  expect_output(printdf(ggplot2::diamonds), "<ord>")
  potus = ggplot2::presidential %>% dplyr::mutate(end = as.POSIXct(end))
  expect_output(printdf(potus), "<date>\\s+<dttm>")
})

test_that("max_print works", {
  tiris = tibble::as_tibble(iris)
  expect_output(max_print(tiris), "100")
})

test_that("print option works", {
  expect_equal(sanitize_width(0L), 10L)
  expect_equal(sanitize_width(42L), 42L)
  expect_equal(sanitize_width(99999L), 10000L)
  expect_equal(sanitize_width(NULL), getOption("width"))
})
