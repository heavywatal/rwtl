test_that("easierprof works", {
  expect_type(easierprof(1 + 1), "list")
})

test_that("getenv works", {
  expect_s3_class(getenv(), c("tbl_df", "tbl", "data.frame"))
  expect_named(getenv("^R_"), c("key", "value"))
})
