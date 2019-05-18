test_that("package utils work", {
  expect_type(has_tests(), "logical")
  expect_match(binary_if_macos(), "binary|source|both")
})
