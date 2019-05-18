test_that("package utils work", {
  expect_true(has_tests())
  expect_match(binary_if_macos(), "binary|source|both")
})
