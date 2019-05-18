test_that("time functions work", {
  expect_match(today(), "^2\\d{7}$")
  expect_match(now(), paste0(today(), "_\\d{6}$"))
})
