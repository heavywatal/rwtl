test_that("time functions work", {
  expect_match(str_today(), "^2\\d{7}$")
  expect_match(str_now(), paste0(str_today(), "_\\d{6}$"))
})
