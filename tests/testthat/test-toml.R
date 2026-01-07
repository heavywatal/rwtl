test_that("toTOML work", {
  withr::local_options(readr.default_locale = readr::locale(tz = "Asia/Tokyo"))
  lst = list(
    integer = 42L,
    double = 42,
    logical = TRUE,
    character = "char",
    date = readr::parse_date("2006-01-02"),
    datetime = readr::parse_datetime("2006-01-02 15:04:05"),
    zchar = "",
    null = character(),
    na = NA
  )
  # fmt: skip
  toml = trimws('
integer = 42
double = 42
logical = true
character = "char"
date = 2006-01-02
datetime = 2006-01-02T{time}
zchar = ""
')
  expect_identical(toTOML(lst), glue(toml, time = "15:04:05+09:00"))
  expect_identical(toTOML(lst, usetz = FALSE), glue(toml, time = "15:04:05"))
  expect_identical(toTOML(lst, usetz = "Z"), glue(toml, time = "06:04:05Z"))
})
