test_that("toTOML work", {
  withr::local_options(readr.default_locale = locale(tz = "Asia/Tokyo"))
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
  toml = '
integer = 42
double = 42
logical = true
character = "char"
date = 2006-01-02
datetime = 2006-01-02T15:04:05+09:00
zchar = ""
' |> trimws()
  expect_identical(toTOML(lst), toml)
})
