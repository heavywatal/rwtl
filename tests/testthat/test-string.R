test_that("glue works", {
  expect_identical(glue("a {b}\n", b = "b"), "a b\n")
  expect_identical(stringr::str_glue("a {b}\n", b = "b"), "a b")
  dat = list(c = "c")
  expect_identical(glue_data(dat, "a {b} {c}\n", b = "b"), "a b c\n")
  expect_identical(stringr::str_glue_data(dat, "a {b} {c}\n", b = "b"), "a b c")
})

test_that("str_c_coalesce works", {
  a = c("a", "b", NA)
  x = c("x", NA, "z")
  expect_identical(str_c_coalesce(a, x), c("ax", "b", "z"))
  expect_identical(str_c_coalesce(a, x, sep = " "), c("a x", "b", "z"))
})

test_that("rsplit works", {
  expect_identical(rsplit("a  b\tc\nd")[[1]], c("a", "b", "c", "d"))
  expect_identical(rsplit("a  b\tc\nd", " ")[[1]], c("a", "", "b\tc\nd"))
  expect_identical(rsplit("a  b\tc\nd", n = 2)[[1]], c("a  b", "c", "d"))
})

test_that("ord and chr work", {
  expect_identical(ord("A"), 65L)
  expect_identical(ord("a"), 97L)
  expect_identical(chr(65L), "A")
  expect_identical(chr(97L), "a")
})

test_that("as_chr works", {
  expect_identical(as_chr(expr), "expr")
})

test_that("as_code works", {
  x = 1:3
  expect_identical(as_code(x), "1:3")
  expect_identical(as_code(as.numeric(x)), "c(1, 2, 3)")
  x = letters[x]
  expect_identical(as_code(x), 'c("a", "b", "c")')
})
