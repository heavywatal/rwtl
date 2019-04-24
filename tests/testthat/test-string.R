test_that("split_chr works", {
  expect_equal(split_chr("a  b\tc\nd"), c("a", "b", "c", "d"))
  expect_equal(split_chr("a  b\tc\nd", " "), c("a", "", "b\tc\nd"))
  expect_equal(split_chr("a  b\tc\nd", n = 3), c("a", "b", "c\nd"))
})

test_that("rsplit works", {
  expect_equal(rsplit("a  b\tc\nd")[[1]], c("a", "b", "c", "d"))
  expect_equal(rsplit("a  b\tc\nd", " ")[[1]], c("a", "", "b\tc\nd"))
  expect_equal(rsplit("a  b\tc\nd", n = 2)[[1]], c("a  b", "c", "d"))
})

test_that("ord and chr work", {
  expect_equal(ord("A"), 65L)
  expect_equal(ord("a"), 97L)
  expect_equal(chr(65L), "A")
  expect_equal(chr(97L), "a")
})

test_that("as_chr works", {
  expect_equal(as_chr(expr), "expr")
})
