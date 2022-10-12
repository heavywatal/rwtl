test_that("setdefault works", {
  v = list(abc = 1)
  expect_identical(setdefault(v, abc = 2), v)
  expect_identical(setdefault(v, def = 2), list(abc = 1, def = 2))
})

test_that("update_ works", {
  v = list(abc = 1)
  expect_identical(update_(v, abc = 2), list(abc = 2))
  expect_identical(update_(v, def = 2), list(abc = 1, def = 2))
})

test_that("select_list works", {
  v = list(abc = 1, def = 2)
  expect_identical(select_list(v, 1:2), v)
  expect_identical(select_list(v, abc:def), v)
  expect_identical(select_list(v, abc, def), v)
  expect_identical(select_list(v, abc), list(abc = 1))
  expect_identical(select_list(v, matches("^a")), list(abc = 1))
})
