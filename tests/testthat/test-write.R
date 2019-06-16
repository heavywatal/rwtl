test_that("save_as() works", {
  dir = tempdir()
  path = file.path(dir, "mtcars.rds")
  expect_output(save_as(mtcars, dir), path)
  expect_true(file.exists(path))
  expect_equal(readRDS(path), mtcars)
})
