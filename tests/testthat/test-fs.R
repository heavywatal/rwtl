test_that("is_outdated works", {
  this_file = "test-fs.R"
  expect_false(is_outdated(this_file))
  expect_true(is_outdated("NOEXIST"))
  withr::with_tempfile("new_file", {
    fs::file_create(new_file)
    expect_true(is_outdated(new_file))
    writeLines("hello", new_file)
    expect_false(is_outdated(new_file))
    expect_false(is_outdated(new_file, this_file))
    expect_true(is_outdated(this_file, new_file))
  })
})

test_that("file_copy_try works", {
  dstdir = fs::path(tempdir())
  this_file = "test-fs.R"
  dstfile = dstdir / this_file
  expect_identical(file_copy_try(this_file, dstdir), dstfile)
  expect_identical(file_copy_try(this_file, dstdir), dstfile)
})

test_that("path_destination works", {
  srcs = c("file", "rel/file", "/abs/file")
  dstdir = fs::path(tempdir())
  dstfiles = dstdir / basename(srcs)
  expect_identical(path_destination(srcs, dstdir), dstfiles)
  expect_identical(path_destination(srcs, dstfiles), dstfiles)
  expect_error(path_destination(srcs, dstdir / "noexist"), "length")
})
