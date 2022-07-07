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
