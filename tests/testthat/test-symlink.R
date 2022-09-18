test_that("symlink with absolute paths works", {
  x = fs::dir_create(tempfile())
  src = fs::dir_create(fs::path(x, "src"))
  dst = fs::path(x, "dst")
  symlink(src, dst) |>
    expect_identical(dst) |>
    expect_silent()
  expect_true(fs::link_exists(dst))
  expect_true(fs::dir_exists(dst))
  expect_identical(link_resolve(dst), src)

  # target is an existing symlink to a directory
  symlink(src, dst, follow = FALSE) |>
    expect_identical(dst) |>
    expect_message("already exists")
  expect_false(fs::file_exists(fs::path(x, "dst", "src")))

  # follow = TRUE by default; x/dst@/src@ -> /path/to/x/src
  lnk = symlink(src, dst) |>
    expect_identical(fs::path(x, "dst", "src")) |>
    expect_silent()
  expect_true(fs::link_exists(lnk))
  expect_true(fs::dir_exists(lnk))
  expect_identical(link_resolve(lnk), src)

  new = fs::dir_create(fs::path(x, "new"))
  symlink(new, dst, follow = FALSE) |>
    expect_identical(dst) |>
    expect_message("already exists") |>
    expect_message("Replacing")
  expect_true(fs::link_exists(dst))
  expect_true(fs::dir_exists(dst))
  expect_identical(link_resolve(dst), new)

  {
    lnk = symlink("noexist", fs::path(x, "broken")) |>
      expect_identical(fs::path(x, "broken"))
  } |>
    expect_warning("broken link")
  link_resolve(lnk) |>
    expect_identical(fs::path(x, "noexist")) |>
    expect_warning("broken link")
})

test_that("symlink with relative paths works", {
  x = fs::dir_create(tempfile())
  withr::local_dir(x)
  src = fs::dir_create("src")
  dst = symlink("src", "dst") |>
    expect_identical(fs::path("dst")) |>
    expect_silent()
  expect_true(fs::link_exists(dst))
  expect_true(fs::dir_exists(dst))
  expect_identical(link_resolve(dst), src)

  lnk = symlink("../src", "src/lnk") |>
    expect_identical(fs::path("src", "lnk")) |>
    expect_silent()
  expect_identical(link_resolve(lnk), src)

  symlink("src", "dst", follow = FALSE) |>
    expect_identical(dst) |>
    expect_message("already exists")
  expect_false(fs::file_exists(fs::path("dst", "src")))

  {
    lnk = symlink("src", "dst") |>
      expect_identical(fs::path("dst", "src"))
  } |>
    expect_warning("broken link")
  expect_warning(link_resolve(lnk), "broken link")
  expect_true(fs::link_exists(lnk))
  skip("r-lib/fs#394")
  expect_false(fs::dir_exists(lnk))
})

test_that("symlink with a target directory works", {
  x = fs::dir_create(tempfile())
  src = fs::dir_create(fs::path(x, paste0("src", 1:3)))
  dir = fs::dir_create(fs::path(x, "dir"))
  dst = symlink(src, dir) |>
    expect_identical(fs::path(dir, fs::path_file(src)))
  expect_true(all(fs::link_exists(dst)))
  expect_identical(link_resolve(dst), src)
})

test_that("symlink_latest_library works", {
  x = fs::dir_create(tempfile())
  parent = fs::path_dir(x)
  lnk = symlink_latest_library(x)
  expect_true(fs::link_exists(lnk))
  skip("r-lib/fs#395")
  expect_true(fs::dir_exists(lnk))
  fs::link_delete(lnk)
})
