test_that("symlink works", {
  x = fs::dir_create(tempfile())
  src = fs::dir_create(fs::path(x, "src"))
  dst = symlink(src, fs::path(x, "dst")) |>
    expect_identical(fs::path(x, "dst"))
  expect_true(fs::link_exists(dst))
  expect_identical(link_resolve(dst), src)
  symlink(src, dst) |>
    expect_identical(fs::path(x, "dst", "src")) |>
    expect_silent()
  symlink(src, dst, follow = FALSE) |>
    expect_identical(fs::path(x, "dst")) |>
    expect_message("already exists")
  new = fs::dir_create(fs::path(x, "new"))
  symlink(new, dst) |>
    expect_identical(fs::path(x, "dst", "new")) |>
    expect_silent()
  symlink(new, dst, follow = FALSE) |>
    expect_message("already exists") |>
    expect_message("Replacing")
})

test_that("symlink with relative paths works", {
  x = fs::dir_create(tempfile())
  withr::local_dir(x)
  src = fs::dir_create("src")
  dst = symlink("src", "dst") |>
    expect_identical(fs::path("dst"))
  expect_true(fs::link_exists(dst))
  expect_identical(link_resolve(dst), src)
  symlink("src", "dst") |>
    expect_identical(fs::path("dst", "src")) |>
    expect_warning("broken link")
  symlink("src", "dst", follow = FALSE) |>
    expect_identical(dst) |>
    expect_message("already exists")
  fs::dir_create("new")
  symlink("../src", "new") |>
    expect_identical(fs::path("new", "src")) |>
    expect_silent()
  symlink("noexist", "target") |>
    expect_identical(fs::path("target")) |>
    expect_warning("broken link")
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
