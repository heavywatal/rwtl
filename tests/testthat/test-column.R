test_that("dedfcol works", {
  orig = data.frame(x1 = 1:3, x2 = 4:6, y = 7:9)
  packed = orig |> tidyr::pack(x = starts_with("x"))
  expect_identical(ncol(packed), 2L)
  dedfed = dedfcol(packed, x) |>
    expect_s3_class("data.frame", exact = TRUE) |>
    expect_mapequal(orig)
  expect_setequal(names(dedfed), names(orig))
  dedfcol(packed, x, names_sep = "$") |>
    expect_named(c("y", "x$x1", "x$x2"))
  dedfcol_all(packed) |>
    names() |>
    expect_setequal(names(dedfed))

  unpacked = tidyr::unpack(packed, x) |>
    expect_s3_class("tbl_df") |>
    expect_named(c("y", "x1", "x2"))
  expect_mapequal(as.data.frame(unpacked), orig)
})

test_that("append_df works", {
  x = data.frame(x = 1L, z = 3L)
  values = data.frame(y = 2L)
  e = data.frame(x = 1L, y = 2L, z = 3L)
  expect_identical(append_df(x, values, 1L), e)
})
