test_that("read_boost_ini works", {
  ini = "x = 42\ny = text\n"
  tsv = "x\ty\n42\ttext\n"
  expect_equal(read_boost_ini(ini), readr::read_tsv(tsv))
})
