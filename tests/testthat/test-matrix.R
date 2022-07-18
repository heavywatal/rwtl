test_that("printmat works", {
  m = matrix(seq_len(9L), nrow = 3L, dimnames = list(letters[1:3], letters[24:26]))
  expect_output(printmat(m), "a 1 4 7\\nb 2 5 8\\nc 3 6 9$")
  expect_output(printmat(m, "%.1f"), "a 1.0 4.0 7.0\\nb 2.0 5.0 8.0\\nc 3.0 6.0 9.0$")
  expect_output(printmat(m, upper = FALSE), "a 1\\s+b 2 5\\s+c 3 6 9")
  expect_output(printmat(m, upper = FALSE, diag = FALSE), "a\\s+b 2\\s+c 3 6")
  expect_output(printmat(m, lower = FALSE), "a 1 4 7\\nb\\s+5 8\\nc\\s+9$")
  expect_output(printmat(m, lower = FALSE, diag = FALSE), "a\\s+4 7\\nb\\s+8\\nc\\s+$")
})
