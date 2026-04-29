test_that("ks_align_columns honors explicit mapping", {
  a <- data.frame(x = 1, y = 2, z = 3)
  b <- data.frame(x = 1, w = 2, z = 3)
  out <- ks_align_columns(a, b, mapping = c(y = "w"))
  expect_equal(nrow(out$pairs), 3L)
  expect_setequal(out$pairs$base, c("x", "y", "z"))
  expect_equal(out$base_only, character())
  expect_equal(out$comp_only, character())
})

test_that("ks_align_columns rejects unknown mapping columns", {
  a <- data.frame(x = 1)
  b <- data.frame(y = 1)
  expect_snapshot(error = TRUE, ks_align_columns(a, b, mapping = c(z = "y")))
})

test_that("ks_align_columns handles disjoint columns", {
  a <- data.frame(x = 1, y = 2)
  b <- data.frame(y = 1, z = 2)
  out <- ks_align_columns(a, b)
  expect_equal(out$pairs$base, "y")
  expect_equal(out$base_only, "x")
  expect_equal(out$comp_only, "z")
})
