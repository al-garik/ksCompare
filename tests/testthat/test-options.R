test_that("ks_tol validates inputs", {
  expect_snapshot(error = TRUE, ks_tol(abs = -1))
  expect_snapshot(error = TRUE, ks_tol(rel = "x"))
  expect_snapshot(error = TRUE, ks_tol(per_column = list(a = 1)))
})

test_that("ks_tol_for picks per-column override", {
  tol <- ks_tol(abs = 0, per_column = list(price = ks_tol(abs = 0.005)))
  expect_equal(ks_tol_for(tol, "price")$abs, 0.005)
  expect_equal(ks_tol_for(tol, "other")$abs, 0)
})

test_that("ks_options validates choices", {
  expect_snapshot(error = TRUE, ks_options(str_case = "upper"))
  expect_snapshot(error = TRUE, ks_options(na_equal = NA))
})
