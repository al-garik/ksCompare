test_that("ULP tolerance accepts neighboring doubles", {
  x <- 1.0
  y <- x + .Machine$double.eps
  d <- ks_ulp_distance(x, y)
  expect_true(is.finite(d))
  expect_lte(d, 4)
})

test_that("ULP distance is 0 for equal values and NA for NA", {
  expect_equal(ks_ulp_distance(1, 1), 0)
  expect_true(is.na(ks_ulp_distance(NA_real_, 1)))
  expect_true(is.na(ks_ulp_distance(1, NaN)))
})

test_that("ULP distance treats +0 and -0 as equal", {
  expect_equal(ks_ulp_distance(0, -0), 0)
})

test_that("ks_compare uses ULP tolerance to mask near-equal floats", {
  a <- data.frame(id = 1:2, x = c(1.0, 2.0))
  b <- data.frame(id = 1:2, x = c(1.0 + .Machine$double.eps, 2.0))
  cmp_default <- ks_compare(a, b, by = "id")
  cmp_ulp <- ks_compare(a, b, by = "id", tolerance = ks_tol(ulp = 4))
  expect_equal(nrow(cmp_default$value_diff), 1L)
  expect_equal(nrow(cmp_ulp$value_diff), 0L)
})
