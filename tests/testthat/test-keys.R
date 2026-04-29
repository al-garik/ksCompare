test_that("ks_resolve_by accepts unnamed character vector", {
  a <- data.frame(id = 1, x = 1)
  out <- ks_resolve_by(a, a, "id")
  expect_equal(out$base, "id")
  expect_equal(out$comp, "id")
})

test_that("ks_resolve_by accepts named character vector", {
  a <- data.frame(subject = 1)
  b <- data.frame(usubjid = 1)
  out <- ks_resolve_by(a, b, c(subject = "usubjid"))
  expect_equal(out$base, "subject")
  expect_equal(out$comp, "usubjid")
})

test_that("ks_resolve_by rejects unknown columns", {
  a <- data.frame(x = 1)
  b <- data.frame(x = 1)
  expect_snapshot(error = TRUE, ks_resolve_by(a, b, "missing"))
})
