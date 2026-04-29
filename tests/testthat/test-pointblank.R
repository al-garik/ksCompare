test_that("ks_assert_clean passes for identical frames", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  cmp <- ks_compare(a, a, by = "id")
  expect_invisible(ks_assert_clean(cmp))
  expect_identical(ks_assert_clean(cmp), cmp)
})

test_that("ks_assert_clean fails on value differences", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  cmp <- ks_compare(a, b, by = "id")
  expect_error(ks_assert_clean(cmp), class = "ksCompare_assertion_failed")
})

test_that("ks_assert_clean tolerates configured max diffs", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  cmp <- ks_compare(a, b, by = "id")
  expect_invisible(ks_assert_clean(cmp, max_value_diffs = 1L))
})

test_that("ks_assert_clean fails on schema differences", {
  a <- data.frame(id = 1:2, x = 1:2)
  b <- data.frame(id = 1:2, y = 1:2)
  cmp <- suppressMessages(ks_compare(a, b, by = "id"))
  expect_error(ks_assert_clean(cmp), class = "ksCompare_assertion_failed")
})

test_that("as_ks_comparison.ks_comparison is identity", {
  a <- data.frame(id = 1:2, x = 1:2)
  cmp <- ks_compare(a, a, by = "id")
  expect_identical(as_ks_comparison(cmp), cmp)
})

test_that("as_ks_comparison default rejects unsupported input", {
  expect_error(as_ks_comparison("oops"), class = "ksCompare_error")
})
