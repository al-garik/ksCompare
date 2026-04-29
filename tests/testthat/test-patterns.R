test_that("constant offset is detected", {
  a <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  b <- data.frame(id = 1:5, x = c(2, 3, 4, 5, 6))
  cmp <- ks_compare(a, b, by = "id")
  ps <- cmp$pattern_summary
  expect_true("constant_offset" %in% ps$pattern)
  expect_equal(ps$detail[ps$pattern == "constant_offset"][[1]], "-1")
})

test_that("sign flip is detected", {
  a <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  b <- data.frame(id = 1:5, x = c(-1, -2, -3, -4, -5))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("sign_flip" %in% cmp$pattern_summary$pattern)
})

test_that("case-only differences are detected", {
  a <- data.frame(id = 1:3, x = c("Apple", "Banana", "Cherry"))
  b <- data.frame(id = 1:3, x = c("apple", "banana", "cherry"))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("case_only" %in% cmp$pattern_summary$pattern)
})

test_that("trim-only differences are detected", {
  a <- data.frame(id = 1:3, x = c("foo", "bar", "baz"))
  b <- data.frame(id = 1:3, x = c(" foo ", " bar ", " baz "))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("trim_only" %in% cmp$pattern_summary$pattern)
})

test_that("identical frames produce no patterns", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  cmp <- ks_compare(a, a, by = "id")
  expect_equal(nrow(cmp$pattern_summary), 0L)
})
