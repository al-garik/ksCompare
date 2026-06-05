test_that("by = 'auto' picks the smallest unique key from shared columns", {
  a <- data.frame(id = 1:3, x = 1:3)
  b <- data.frame(id = 1:3, x = 1:3)
  expect_message(
    cmp <- ks_compare(a, b, by = "auto", loglevel = "verbose"),
    class = "ksCompare_auto_key_inferred"
  )
  expect_equal(cmp$meta$keys$base, "id")
})

test_that("by = 'auto' falls back to position match when no key found", {
  a <- data.frame(x = c(1, 1), y = c("a", "a"))
  b <- data.frame(x = c(1, 1), y = c("a", "a"))
  expect_warning(
    cmp <- ks_compare(a, b, by = "auto"),
    class = "ksCompare_auto_key_failed"
  )
  expect_equal(nrow(cmp$meta$keys), 0L)
})

test_that("by = 'auto' prefers single-column keys over composites", {
  a <- data.frame(study = "S1", id = 1:3, x = 1:3)
  b <- data.frame(study = "S1", id = 1:3, x = 1:3)
  cmp <- suppressMessages(ks_compare(a, b, by = "auto"))
  expect_equal(cmp$meta$keys$base, "id")
})
