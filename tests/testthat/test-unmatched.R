test_that("find_patterns = FALSE skips pattern detection but keeps schema", {
  a <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  b <- data.frame(id = 1:5, x = c(2, 3, 4, 5, 6))
  cmp <- ks_compare(a, b, by = "id")
  ps <- cmp$pattern_summary
  expect_s3_class(ps, "tbl_df")
  expect_equal(nrow(ps), 0L)
  expect_named(ps, c("column", "pattern", "coverage", "detail"))
})

test_that("find_patterns = TRUE populates the summary", {
  a <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  b <- data.frame(id = 1:5, x = c(2, 3, 4, 5, 6))
  cmp <- ks_compare(a, b, by = "id", find_patterns = TRUE)
  expect_true("constant_offset" %in% cmp$pattern_summary$pattern)
})

test_that("ks_unmatched_rows returns base-only and comp-only rows", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 2:4, x = c(2, 3, 4))
  cmp <- ks_compare(a, b, by = "id")

  ur <- ks_unmatched_rows(cmp)
  expect_s3_class(ur, "tbl_df")
  expect_true(all(c("side", "key_id", "key_label", "base_row", "comp_row") %in% names(ur)))
  # base_row / comp_row pinpoint the original observation in each frame
  expect_true(all(is.na(ur$comp_row[ur$side == "base_only"])))
  expect_true(all(is.na(ur$base_row[ur$side == "comp_only"])))
  expect_setequal(ur$side, c("base_only", "comp_only"))
  expect_equal(sum(ur$side == "base_only"), 1L)
  expect_equal(sum(ur$side == "comp_only"), 1L)

  expect_equal(nrow(ks_unmatched_rows(cmp, side = "base_only")), 1L)
  expect_equal(nrow(ks_unmatched_rows(cmp, side = "comp_only")), 1L)
})

test_that("ks_unmatched_rows returns an empty tibble when fully matched", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  cmp <- ks_compare(a, a, by = "id")
  ur <- ks_unmatched_rows(cmp)
  expect_s3_class(ur, "tbl_df")
  expect_equal(nrow(ur), 0L)
})

test_that("max_unmatched_rows caps the stored unmatched-rows table", {
  a <- data.frame(id = 1:20, x = 1:20)
  b <- data.frame(id = 31:50, x = 31:50)  # entirely disjoint
  cmp <- ks_compare(a, b, by = "id", max_unmatched_rows = 6L)
  ur <- ks_unmatched_rows(cmp)
  expect_lte(nrow(ur), 6L)
  trunc <- attr(ur, "truncated")
  expect_true(any(trunc))
  totals <- attr(ur, "n_total")
  expect_equal(unname(totals[["base"]]), 20L)
  expect_equal(unname(totals[["comp"]]), 20L)
})

test_that("max_unmatched_rows = 0 stores no row data but keeps counts", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 4:6, x = c(4, 5, 6))
  cmp <- ks_compare(a, b, by = "id", max_unmatched_rows = 0L)
  expect_equal(nrow(ks_unmatched_rows(cmp)), 0L)
  expect_equal(cmp$key_diff$base_only, 3L)
  expect_equal(cmp$key_diff$comp_only, 3L)
})

test_that("ks_tidy(include_unmatched = TRUE) appends unmatched rows", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 2:4, x = c(2, 3, 5))
  cmp <- ks_compare(a, b, by = "id")

  base_tidy <- ks_tidy(cmp)
  full_tidy <- ks_tidy(cmp, include_unmatched = TRUE)

  expect_equal(nrow(full_tidy), nrow(base_tidy) + 2L)
  expect_setequal(
    setdiff(unique(full_tidy$kind), unique(base_tidy$kind)),
    c("base_only", "comp_only")
  )
})

test_that("first_last_unequal captures first/last per column", {
  set.seed(11)
  n <- 20
  a <- data.frame(id = seq_len(n), x = seq_len(n), y = seq_len(n))
  b <- a
  b$x <- b$x + 1                              # all 20 differ
  b$y[c(2, 19)] <- b$y[c(2, 19)] + 1          # only 2 differ
  cmp <- ks_compare(a, b, by = "id", n_first_last = 3L)
  fl <- cmp$first_last_unequal
  expect_s3_class(fl, "tbl_df")
  expect_true(all(c("column_base", "position", "rank", "key_id") %in% names(fl)))

  fl_x <- fl[fl$column_base == "x", , drop = FALSE]
  expect_equal(sort(unique(fl_x$position)), c("first", "last"))
  expect_equal(sum(fl_x$position == "first"), 3L)
  expect_equal(sum(fl_x$position == "last"), 3L)
  expect_equal(fl_x$key_id[fl_x$position == "first"], 1:3)

  fl_y <- fl[fl$column_base == "y", , drop = FALSE]
  # Only 2 diffs total -> both go in "first", "last" overlap is dropped.
  expect_equal(sum(fl_y$position == "first"), 2L)
  expect_equal(sum(fl_y$position == "last"), 0L)
})

test_that("first_last_unequal is empty when n_first_last = 0 or no diffs", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  cmp_zero <- ks_compare(a, b, by = "id", n_first_last = 0L)
  expect_equal(nrow(cmp_zero$first_last_unequal), 0L)

  cmp_clean <- ks_compare(a, a, by = "id")
  expect_equal(nrow(cmp_clean$first_last_unequal), 0L)
})

test_that("invalid arg values raise informative errors", {
  a <- data.frame(id = 1:2, x = c(1, 2))
  expect_error(ks_compare(a, a, by = "id", find_patterns = NA), "find_patterns")
  expect_error(ks_compare(a, a, by = "id", n_first_last = -1), "n_first_last")
  expect_error(
    ks_compare(a, a, by = "id", max_unmatched_rows = -5),
    "max_unmatched_rows"
  )
})
