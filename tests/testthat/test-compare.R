test_that("identical frames produce no value diffs", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
  cmp <- ks_compare(a, a, by = "id")
  expect_s3_class(cmp, "ks_comparison")
  expect_equal(nrow(cmp$value_diff), 0L)
  expect_equal(sum(cmp$row_diff$status == "matched"), 3L)
})

test_that("differing numeric values surface in value_diff", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  cmp <- ks_compare(a, b, by = "id")
  expect_equal(nrow(cmp$value_diff), 1L)
  expect_equal(cmp$value_diff$column_base, "x")
  expect_equal(cmp$value_diff$diff, -1)
})

test_that("absolute tolerance suppresses small numeric differences", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1 + 1e-10, 2, 3))
  cmp_default <- ks_compare(a, b, by = "id")
  cmp_tol <- ks_compare(a, b, by = "id", tolerance = ks_tol(abs = 1e-9))
  expect_equal(nrow(cmp_default$value_diff), 1L)
  expect_equal(nrow(cmp_tol$value_diff), 0L)
})

test_that("relative tolerance handles scaled magnitudes", {
  a <- data.frame(id = 1L, x = 1e6)
  b <- data.frame(id = 1L, x = 1e6 + 1)
  cmp_abs <- ks_compare(a, b, by = "id", tolerance = ks_tol(abs = 0.5))
  cmp_rel <- ks_compare(a, b, by = "id", tolerance = ks_tol(rel = 1e-5))
  expect_equal(nrow(cmp_abs$value_diff), 1L)
  expect_equal(nrow(cmp_rel$value_diff), 0L)
})

test_that("position match handles unequal row counts", {
  a <- data.frame(x = 1:3)
  b <- data.frame(x = 1:5)
  cmp <- ks_compare(a, b)
  expect_equal(sum(cmp$row_diff$status == "matched"), 3L)
  expect_equal(sum(cmp$row_diff$status == "comp_only"), 2L)
})

test_that("keyed match identifies base-only and comp-only rows", {
  a <- data.frame(id = c(1, 2, 3), x = c("a", "b", "c"))
  b <- data.frame(id = c(2, 3, 4), x = c("b", "c", "d"))
  cmp <- ks_compare(a, b, by = "id")
  status_counts <- table(cmp$row_diff$status)
  expect_equal(unname(status_counts["matched"]), 2L)
  expect_equal(unname(status_counts["base_only"]), 1L)
  expect_equal(unname(status_counts["comp_only"]), 1L)
})

test_that("character options trim and case-fold", {
  a <- data.frame(id = 1L, x = "hello")
  b <- data.frame(id = 1L, x = "  HELLO  ")
  cmp_default <- ks_compare(a, b, by = "id")
  cmp_smart <- ks_compare(
    a,
    b,
    by = "id",
    options = ks_comp_options(str_trim = TRUE, str_case = "fold")
  )
  expect_equal(nrow(cmp_default$value_diff), 1L)
  expect_equal(nrow(cmp_smart$value_diff), 0L)
})

test_that("NA equality is configurable", {
  a <- data.frame(id = 1:2, x = c(NA_real_, 1))
  b <- data.frame(id = 1:2, x = c(NA_real_, 1))
  cmp_eq <- ks_compare(a, b, by = "id")
  cmp_neq <- ks_compare(a, b, by = "id", options = ks_comp_options(na_equal = FALSE))
  expect_equal(nrow(cmp_eq$value_diff), 0L)
  expect_equal(nrow(cmp_neq$value_diff), 1L)
})

test_that("base-only and comp-only columns are reported", {
  a <- data.frame(id = 1:2, x = 1:2, only_a = 1:2)
  b <- data.frame(id = 1:2, x = 1:2, only_b = 1:2)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("only_a" %in% cmp$schema_diff$base[cmp$schema_diff$side == "base_only"])
  expect_true("only_b" %in% cmp$schema_diff$comp[cmp$schema_diff$side == "comp_only"])
})

test_that("explicit mapping renames columns for comparison", {
  a <- data.frame(id = 1:2, value = c(10, 20))
  b <- data.frame(id = 1:2, val = c(10, 20))
  cmp <- ks_compare(a, b, by = "id", mapping = c(value = "val"))
  expect_equal(nrow(cmp$value_diff), 0L)
  expect_true(any(
    cmp$schema_diff$base == "value" & cmp$schema_diff$comp == "val"
  ))
})

test_that("named `by` permits differently-named keys", {
  a <- data.frame(subject = 1:3, x = c(1, 2, 3))
  b <- data.frame(usubjid = 1:3, x = c(1, 2, 3))
  cmp <- ks_compare(a, b, by = c(subject = "usubjid"))
  expect_equal(nrow(cmp$value_diff), 0L)
})

test_that("duplicate keys raise a classed error when dup_keys = 'error'", {
  a <- data.frame(id = c(1, 1, 2), x = c(1, 2, 3))
  b <- data.frame(id = c(1, 2), x = c(1, 3))
  expect_snapshot(error = TRUE, ks_compare(a, b, by = "id", dup_keys = "error"))
})

test_that("default dup_keys = 'first' collapses with an inform message", {
  a <- data.frame(id = c(1, 1, 2), x = c(1, 2, 3))
  b <- data.frame(id = c(1, 2), x = c(1, 3))
  expect_message(
    cmp <- ks_compare(a, b, by = "id"),
    class = "ksCompare_dup_keys_resolved"
  )
  expect_equal(cmp$meta$matching$strategy, "keyed_dup_first")
  expect_equal(cmp$meta$matching$n_dropped_base, 1L)
})

test_that("by = 'auto' infers a key", {
  a <- data.frame(id = 1:2, x = 1:2)
  cmp <- suppressMessages(ks_compare(a, a, by = "auto"))
  expect_equal(cmp$meta$keys$base, "id")
})

test_that("ks_glance returns a one-row tibble", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  g <- ks_glance(ks_compare(a, b, by = "id"))
  expect_s3_class(g, "tbl_df")
  expect_equal(nrow(g), 1L)
  expect_equal(g$n_value_diffs, 1L)
})
