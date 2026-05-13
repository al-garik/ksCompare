test_that("ks_cause_summary returns a tidy taxonomy", {
  a <- data.frame(
    id = 1:4,
    x = c("a", "b ", "c", "d"),
    y = c(1, 2, 3, NA),
    stringsAsFactors = FALSE
  )
  b <- data.frame(
    id = 1:4,
    x = c("A", "b", "c", "d"),
    y = c(1, 2, 4, 5),
    stringsAsFactors = FALSE
  )
  cmp <- ks_compare(a, b, by = "id")
  cs <- ks_cause_summary(cmp)
  expect_s3_class(cs, "tbl_df")
  expect_true(all(c("cause", "n_cells", "n_columns", "columns") %in% names(cs)))
  expect_gt(nrow(cs), 0L)
  expect_true(all(cs$n_cells >= 1L))
  # Sorted by n_cells desc
  expect_equal(cs$n_cells, sort(cs$n_cells, decreasing = TRUE))
})

test_that("ks_cause_summary is empty when no diffs", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  cmp <- ks_compare(a, a, by = "id")
  cs <- ks_cause_summary(cmp)
  expect_equal(nrow(cs), 0L)
})

test_that("ks_row_diff_summary tallies per-row diffs", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"),
                  stringsAsFactors = FALSE)
  b <- data.frame(id = 1:3, x = c(1, 9, 3), y = c("a", "b", "Z"),
                  stringsAsFactors = FALSE)
  cmp <- ks_compare(a, b, by = "id")
  rs <- ks_row_diff_summary(cmp)
  expect_s3_class(rs, "tbl_df")
  expect_true(all(c("key_id", "base_row", "comp_row", "n_diffs", "columns") %in% names(rs)))
  expect_equal(rs$n_diffs, sort(rs$n_diffs, decreasing = TRUE))
  expect_equal(sum(rs$n_diffs), nrow(cmp$value_diff))
})

test_that("ks_row_diff_summary honours n and validates input", {
  a <- data.frame(id = 1:5, x = 1:5)
  b <- data.frame(id = 1:5, x = c(9, 9, 9, 9, 9))
  cmp <- ks_compare(a, b, by = "id")
  rs <- ks_row_diff_summary(cmp, n = 2L)
  expect_lte(nrow(rs), 2L)
  expect_error(ks_row_diff_summary(cmp, n = -1L))
})

test_that("value_diff carries base_row, comp_row, na_flow columns", {
  a <- data.frame(id = 1:3, x = c(1, NA, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, NA))
  cmp <- ks_compare(a, b, by = "id")
  vd <- cmp$value_diff
  expect_true(all(c("base_row", "comp_row", "na_flow") %in% names(vd)))
  # base_row / comp_row match key_id for this fixture (1:3 order preserved)
  expect_equal(vd$base_row, vd$comp_row)
  expect_true(any(vd$na_flow %in% c("na_to_value", "value_to_na")))
})

test_that("print.ks_comparison shows a Verdict line", {
  a <- data.frame(id = 1:2, x = c(1, 2))
  b <- data.frame(id = 1:2, x = c(1, 9))
  cmp <- ks_compare(a, b, by = "id")
  out <- paste(
    c(
      capture.output(print(cmp)),
      capture.output(print(cmp), type = "message")
    ),
    collapse = "\n"
  )
  expect_match(out, "Verdict", fixed = TRUE)
})

test_that("ks_match_health flags duplicate-keys positional pairing", {
  a <- data.frame(id = rep(1:5, each = 3), x = 1:15, y = letters[1:15],
                  stringsAsFactors = FALSE)
  b <- a
  b$x <- rev(b$x)
  b$y <- rev(b$y)
  cmp <- ks_compare(a, b, by = "id", dup_keys = "keep_all")
  h <- ks_match_health(cmp)
  expect_true(h$dup_keys)
  expect_true(h$dup_positional)
  expect_gt(h$diff_density, 0)
})

test_that("ks_recommendations returns critical row when dup-key density high", {
  a <- data.frame(id = rep(1:5, each = 3), x = 1:15)
  b <- data.frame(id = rep(1:5, each = 3), x = 15:1)
  cmp <- ks_compare(a, b, by = "id", dup_keys = "keep_all")
  recs <- ks_recommendations(cmp)
  expect_s3_class(recs, "tbl_df")
  expect_true(any(recs$severity == "critical"))
})

test_that("ks_match_health is ok on a clean comparison", {
  a <- data.frame(id = 1:3, x = 1:3)
  cmp <- ks_compare(a, a, by = "id")
  h <- ks_match_health(cmp)
  expect_equal(h$severity, "ok")
  expect_equal(h$diff_density, 0)
})
