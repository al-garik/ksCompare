test_that("key columns with all NA on one side are coerced properly", {
  # Numeric vs character (all NA)
  a <- data.frame(
    group = c(1, 2, 3),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    group = c(NA_character_, NA_character_, NA_character_),
    value = c(10, 20, 30)
  )
  
  cmp <- suppressMessages(ks_compare(a, b, by = "group"))
  expect_s3_class(cmp, "ks_comparison")
  # NA keys cannot match non-NA keys; base rows become base_only.
  # comp's 3 identical NA keys are deduplicated to 1 comp_only row.
  expect_equal(sum(cmp$row_diff$status == "matched"), 0L)
  expect_equal(sum(cmp$row_diff$status == "base_only"), 3L)
})

test_that("key columns with logical NA are coerced to numeric", {
  # Integer vs logical NA
  a <- data.frame(
    group = c(1L, 2L, 3L),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    group = c(NA, NA, NA),  # logical NA
    value = c(10, 20, 30)
  )
  
  cmp <- suppressMessages(ks_compare(a, b, by = "group"))
  expect_s3_class(cmp, "ks_comparison")
  # NA keys cannot match non-NA keys; base rows become base_only.
  expect_equal(sum(cmp$row_diff$status == "matched"), 0L)
  expect_equal(sum(cmp$row_diff$status == "base_only"), 3L)
})

test_that("multi-column keys with NA in one column are coerced", {
  a <- data.frame(
    group1 = c("A", "B", "C"),
    group2 = c(1, 2, 3),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    group1 = c("A", "B", "C"),
    group2 = c(NA, NA, NA),  # All NA
    value = c(10, 20, 30)
  )
  
  cmp <- ks_compare(a, b, by = c("group1", "group2"))
  expect_s3_class(cmp, "ks_comparison")
  # Composite key ("A",NA) != ("A",1) etc. — NA in any key column blocks matching.
  # Each comp row has a distinct composite key, so all 3 become comp_only.
  expect_equal(sum(cmp$row_diff$status == "matched"), 0L)
  expect_equal(sum(cmp$row_diff$status == "base_only"), 3L)
  expect_equal(sum(cmp$row_diff$status == "comp_only"), 3L)
})

test_that("factor vs character keys are coerced", {
  a <- data.frame(
    group = factor(c("A", "B", "C")),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    group = c("A", "B", "C"),
    value = c(10, 20, 30)
  )
  
  cmp <- ks_compare(a, b, by = "group")
  expect_s3_class(cmp, "ks_comparison")
  expect_equal(sum(cmp$row_diff$status == "matched"), 3L)
  expect_equal(nrow(cmp$value_diff), 0L)
})

test_that("numeric vs character keys with values are coerced to character", {
  a <- data.frame(
    group = c(1, 2, 3),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    group = c("1", "2", "3"),  # Character versions of numbers
    value = c(10, 20, 30)
  )
  
  # With lossy coercion, this should work
  cmp <- ks_compare(a, b, by = "group", coerce = "lossy")
  expect_s3_class(cmp, "ks_comparison")
  expect_equal(sum(cmp$row_diff$status == "matched"), 3L)
})

test_that("key coercion works with duplicate keys", {
  a <- data.frame(
    group = c(1, 1, 2),
    value = c(10, 11, 20)
  )
  b <- data.frame(
    group = c("1", "1", "2"),
    value = c(10, 11, 20)
  )
  
  cmp <- suppressMessages(ks_compare(a, b, by = "group", dup_keys = "keep_all"))
  expect_s3_class(cmp, "ks_comparison")
  expect_equal(sum(cmp$row_diff$status == "matched"), 3L)
})

test_that("both sides all NA keys are handled", {
  a <- data.frame(
    group = c(NA, NA, NA),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    group = c(NA_character_, NA_character_, NA_character_),
    value = c(10, 20, 30)
  )
  
  cmp <- ks_compare(a, b, by = "group")
  expect_s3_class(cmp, "ks_comparison")
  # When both sides have NA keys, matching depends on na_equal option
  # Default na_equal = TRUE, so NAs should match
  expect_true(sum(cmp$row_diff$status == "matched") > 0L)
})

test_that("date keys with all NA on one side are coerced", {
  a <- data.frame(
    date_key = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    date_key = as.Date(c(NA, NA, NA)),
    value = c(10, 20, 30)
  )
  
  cmp <- suppressMessages(ks_compare(a, b, by = "date_key"))
  expect_s3_class(cmp, "ks_comparison")
  # NA date keys cannot match non-NA date keys.
  expect_equal(sum(cmp$row_diff$status == "matched"), 0L)
  expect_equal(sum(cmp$row_diff$status == "base_only"), 3L)
})

test_that("datetime keys with all NA on one side are coerced", {
  a <- data.frame(
    dt_key = as.POSIXct(c("2020-01-01 10:00:00", "2020-01-01 11:00:00", "2020-01-01 12:00:00")),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    dt_key = as.POSIXct(c(NA, NA, NA)),
    value = c(10, 20, 30)
  )
  
  cmp <- suppressMessages(ks_compare(a, b, by = "dt_key"))
  expect_s3_class(cmp, "ks_comparison")
  # NA datetime keys cannot match non-NA datetime keys.
  expect_equal(sum(cmp$row_diff$status == "matched"), 0L)
  expect_equal(sum(cmp$row_diff$status == "base_only"), 3L)
})

test_that("date vs datetime keys are coerced", {
  a <- data.frame(
    key = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    key = as.POSIXct(c("2020-01-01 00:00:00", "2020-01-02 00:00:00", "2020-01-03 00:00:00")),
    value = c(10, 20, 30)
  )
  
  # Date and POSIXct should be coercible by vctrs
  cmp <- ks_compare(a, b, by = "key")
  expect_s3_class(cmp, "ks_comparison")
  expect_equal(sum(cmp$row_diff$status == "matched"), 3L)
})

test_that("date vs numeric keys are coerced to character in lossy mode", {
  a <- data.frame(
    key = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    value = c(10, 20, 30)
  )
  b <- data.frame(
    key = c(18262, 18263, 18264),  # numeric representation
    value = c(10, 20, 30)
  )

  # With lossy coercion, date<->numeric should convert to character
  cmp <- ks_compare(a, b, by = "key")
  expect_s3_class(cmp, "ks_comparison")
  # They won't match because date formats as "2020-01-01" vs "18262"
  # but the comparison should not error
})

