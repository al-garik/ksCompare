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


# ---- numeric -------------------------------------------------------------

test_that("null_as_zero is detected", {
  a <- data.frame(id = 1:6, x = c(NA, NA, NA, NA, 5, 6))
  b <- data.frame(id = 1:6, x = c(0,  0,  0,  0,  5, 6))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("null_as_zero" %in% cmp$pattern_summary$pattern)
})

test_that("null_as_sentinel is detected", {
  a <- data.frame(id = 1:5, x = c(NA, NA, NA, NA, 5))
  b <- data.frame(id = 1:5, x = c(-999, -999, -999, -999, 5))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("null_as_sentinel" %in% cmp$pattern_summary$pattern)
})

test_that("percentage_scale is detected", {
  a <- data.frame(id = 1:5, x = c(0.1, 0.2, 0.3, 0.4, 0.5))
  b <- data.frame(id = 1:5, x = c( 10,  20,  30,  40,  50))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("percentage_scale" %in% cmp$pattern_summary$pattern)
})

test_that("unit_scale is detected (mi -> km)", {
  mi <- c(1, 5, 10, 100)
  km <- mi * 1.609344
  a <- data.frame(id = seq_along(mi), x = mi)
  b <- data.frame(id = seq_along(mi), x = km)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("unit_scale" %in% cmp$pattern_summary$pattern)
})

test_that("precision_truncated is detected", {
  raw <- c(1.234567, 2.345678, 3.456789, 4.567890, 5.678901)
  a <- data.frame(id = seq_along(raw), x = raw)
  b <- data.frame(id = seq_along(raw), x = round(raw, 2))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("precision_truncated" %in% cmp$pattern_summary$pattern)
})

test_that("signed_vs_abs is detected", {
  a <- data.frame(id = 1:5, x = c(-1, -2, -3, -4, -5))
  b <- data.frame(id = 1:5, x = c( 1,  2,  3,  4,  5))
  cmp <- ks_compare(a, b, by = "id")
  expect_true(any(c("signed_vs_abs", "sign_flip") %in%
                    cmp$pattern_summary$pattern))
})

test_that("monotone_drift is detected", {
  set.seed(1)
  n <- 30
  a <- data.frame(id = seq_len(n), x = seq_len(n) + 0)
  b <- data.frame(id = seq_len(n), x = seq_len(n) + seq(0.1, 3, length.out = n))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("monotone_drift" %in% cmp$pattern_summary$pattern)
})

# ---- logical -------------------------------------------------------------

test_that("flag_polarity_swapped is detected", {
  a <- data.frame(id = 1:6, x = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE))
  b <- data.frame(id = 1:6, x = !a$x)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("flag_polarity_swapped" %in% cmp$pattern_summary$pattern)
})

test_that("true_to_na is detected", {
  a <- data.frame(id = 1:6, x = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  b <- data.frame(id = 1:6, x = c(NA,   NA,   NA,   NA,   FALSE, FALSE))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("true_to_na" %in% cmp$pattern_summary$pattern)
})

test_that("false_to_na is detected", {
  a <- data.frame(id = 1:6, x = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
  b <- data.frame(id = 1:6, x = c(NA,    NA,    NA,    NA,    TRUE, TRUE))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("false_to_na" %in% cmp$pattern_summary$pattern)
})

# ---- temporal ------------------------------------------------------------

test_that("epoch_swap is detected (SAS 1960 vs Unix 1970)", {
  base_dt <- as.Date(c("2020-01-01", "2020-06-15", "2021-03-20", "2022-12-31"))
  comp_dt <- base_dt + 3653  # SAS->Unix epoch delta in days
  a <- data.frame(id = seq_along(base_dt), d = base_dt)
  b <- data.frame(id = seq_along(base_dt), d = comp_dt)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("epoch_swap" %in% cmp$pattern_summary$pattern)
})

test_that("year_offset is detected", {
  base_dt <- as.Date(c("2020-01-01", "2020-06-15", "2021-03-20", "2022-12-31"))
  comp_dt <- base_dt + 365
  a <- data.frame(id = seq_along(base_dt), d = base_dt)
  b <- data.frame(id = seq_along(base_dt), d = comp_dt)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("year_offset" %in% cmp$pattern_summary$pattern)
})

test_that("tz_hour_offset is detected", {
  ts <- as.POSIXct(c("2024-01-01 10:00:00", "2024-06-15 14:30:00",
                     "2024-09-20 23:15:00"), tz = "UTC")
  a <- data.frame(id = seq_along(ts), t = ts)
  b <- data.frame(id = seq_along(ts), t = ts + 3600)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("tz_hour_offset" %in% cmp$pattern_summary$pattern)
})

test_that("midnight_truncation is detected", {
  ts_full <- as.POSIXct(
    c("2024-01-01 10:30:00", "2024-06-15 14:45:00", "2024-09-20 23:15:00"),
    tz = "UTC"
  )
  ts_mid <- as.POSIXct(
    c("2024-01-01 00:00:00", "2024-06-15 00:00:00", "2024-09-20 00:00:00"),
    tz = "UTC"
  )
  a <- data.frame(id = seq_along(ts_full), t = ts_full)
  b <- data.frame(id = seq_along(ts_full), t = ts_mid)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("midnight_truncation" %in% cmp$pattern_summary$pattern)
})

# ---- string --------------------------------------------------------------

test_that("unicode_normalization_only is detected", {
  composed   <- c("caf\u00e9", "na\u00efve", "r\u00e9sum\u00e9")
  decomposed <- c("cafe\u0301", "nai\u0308ve", "re\u0301sume\u0301")
  a <- data.frame(id = seq_along(composed), x = composed)
  b <- data.frame(id = seq_along(composed), x = decomposed)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("unicode_normalization_only" %in% cmp$pattern_summary$pattern)
})

test_that("punctuation_only is detected", {
  a <- data.frame(id = 1:3, x = c("hello.world", "foo-bar", "a/b/c"))
  b <- data.frame(id = 1:3, x = c("helloworld",  "foobar",  "abc"))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("punctuation_only" %in% cmp$pattern_summary$pattern)
})

test_that("prefix_added and prefix_removed are detected", {
  a <- data.frame(id = 1:3, x = c("001", "002", "003"))
  b <- data.frame(id = 1:3, x = c("PT-001", "PT-002", "PT-003"))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("prefix_added" %in% cmp$pattern_summary$pattern)

  cmp2 <- ks_compare(b, a, by = "id")
  expect_true("prefix_removed" %in% cmp2$pattern_summary$pattern)
})

test_that("suffix_added and suffix_removed are detected", {
  a <- data.frame(id = 1:3, x = c("alpha", "beta", "gamma"))
  b <- data.frame(id = 1:3, x = c("alpha_v2", "beta_v2", "gamma_v2"))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("suffix_added" %in% cmp$pattern_summary$pattern)

  cmp2 <- ks_compare(b, a, by = "id")
  expect_true("suffix_removed" %in% cmp2$pattern_summary$pattern)
})

test_that("zero_padded is detected", {
  a <- data.frame(id = 1:5, x = c("1", "2", "3", "4", "5"))
  b <- data.frame(id = 1:5, x = c("001", "002", "003", "004", "005"))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("zero_padded" %in% cmp$pattern_summary$pattern)
})

test_that("truncated_to_width is detected", {
  long <- c("PROTOCOL01", "PROTOCOL02", "PROTOCOL03", "PROTOCOL04")
  short <- substr(long, 1, 5)  # constant width 5
  a <- data.frame(id = seq_along(long), x = long)
  b <- data.frame(id = seq_along(long), x = short)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("truncated_to_width" %in% cmp$pattern_summary$pattern)
})

test_that("abbreviation is detected (varying-length prefix)", {
  full <- c("Female", "Male", "Other", "Female")
  abbr <- c("F", "Ma", "Oth", "Fem")
  a <- data.frame(id = seq_along(full), x = full)
  b <- data.frame(id = seq_along(full), x = abbr)
  cmp <- ks_compare(a, b, by = "id")
  expect_true("abbreviation" %in% cmp$pattern_summary$pattern)
})

test_that("coded_decode is detected for character", {
  a <- data.frame(id = 1:6, x = c("M", "F", "M", "F", "M", "F"))
  b <- data.frame(id = 1:6, x = c("Male", "Female", "Male", "Female", "Male", "Female"))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("coded_decode" %in% cmp$pattern_summary$pattern)
})

test_that("near_match fires when stringdist is available", {
  skip_if_not_installed("stringdist")
  a <- data.frame(id = 1:5,
                  x = c("Apple",  "Banana", "Cherry", "Date",  "Elderberry"))
  b <- data.frame(id = 1:5,
                  x = c("Aplpe",  "Bnana",  "Chery",  "Dae",   "Eldreberry"))
  cmp <- ks_compare(a, b, by = "id")
  expect_true("near_match" %in% cmp$pattern_summary$pattern)
})

# ---- cross-column --------------------------------------------------------

test_that("pareto_columns is detected", {
  set.seed(2)
  n <- 50
  base <- data.frame(
    id = seq_len(n),
    a  = 1:n,
    b  = 1:n,
    c  = 1:n,
    d  = 1:n,
    e  = 1:n
  )
  # Concentrate diffs on column "a".
  comp <- base
  comp$a <- comp$a + 1
  comp$b[1:2] <- comp$b[1:2] + 1
  cmp <- ks_compare(base, comp, by = "id")
  pats <- cmp$pattern_summary
  expect_true("pareto_columns" %in% pats$pattern)
  expect_true(all(is.na(pats$column[pats$pattern == "pareto_columns"])))
})

test_that("pareto_keys is detected when one row dominates", {
  n <- 20
  base <- data.frame(
    id = seq_len(n),
    a  = 1:n, b = 1:n, c = 1:n, d = 1:n, e = 1:n,
    f  = 1:n, g = 1:n, h = 1:n
  )
  comp <- base
  # Row 1 differs on every column (8 diffs); row 2 differs on one column (1 diff).
  comp[1, -1] <- comp[1, -1] + 100
  comp$a[2]  <- comp$a[2] + 1
  cmp <- ks_compare(base, comp, by = "id")
  expect_true("pareto_keys" %in% cmp$pattern_summary$pattern)
})

test_that("paired_columns is detected", {
  n <- 20
  base <- data.frame(
    id = seq_len(n),
    a  = 1:n, b = 1:n, c = 1:n
  )
  comp <- base
  # cols a and b diff on the same 8 rows; c never differs.
  rows <- 1:8
  comp$a[rows] <- comp$a[rows] + 1
  comp$b[rows] <- comp$b[rows] + 7
  cmp <- ks_compare(base, comp, by = "id")
  pats <- cmp$pattern_summary
  expect_true("paired_columns" %in% pats$pattern)
})
