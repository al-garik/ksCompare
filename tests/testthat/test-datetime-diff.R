test_that("date diffs are reported in days", {
  a <- data.frame(
    id = 1:3,
    d  = as.Date(c("2020-01-01", "2020-06-15", "2021-12-31"))
  )
  b <- data.frame(
    id = 1:3,
    d  = as.Date(c("2020-01-01", "2020-06-20", "2022-01-10"))
  )
  cmp <- ks_compare(a, b, by = "id")
  expect_equal(nrow(cmp$value_diff), 2L)
  expect_setequal(cmp$value_diff$diff, c(-5, -10))
  expect_true(all(cmp$value_diff$kind == "date"))
})

test_that("datetime diffs are reported in seconds with friendly notes", {
  a <- data.frame(
    id = 1:2,
    t  = as.POSIXct(c("2024-01-01 08:00:00.000", "2024-01-01 08:00:00"),
                    tz = "UTC")
  )
  b <- data.frame(
    id = 1:2,
    t  = as.POSIXct(c("2024-01-01 08:00:00.250", "2024-01-02 08:00:00"),
                    tz = "UTC")
  )
  cmp <- ks_compare(a, b, by = "id")
  expect_equal(nrow(cmp$value_diff), 2L)
  expect_setequal(cmp$value_diff$diff, c(-0.25, -86400))
  expect_true(all(cmp$value_diff$kind == "datetime"))
  notes <- cmp$value_diff$note
  expect_true(any(grepl("sub-second", notes)))
  expect_true(any(grepl("whole-day", notes)))
})

test_that("constant date offset is detected as a pattern", {
  a <- data.frame(
    id = 1:5,
    d  = as.Date("2024-01-01") + 0:4
  )
  b <- a
  b$d <- b$d + 10
  cmp <- ks_compare(a, b, by = "id", find_patterns = TRUE)
  expect_true(any(cmp$pattern_summary$pattern == "constant_offset"))
  detail <- cmp$pattern_summary$detail[
    cmp$pattern_summary$pattern == "constant_offset"
  ][[1]]
  expect_match(detail, "-10")
  expect_match(detail, "days")
})
