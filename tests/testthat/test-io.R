test_that("ks_compare reads parquet inputs by extension", {
  skip_if_not_installed("arrow")
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  ta <- withr::local_tempfile(fileext = ".parquet")
  tb <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(a, ta)
  arrow::write_parquet(b, tb)

  cmp <- ks_compare(ta, tb, by = "id")
  expect_s3_class(cmp, "ks_comparison")
  expect_equal(nrow(cmp$value_diff), 1L)
  expect_equal(cmp$meta$base_source, normalizePath(ta, mustWork = FALSE))
  expect_equal(cmp$meta$comp_source, normalizePath(tb, mustWork = FALSE))
})

test_that("ks_compare reads feather inputs by extension", {
  skip_if_not_installed("arrow")
  ta <- withr::local_tempfile(fileext = ".feather")
  tb <- withr::local_tempfile(fileext = ".feather")
  arrow::write_feather(iris, ta)
  arrow::write_feather(iris, tb)
  cmp <- ks_compare(ta, tb)
  expect_s3_class(cmp, "ks_comparison")
})

test_that("ks_compare errors on unknown extension", {
  tmp <- withr::local_tempfile(fileext = ".unknown")
  file.create(tmp)
  expect_error(
    ks_compare(tmp, tmp),
    class = "ksCompare_error"
  )
})

test_that("ks_compare errors when path does not exist", {
  expect_error(
    ks_compare("z:/no/such/file.parquet", "z:/no/such/other.parquet"),
    class = "ksCompare_error"
  )
})

test_that("ks_compare reads RDS inputs", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  ta <- withr::local_tempfile(fileext = ".rds")
  tb <- withr::local_tempfile(fileext = ".rds")
  saveRDS(a, ta)
  saveRDS(b, tb)
  cmp <- ks_compare(ta, tb, by = "id")
  expect_equal(nrow(cmp$value_diff), 1L)
})

test_that("ks_compare reads CSV inputs", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  ta <- withr::local_tempfile(fileext = ".csv")
  tb <- withr::local_tempfile(fileext = ".csv")
  write.csv(a, ta, row.names = FALSE)
  write.csv(b, tb, row.names = FALSE)
  cmp <- ks_compare(ta, tb, by = "id")
  expect_equal(nrow(cmp$value_diff), 1L)
})

test_that("ks_compare disambiguates display names for same basename", {
  skip_if_not_installed("arrow")
  d1 <- withr::local_tempdir()
  d2 <- withr::local_tempdir()
  ta <- file.path(d1, "data.parquet")
  tb <- file.path(d2, "data.parquet")
  arrow::write_parquet(iris, ta)
  arrow::write_parquet(iris, tb)
  cmp <- ks_compare(ta, tb)
  expect_false(identical(cmp$meta$base_name, cmp$meta$comp_name))
  expect_match(cmp$meta$base_name, "/data$")
  expect_match(cmp$meta$comp_name, "/data$")
})

test_that("ks_compare uses bare basename when basenames differ", {
  skip_if_not_installed("arrow")
  ta <- withr::local_tempfile(fileext = ".parquet")
  tb <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(iris, ta)
  arrow::write_parquet(iris, tb)
  cmp <- ks_compare(ta, tb)
  expect_equal(cmp$meta$base_name, tools::file_path_sans_ext(basename(ta)))
})
