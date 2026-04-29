test_that("ks_suggest_columns returns empty when stringdist is missing", {
  skip_if_installed <- function() {
    if (rlang::is_installed("stringdist")) skip("stringdist installed")
  }
  skip_if_installed()
  a <- data.frame(value = 1)
  b <- data.frame(val = 1)
  alignment <- ks_align_columns(a, b)
  out <- ks_suggest_columns(a, b, alignment)
  expect_equal(nrow(out), 0L)
})

test_that("ks_suggest_columns ranks fuzzy matches", {
  skip_if_not_installed("stringdist")
  a <- data.frame(usubjid = 1, age_yrs = 30, sex = "M")
  b <- data.frame(usubjid = 1, age = 30, gender = "M")
  alignment <- ks_align_columns(a, b)
  sug <- ks_suggest_columns(a, b, alignment, n = 5L)
  expect_gt(nrow(sug), 0L)
  expect_equal(sug$base[[1]], "age_yrs")
  expect_equal(sug$comp[[1]], "age")
})

test_that("compare surfaces suggestions in the result object", {
  skip_if_not_installed("stringdist")
  a <- data.frame(id = 1, age_yrs = 30)
  b <- data.frame(id = 1, age = 30)
  cmp <- ks_compare(a, b, by = "id")
  expect_gt(nrow(cmp$column_suggestions), 0L)
})
