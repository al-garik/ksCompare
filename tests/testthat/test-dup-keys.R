test_that("dup_keys = 'first' collapses duplicates by first occurrence", {
  a <- data.frame(id = c(1, 1, 2), x = c(10, 99, 20))
  b <- data.frame(id = c(1, 2), x = c(10, 20))
  cmp <- suppressMessages(ks_compare(a, b, by = "id", dup_keys = "first"))
  expect_equal(nrow(cmp$value_diff), 0L)
  expect_equal(sum(cmp$row_diff$status == "matched"), 2L)
  expect_equal(cmp$meta$matching$dup_strategy, "first")
  expect_equal(cmp$meta$matching$n_dropped_base, 1L)
})

test_that("dup_keys = 'last' collapses duplicates by last occurrence", {
  a <- data.frame(id = c(1, 1, 2), x = c(99, 10, 20))
  b <- data.frame(id = c(1, 2), x = c(10, 20))
  cmp <- suppressMessages(ks_compare(a, b, by = "id", dup_keys = "last"))
  expect_equal(nrow(cmp$value_diff), 0L)
})

test_that("dup_keys = 'keep_all' pairs duplicates positionally within each key", {
  a <- data.frame(id = c(1, 1, 1, 2), x = c(10, 20, 30, 40))
  b <- data.frame(id = c(1, 1, 2),    x = c(10, 20, 40))
  cmp <- suppressMessages(ks_compare(a, b, by = "id", dup_keys = "keep_all"))
  rd <- cmp$row_diff
  expect_equal(sum(rd$status == "matched"), 3L)   # (1->1, 1->1, 2->2)
  expect_equal(sum(rd$status == "base_only"), 1L) # third id=1 row in base
  expect_equal(cmp$meta$matching$dup_strategy, "keep_all")
  expect_equal(cmp$meta$matching$n_pairs_created, 3L)
})

test_that("dup_keys = 'all_pairs' produces cartesian-pair matches", {
  a <- data.frame(id = c(1, 1), x = c(10, 20))
  b <- data.frame(id = c(1, 1), x = c(10, 20))
  cmp <- suppressMessages(ks_compare(a, b, by = "id", dup_keys = "all_pairs"))
  expect_equal(sum(cmp$row_diff$status == "matched"), 4L)
})

test_that("dup_keys = 'all_pairs' warns when cardinalities differ", {
  a <- data.frame(id = c(1, 1),    x = c(10, 20))
  b <- data.frame(id = c(1, 1, 1), x = c(10, 20, 30))
  expect_warning(
    suppressMessages(
      ks_compare(a, b, by = "id", dup_keys = "all_pairs")
    ),
    class = "ksCompare_all_pairs_cardinality"
  )
})

test_that("meta$matching is populated for keyed_unique", {
  a <- data.frame(id = 1:3, x = 1:3)
  b <- data.frame(id = 1:3, x = c(1, 2, 9))
  cmp <- ks_compare(a, b, by = "id")
  expect_equal(cmp$meta$matching$strategy, "keyed_unique")
  expect_equal(cmp$meta$matching$n_base_dup_keys, 0L)
})

test_that("meta$matching is populated for position match", {
  a <- data.frame(x = 1:3)
  b <- data.frame(x = 1:3)
  cmp <- ks_compare(a, b)
  expect_equal(cmp$meta$matching$strategy, "position")
})
