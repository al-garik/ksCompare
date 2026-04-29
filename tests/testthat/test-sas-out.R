test_that("as_outbase / as_outcomp / as_outdif return expected shapes", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
  b <- data.frame(id = 1:3, x = c(1, 2, 4), y = c("a", "B", "c"))
  cmp <- ks_compare(a, b, by = "id")

  ob <- as_outbase(cmp)
  oc <- as_outcomp(cmp)
  od <- as_outdif(cmp)

  expect_s3_class(ob, "tbl_df")
  expect_true("id" %in% names(ob))
  expect_setequal(setdiff(names(ob), "id"), c("key_id", "x", "y"))
  expect_equal(nrow(ob), 2L)
  expect_equal(nrow(oc), 2L)
  expect_equal(nrow(od), 2L)
  # diff column should hold base - comp for numeric x
  x_diff <- od$x[!is.na(od$x)]
  expect_equal(x_diff, -1)
})

test_that("as_outnoequal returns long-format diffs", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  cmp <- ks_compare(a, b, by = "id")
  noeq <- as_outnoequal(cmp)
  expect_equal(nrow(noeq), 1L)
  expect_equal(noeq$column_base, "x")
  expect_true("id" %in% names(noeq))
})

test_that("OUT helpers reject non-comparison input", {
  expect_snapshot(error = TRUE, as_outbase("not a comparison"))
})

test_that("ks_sysinfo sets value-unequal and obs-only bits", {
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = c(1, 2, 4), x = c(1, 2, 3))
  cmp <- ks_compare(a, b, by = "id")
  si <- ks_sysinfo(cmp)
  bits <- attr(si, "bits")
  expect_gt(as.integer(si), 0L)
  expect_gt(bits[["base has observation not in compare"]], 0L)
  expect_gt(bits[["compare has observation not in base"]], 0L)
})

test_that("ks_sysinfo flags variable-only bits", {
  a <- data.frame(id = 1:2, x = 1:2, only_a = 1:2)
  b <- data.frame(id = 1:2, x = 1:2, only_b = 1:2)
  cmp <- ks_compare(a, b, by = "id")
  bits <- attr(ks_sysinfo(cmp), "bits")
  expect_gt(bits[["base has variable not in compare"]], 0L)
  expect_gt(bits[["compare has variable not in base"]], 0L)
})

test_that("ks_sysinfo is zero for identical frames", {
  a <- data.frame(id = 1:3, x = 1:3)
  cmp <- ks_compare(a, a, by = "id")
  expect_equal(as.integer(unclass(ks_sysinfo(cmp))), 0L)
})
