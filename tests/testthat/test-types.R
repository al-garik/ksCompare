test_that("ks_kind classifies common types", {
  expect_equal(ks_kind(1L), "integer")
  expect_equal(ks_kind(1.0), "double")
  expect_equal(ks_kind("a"), "character")
  expect_equal(ks_kind(TRUE), "logical")
  expect_equal(ks_kind(factor("a")), "factor")
  expect_equal(ks_kind(Sys.Date()), "date")
  expect_equal(ks_kind(Sys.time()), "datetime")
  expect_equal(ks_kind(list(1)), "list")
})

test_that("ks_common_ptype safe mode allows int/double and factor/char", {
  expect_equal(ks_common_ptype(1L, 1.0)$ptype, double())
  expect_equal(
    ks_common_ptype(factor("a"), "a", mode = "safe")$ptype,
    character()
  )
})

test_that("ks_common_ptype strict refuses lossy casts", {
  expect_null(ks_common_ptype(1, "1", mode = "strict")$ptype)
})

test_that("ks_common_ptype lossy allows numeric<->character", {
  expect_equal(ks_common_ptype(1, "1", mode = "lossy")$ptype, character())
})
