test_that("ks_tol validates inputs", {
  expect_snapshot(error = TRUE, ks_tol(abs = -1))
  expect_snapshot(error = TRUE, ks_tol(rel = "x"))
  expect_snapshot(error = TRUE, ks_tol(per_column = list(a = 1)))
})

test_that("ks_tol_for picks per-column override", {
  tol <- ks_tol(abs = 0, per_column = list(price = ks_tol(abs = 0.005)))
  expect_equal(ks_tol_for(tol, "price")$abs, 0.005)
  expect_equal(ks_tol_for(tol, "other")$abs, 0)
})

test_that("ks_comp_options validates choices", {
  expect_snapshot(error = TRUE, ks_comp_options(str_case = "upper"))
  expect_snapshot(error = TRUE, ks_comp_options(na_equal = NA))
})

test_that("ks_comp_options validates the path argument", {
  withr::local_options(list(ksCompare.path = NULL))
  expect_error(ks_comp_options(path = NA_character_))
  expect_error(ks_comp_options(path = ""))
  expect_error(ks_comp_options(path = c("a", "b")))
  expect_null(ks_comp_options()$path)
  expect_equal(ks_comp_options(path = "out/qc")$path, "out/qc")
})

test_that("ks_comp_options reads ksCompare.* getOption() defaults", {
  withr::with_options(
    list(ksCompare.path = "out/qc",
         ksCompare.str_case = "fold",
         ksCompare.na_equal = FALSE),
    {
      o <- ks_comp_options()
      expect_equal(o$path, "out/qc")
      expect_equal(o$str_case, "fold")
      expect_false(o$na_equal)
    }
  )
})

test_that("explicit args override ksCompare.* globals", {
  withr::with_options(
    list(ksCompare.path = "out/global"),
    {
      expect_equal(ks_comp_options(path = "out/explicit")$path, "out/explicit")
    }
  )
})

test_that("ks_set_comp_options forwards to options() and rejects unknown names", {
  old <- ks_set_comp_options(path = "out/qc", str_case = "fold")
  on.exit(do.call(options, old), add = TRUE)
  expect_equal(getOption("ksCompare.path"), "out/qc")
  expect_equal(getOption("ksCompare.str_case"), "fold")
  expect_equal(ks_comp_options()$path, "out/qc")
  expect_error(ks_set_comp_options(bogus = 1))
  expect_error(ks_set_comp_options("unnamed"))
})

test_that("ks_set_comp_options accepts a ks_comp_options object", {
  o <- ks_comp_options(path = "out/from_obj", str_case = "fold")
  prev <- ks_set_comp_options(o)
  on.exit(do.call(options, prev), add = TRUE)
  expect_equal(getOption("ksCompare.path"), "out/from_obj")
  expect_equal(getOption("ksCompare.str_case"), "fold")
})
