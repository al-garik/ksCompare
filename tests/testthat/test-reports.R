test_that("ks_report_html returns an htmltools tag when path = NA", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("reactable")
  cmp <- ks_compare(
    data.frame(id = 1:3, x = c(1, 2, 3)),
    data.frame(id = 1:3, x = c(1, 2, 4)),
    by = "id"
  )
  page <- ks_report_html(cmp, path = NA)
  expect_true(inherits(page, c("shiny.tag", "shiny.tag.list")))
})

test_that("ks_report_html auto-generates a filename when path is NULL", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("reactable")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  a <- data.frame(id = 1:3, x = c(1, 2, 3))
  b <- data.frame(id = 1:3, x = c(1, 2, 4))
  cmp <- ks_compare(a, b, by = "id")
  out <- ks_report_html(cmp)
  expect_true(file.exists(out))
  expect_match(basename(out), "^ksCompare_.*\\.html$")
})

test_that("ks_report_html appends .html when extension missing", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("reactable")
  d <- withr::local_tempdir()
  cmp <- ks_compare(
    data.frame(id = 1:2, x = 1:2),
    data.frame(id = 1:2, x = c(1, 9)),
    by = "id"
  )
  out <- ks_report_html(cmp, path = file.path(d, "myreport"))
  expect_true(file.exists(out))
  expect_equal(tools::file_ext(out), "html")
})

test_that("ks_report_html writes a self-contained HTML file", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("reactable")
  cmp <- ks_compare(
    data.frame(id = 1:3, x = c(1, 2, 3)),
    data.frame(id = 1:3, x = c(1, 2, 4)),
    by = "id"
  )
  tmp <- withr::local_tempfile(fileext = ".html")
  ks_report_html(cmp, path = tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 1024)
  txt <- readLines(tmp, n = 200, warn = FALSE)
  expect_true(any(grepl("ksCompare report", txt, fixed = TRUE)))
})

test_that("ks_report_html rejects non-comparison input", {
  skip_if_not_installed("htmltools")
  expect_error(ks_report_html("nope"), class = "ksCompare_error")
})

test_that("ks_report_html honours title, subtitle, and theme", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("reactable")
  cmp <- ks_compare(
    data.frame(id = 1:3, x = c(1, 2, 3)),
    data.frame(id = 1:3, x = c(1, 2, 4)),
    by = "id"
  )
  tmp <- withr::local_tempfile(fileext = ".html")
  ks_report_html(
    cmp,
    path = tmp,
    title = "ADSL QC",
    subtitle = "Run 42",
    theme = "slate"
  )
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "ADSL QC", fixed = TRUE)
  expect_match(txt, "Run 42", fixed = TRUE)
  expect_match(txt, "ks-theme-slate", fixed = TRUE)
  expect_match(txt, "ks-toc", fixed = TRUE)
  expect_match(txt, "ks-status", fixed = TRUE)
})

test_that("ks_report_xlsx writes a workbook with expected sheets", {
  skip_if_not_installed("openxlsx2")
  cmp <- ks_compare(
    data.frame(id = 1:3, x = c(1, 2, 3)),
    data.frame(id = 1:3, x = c(1, 2, 4)),
    by = "id"
  )
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  ks_report_xlsx(cmp, path = tmp)
  expect_true(file.exists(tmp))
  wb <- openxlsx2::wb_load(tmp)
  sheets <- openxlsx2::wb_get_sheet_names(wb)
  for (n in c(
    "Summary",
    "Schema",
    "KeyDiff",
    "Values",
    "Patterns",
    "UnmatchedRows",
    "FirstLastUnequal",
    "OUT_BASE",
    "OUT_COMP",
    "OUT_DIF",
    "OUT_NOEQUAL",
    "Manifest"
  )) {
    expect_true(n %in% sheets, info = paste("missing sheet:", n))
  }
})
