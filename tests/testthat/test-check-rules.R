test_that("ks_check_rules supports first and all modes", {
  df <- tibble::tibble(
    id = 1:4,
    age = c(35, -2, -5, 18),
    sex = c("F", "M", NA, NA)
  )
  rules <- list(
    age_non_negative = list(expr = ~ age < 0, msg = "Negative age"),
    sex_not_missing = list(expr = ~ is.na(sex), msg = "Missing sex")
  )

  first <- ks_check_rules(df, rules, mode = "first")
  expect_equal(first$check_msgs[[1]], "OK")
  expect_equal(first$check_msgs[[2]], "Negative age")
  expect_equal(first$check_msgs[[3]], "Negative age")
  expect_equal(first$check_msgs[[4]], "Missing sex")

  all <- ks_check_rules(df, rules, mode = "all")
  expect_equal(all$check_msgs[[1]], "OK")
  expect_equal(all$check_msgs[[2]], "Negative age")
  expect_equal(all$check_msgs[[3]], c("Negative age", "Missing sex"))
  expect_equal(all$check_msgs[[4]], "Missing sex")
})

test_that("ks_check_rules handles empty rules, zero-row data, and NA hits", {
  df <- tibble::tibble(id = 1:2, x = c(1, 2))
  out_empty <- ks_check_rules(df, rules = list(), mode = "all")
  expect_equal(out_empty$check_msgs, list("OK", "OK"))

  zero <- tibble::tibble(id = integer(), x = numeric())
  out_zero <- ks_check_rules(zero, rules = list(), mode = "first")
  expect_equal(length(out_zero$check_msgs), 0L)

  rules_na <- list(
    na_rule = list(expr = ~ ifelse(x > 1, NA, FALSE), msg = "Should not hit")
  )
  out_na <- ks_check_rules(df, rules_na, mode = "all")
  expect_equal(out_na$check_msgs, list("OK", "OK"))
})

test_that("ks_collapse_check_msgs collapses and validates required column", {
  df <- tibble::tibble(
    id = 1:2,
    check_msgs = list(c("A", "B"), "OK")
  )
  collapsed <- ks_collapse_check_msgs(df, col = check_msgs)
  expect_equal(collapsed$check_msgs, c("A; B", "OK"))
  expect_error(ks_collapse_check_msgs(df), "col")
})

test_that("ks_compare_check_state marks new changed and unchanged rows", {
  previous <- tibble::tibble(
    id = c(1, 2),
    msg = c("OK", "X")
  )
  current <- tibble::tibble(
    id = c(1, 2, 3),
    msg = c("OK", "Y", "OK")
  )

  out <- ks_compare_check_state(
    current,
    previous,
    key_cols = "id",
    compare_cols = "msg"
  )
  expect_equal(out$status, c("", "changed", "new"))

  out_first <- ks_compare_check_state(
    current,
    previous = NULL,
    key_cols = "id",
    compare_cols = "msg"
  )
  expect_true(all(out_first$status == "new"))
})

test_that("ks_check_summary returns expected overview and violations", {
  checked <- tibble::tibble(
    id = 1:4,
    check_msgs = list(
      "OK",
      c("A", "B"),
      c("A"),
      "OK"
    )
  )

  s <- ks_check_summary(checked)
  expect_true(all(c("overview", "violations") %in% names(s)))
  expect_equal(s$overview$rows_total[[1]], 4)
  expect_equal(s$overview$n_pass[[1]], 2)
  expect_equal(s$overview$n_fail[[1]], 2)

  expect_equal(s$violations$message[[1]], "A")
  expect_equal(s$violations$n_rows[[1]], 2)
})

test_that("ks_check_report_html returns tag output and writes file", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("reactable")

  checked <- tibble::tibble(
    id = 1:3,
    val = c(10, 20, 30),
    check_msgs = list("OK", "Issue A", c("Issue A", "Issue B"))
  )

  page <- ks_check_report_html(checked, path = NA)
  expect_true(inherits(page, c("shiny.tag", "shiny.tag.list")))

  tmp <- withr::local_tempfile(fileext = ".html")
  out <- ks_check_report_html(checked, path = tmp, title = "Check report")
  expect_true(file.exists(out))
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(txt, "Check report", fixed = TRUE)
  expect_match(txt, "Violation summary", fixed = TRUE)
})

test_that("ks_save2xlsx_by writes grouped worksheets", {
  skip_if_not_installed("openxlsx2")

  checked <- tibble::tibble(
    type = c("A", "A", "B"),
    id = 1:3,
    check_msgs = c("OK", "Issue A", "Issue B")
  )
  tmp <- withr::local_tempfile(fileext = ".xlsx")

  ks_save2xlsx_by(
    checked,
    file = tmp,
    colors = c("Issue" = "#FFCCCC", "OK" = "#CCFFCC"),
    split_col = type,
    msg_col = check_msgs
  )

  expect_true(file.exists(tmp))
  wb <- openxlsx2::wb_load(tmp)
  sheets <- openxlsx2::wb_get_sheet_names(wb)
  expect_true(all(c("A", "B") %in% sheets))
})