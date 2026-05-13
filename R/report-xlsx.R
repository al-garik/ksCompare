#' Render an Excel workbook report
#'
#' Writes a multi-sheet workbook summarising a `ks_comparison`. Sheets:
#' `Summary`, `Schema`, `KeyDiff`, `Values`, `Patterns`,
#' `OUT_BASE`, `OUT_COMP`, `OUT_DIF`, `OUT_NOEQUAL`, `Manifest`. Cells with
#' value differences are highlighted on the wide `OUT_DIF` sheet.
#'
#' @param x A `ks_comparison` object.
#' @param path File path for the workbook. When `NULL` (default), the
#'   filename is auto-generated from the comparison's `base_name` /
#'   `comp_name` and placed in `options$path` (if set on
#'   [ks_comp_options()]) or the current working directory, e.g.
#'   `ksCompare_adsl_vs_adsl_qc.xlsx`. A `.xlsx` extension is appended
#'   if missing. A bare filename is resolved against `options$path`
#'   when that has been set.
#' @param highlight If `TRUE` (default), apply conditional formatting to
#'   highlight numeric `OUT_DIF` cells whose magnitude is above `threshold`.
#' @param threshold Numeric threshold for highlighting (default `0`, so any
#'   non-zero diff is highlighted).
#'
#' @return Invisibly returns `path`.
#'
#' @examples
#' \dontrun{
#'   cmp <- ks_compare(iris, iris, by = "Species")
#'   ks_report_xlsx(cmp, "report.xlsx")
#'
#'   # Auto-named output in a pinned folder
#'   cmp2 <- ks_compare(
#'     iris, iris, by = "Species",
#'     options = ks_comp_options(path = tempfile("ksCompare_"))
#'   )
#'   ks_report_xlsx(cmp2)
#' }
#' @export
ks_report_xlsx <- function(x, path = NULL, highlight = TRUE, threshold = 0) {
  ks_assert_comparison(x)
  ks_check_installed("openxlsx2", reason = "for ks_report_xlsx()")

  wb <- openxlsx2::wb_workbook()

  s <- summary(x)
  summary_df <- tibble::tibble(
    metric = c(
      "Base rows",
      "Comp rows",
      "Matched rows",
      "Base-only rows",
      "Comp-only rows",
      "Matched columns",
      "Base-only columns",
      "Comp-only columns",
      "Value differences",
      "Columns with diffs"
    ),
    value = c(
      s$n_base_rows,
      s$n_comp_rows,
      s$n_matched_rows,
      s$n_base_only_rows,
      s$n_comp_only_rows,
      s$n_matched_columns,
      s$n_base_only_columns,
      s$n_comp_only_columns,
      s$n_value_diffs,
      s$n_columns_with_diffs
    )
  )

  add_sheet <- function(name, df) {
    if (is.null(df) || nrow(df) == 0L) {
      df <- tibble::tibble(note = "(none)")
    }
    wb$add_worksheet(name)
    wb$add_data(sheet = name, x = as.data.frame(df))
    wb$set_col_widths(sheet = name, cols = seq_len(ncol(df)), widths = "auto")
  }

  add_sheet("Summary", summary_df)
  add_sheet("Schema", x$schema_diff)
  add_sheet("KeyDiff", x$key_diff)
  add_sheet("Values", x$value_diff)
  add_sheet("DiffCauses", tryCatch(ks_cause_summary(x), error = function(e) tibble::tibble()))
  add_sheet("RowHotspots", tryCatch(ks_row_diff_summary(x), error = function(e) tibble::tibble()))
  add_sheet("Patterns", x$pattern_summary %||% tibble::tibble())
  add_sheet("UnmatchedRows", x$unmatched_rows %||% tibble::tibble())
  add_sheet("FirstLastUnequal", x$first_last_unequal %||% tibble::tibble())

  out_base <- try(as_outbase(x), silent = TRUE)
  out_comp <- try(as_outcomp(x), silent = TRUE)
  out_dif <- try(as_outdif(x), silent = TRUE)
  out_neq <- try(as_outnoequal(x), silent = TRUE)

  if (!inherits(out_base, "try-error")) add_sheet("OUT_BASE", out_base)
  if (!inherits(out_comp, "try-error")) add_sheet("OUT_COMP", out_comp)
  if (!inherits(out_dif, "try-error")) add_sheet("OUT_DIF", out_dif)
  if (!inherits(out_neq, "try-error")) add_sheet("OUT_NOEQUAL", out_neq)

  manifest_df <- tibble::tibble(
    field = names(x$manifest),
    value = vapply(x$manifest, function(v) format(v)[[1]], character(1))
  )
  add_sheet("Manifest", manifest_df)

  if (
    highlight &&
      !inherits(out_dif, "try-error") &&
      !is.null(out_dif) &&
      nrow(out_dif) > 0L
  ) {
    ks_xlsx_highlight_dif(wb, out_dif, threshold = threshold)
  }

  if (is.null(path)) {
    path <- ks_default_report_path(x, ext = "xlsx")
  } else {
    if (!is.character(path) || length(path) != 1L || is.na(path)) {
      ks_abort("{.arg path} must be {.code NULL} or a single file path.")
    }
    if (!nzchar(tools::file_ext(path))) {
      path <- paste0(path, ".xlsx")
    }
    path <- ks_resolve_output_path(path, x)
  }
  ks_ensure_dir(dirname(path))
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  cli::cli_alert_success("Excel report written to {.file {path}}")
  invisible(path)
}

ks_xlsx_highlight_dif <- function(wb, df, threshold) {
  numeric_cols <- which(vapply(df, is.numeric, logical(1)))
  if (length(numeric_cols) == 0L) {
    return(invisible())
  }
  n_rows <- nrow(df)
  for (j in numeric_cols) {
    col_letter <- openxlsx2::int2col(j)
    rng <- sprintf("%s2:%s%d", col_letter, col_letter, n_rows + 1L)
    rule <- sprintf("ABS(%s2)>%g", col_letter, threshold)
    try(
      openxlsx2::wb_add_conditional_formatting(
        wb,
        sheet = "OUT_DIF",
        dims = rng,
        rule = rule,
        style = openxlsx2::create_dxfs_style(
          bg_fill = openxlsx2::wb_color(hex = "FFF2CC")
        )
      ),
      silent = TRUE
    )
  }
  invisible()
}
