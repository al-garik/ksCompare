#' Construct a `ks_comparison` S3 object
#'
#' Internal constructor; users should call [ks_compare()] instead.
#'
#' @keywords internal
#' @noRd
new_ks_comparison <- function(
  meta,
  schema_diff,
  column_suggestions,
  key_diff,
  row_diff,
  value_diff,
  pattern_summary,
  options,
  tolerance,
  manifest
) {
  structure(
    list(
      meta = meta,
      schema_diff = schema_diff,
      column_suggestions = column_suggestions,
      key_diff = key_diff,
      row_diff = row_diff,
      value_diff = value_diff,
      pattern_summary = pattern_summary,
      options = options,
      tolerance = tolerance,
      manifest = manifest
    ),
    class = "ks_comparison"
  )
}

#' @export
as_tibble.ks_comparison <- function(x, ...) {
  x$value_diff
}

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' Summary statistics for a ks_comparison
#'
#' Computes a small list of headline counts that drive the printed
#' summary, the HTML report KPI cards, and CI gates.
#'
#' @param object A `ks_comparison` returned by [ks_compare()].
#' @param ... Unused; present for S3 conformance.
#' @return A `ks_comparison_summary` list (also pretty-printed via
#'   `print()`) with components:
#'   - `n_base_rows`, `n_comp_rows`: input row counts.
#'   - `n_matched_rows`, `n_base_only_rows`, `n_comp_only_rows`: row
#'     counts after matching.
#'   - `n_matched_columns`, `n_base_only_columns`,
#'     `n_comp_only_columns`: schema-side counts.
#'   - `n_value_diffs`: number of differing cells in matched rows.
#'   - `n_columns_with_diffs`: how many distinct columns hold those
#'     differences.
#' @seealso [ks_glance()] for a tibble version, [ks_tidy()] for the
#'   long cell-level table.
#' @export
#' @examples
#' cmp <- ks_compare(
#'   data.frame(id = 1:3, x = c(1, 2, 3)),
#'   data.frame(id = 1:3, x = c(1, 2, 4)),
#'   by = "id"
#' )
#' summary(cmp)
summary.ks_comparison <- function(object, ...) {
  rd <- object$row_diff
  out <- list(
    n_base_rows = object$meta$n_base_rows,
    n_comp_rows = object$meta$n_comp_rows,
    n_matched_rows = sum(rd$status == "matched"),
    n_base_only_rows = sum(rd$status == "base_only"),
    n_comp_only_rows = sum(rd$status == "comp_only"),
    n_matched_columns = sum(object$schema_diff$side == "matched"),
    n_base_only_columns = sum(object$schema_diff$side == "base_only"),
    n_comp_only_columns = sum(object$schema_diff$side == "comp_only"),
    n_value_diffs = nrow(object$value_diff),
    n_columns_with_diffs = length(unique(object$value_diff$column_base))
  )
  class(out) <- c("ks_comparison_summary", "list")
  out
}

#' @export
print.ks_comparison_summary <- function(x, ...) {
  cli::cli_h1("ksCompare summary")
  cli::cli_dl(c(
    "Rows (base / comp)" = "{x$n_base_rows} / {x$n_comp_rows}",
    "Matched / base-only / comp-only rows" = "{x$n_matched_rows} / {x$n_base_only_rows} / {x$n_comp_only_rows}",
    "Matched / base-only / comp-only columns" = "{x$n_matched_columns} / {x$n_base_only_columns} / {x$n_comp_only_columns}",
    "Cells with diffs" = "{x$n_value_diffs} (in {x$n_columns_with_diffs} column{?s})"
  ))
  invisible(x)
}

#' @export
print.ks_comparison <- function(x, ...) {
  s <- summary(x)
  cli::cli_h1("ksCompare comparison")
  cli::cli_dl(c(
    "Base" = "{.field {x$meta$base_name}} ({x$meta$n_base_rows} \u00d7 {x$meta$n_base_cols})",
    "Comp" = "{.field {x$meta$comp_name}} ({x$meta$n_comp_rows} \u00d7 {x$meta$n_comp_cols})",
    "Keys" = if (nrow(x$meta$keys) > 0L) {
      paste(x$meta$keys$base, collapse = ", ")
    } else {
      "<by row position>"
    }
  ))

  schema <- x$schema_diff
  n_kind_mismatch <- sum(!schema$kind_match[schema$side == "matched"], na.rm = TRUE)
  n_label_mismatch <- sum(!schema$label_match[schema$side == "matched"], na.rm = TRUE)
  n_format_mismatch <- sum(!schema$format_match[schema$side == "matched"], na.rm = TRUE)

  cli::cli_h2("Schema")
  cli::cli_bullets(c(
    "*" = "Matched columns: {.val {s$n_matched_columns}}",
    if (s$n_base_only_columns > 0) {
      c("!" = "Base-only columns: {.val {schema$base[schema$side == 'base_only']}}")
    },
    if (s$n_comp_only_columns > 0) {
      c("!" = "Comp-only columns: {.val {schema$comp[schema$side == 'comp_only']}}")
    },
    if (n_kind_mismatch > 0) {
      c("!" = "Type mismatches: {.val {n_kind_mismatch}}")
    },
    if (n_label_mismatch > 0) {
      c("i" = "Label mismatches: {.val {n_label_mismatch}}")
    },
    if (n_format_mismatch > 0) {
      c("i" = "SAS format mismatches: {.val {n_format_mismatch}}")
    }
  ))

  cli::cli_h2("Rows")
  matching <- x$meta$matching
  if (!is.null(matching)) {
    strat_msg <- switch(
      matching$strategy %||% "",
      position = "Strategy: row-position match (no key columns)",
      keyed_unique = sprintf(
        "Strategy: keyed match on {.val %s} (unique on both sides)",
        paste(matching$keys$base, collapse = ", ")
      ),
      keyed_dup_first = sprintf(
        "Strategy: keyed match on {.val %s}, duplicates resolved with {.code first}",
        paste(matching$keys$base, collapse = ", ")
      ),
      keyed_dup_last = sprintf(
        "Strategy: keyed match on {.val %s}, duplicates resolved with {.code last}",
        paste(matching$keys$base, collapse = ", ")
      ),
      keyed_dup_keep_all = sprintf(
        "Strategy: keyed match on {.val %s}, duplicates paired positionally ({.code keep_all})",
        paste(matching$keys$base, collapse = ", ")
      ),
      keyed_dup_all_pairs = sprintf(
        "Strategy: keyed match on {.val %s}, duplicates expanded ({.code all_pairs})",
        paste(matching$keys$base, collapse = ", ")
      ),
      sprintf("Strategy: %s", matching$strategy %||% "unknown")
    )
    cli::cli_bullets(c("*" = strat_msg))
    if ((matching$n_base_dup_keys %||% 0L) + (matching$n_comp_dup_keys %||% 0L) > 0L) {
      cli::cli_bullets(c(
        "!" = sprintf(
          "Duplicate keys: {.val %d} (base), {.val %d} (comp)",
          matching$n_base_dup_keys, matching$n_comp_dup_keys
        )
      ))
    }
  }
  cli::cli_bullets(c(
    "*" = "Matched: {.val {s$n_matched_rows}}",
    "*" = "Base-only: {.val {s$n_base_only_rows}}",
    "*" = "Comp-only: {.val {s$n_comp_only_rows}}"
  ))

  cli::cli_h2("Values")
  if (s$n_value_diffs == 0L) {
    cli::cli_alert_success("No value differences in matched cells.")
  } else {
    cli::cli_alert_warning(
      "{.val {s$n_value_diffs}} value difference{?s} across {.val {s$n_columns_with_diffs}} column{?s}."
    )
    top <- utils::head(
      dplyr::count(x$value_diff, .data$column_base, sort = TRUE),
      5L
    )
    for (i in seq_len(nrow(top))) {
      cli::cli_bullets(c(
        "*" = "{.field {top$column_base[[i]]}}: {.val {top$n[[i]]}} diff{?s}"
      ))
    }
    if (s$n_columns_with_diffs > 5L) {
      cli::cli_alert_info(
        "(...) {s$n_columns_with_diffs - 5L} more column{?s} with differences."
      )
    }
  }

  if (!is.null(x$pattern_summary) && nrow(x$pattern_summary) > 0L) {
    cli::cli_h2("Patterns")
    ps <- x$pattern_summary
    for (i in seq_len(nrow(ps))) {
      pct <- round(ps$coverage[[i]] * 100)
      detail <- if (is.na(ps$detail[[i]])) "" else paste0(" [", ps$detail[[i]], "]")
      col_label <- if (is.na(ps$column[[i]])) "<all columns>" else ps$column[[i]]
      cli::cli_bullets(c(
        "*" = "{.field {col_label}}: {.emph {ps$pattern[[i]]}} ({pct}%){detail}"
      ))
    }
  }

  if (!is.null(x$column_suggestions) && nrow(x$column_suggestions) > 0L) {
    cli::cli_h2("Column rename suggestions")
    sug <- x$column_suggestions
    for (i in seq_len(nrow(sug))) {
      cli::cli_bullets(c(
        "i" = "{.val {sug$base[[i]]}} \u21d4 {.val {sug$comp[[i]]}} (score {round(sug$score[[i]], 2)})"
      ))
    }
    cli::cli_alert_info(
      "Pass these to {.arg mapping} if you want them treated as matched."
    )
  }

  invisible(x)
}

#' Tidy a ks_comparison
#'
#' Returns the long-format cell-level diff table. Equivalent to
#' `as_tibble(cmp)` and identical to `cmp$value_diff`. The result has
#' one row per differing cell with columns:
#' `key_id`, `column_base`, `column_comp`, `kind`, `base`, `comp`,
#' `diff`, `note`.
#'
#' @param x A `ks_comparison`.
#' @param ... Unused.
#' @return A tibble of value differences.
#' @export
#' @examples
#' cmp <- ks_compare(
#'   data.frame(id = 1:2, x = c(1, 2)),
#'   data.frame(id = 1:2, x = c(1, 3)),
#'   by = "id"
#' )
#' ks_tidy(cmp)
ks_tidy <- function(x, ...) {
  UseMethod("ks_tidy")
}

#' @export
ks_tidy.ks_comparison <- function(x, ...) {
  x$value_diff
}

#' Glance at a ks_comparison
#'
#' Returns a one-row tibble with the same headline counts as
#' [summary.ks_comparison()]. Convenient for binding many comparisons
#' together in a QC loop.
#'
#' @param x A `ks_comparison`.
#' @param ... Unused.
#' @return A one-row tibble with columns `n_base_rows`, `n_comp_rows`,
#'   `n_matched_rows`, `n_base_only_rows`, `n_comp_only_rows`,
#'   `n_matched_columns`, `n_value_diffs`, `n_columns_with_diffs`.
#' @export
#' @examples
#' cmp <- ks_compare(
#'   data.frame(id = 1:2, x = c(1, 2)),
#'   data.frame(id = 1:2, x = c(1, 3)),
#'   by = "id"
#' )
#' ks_glance(cmp)
ks_glance <- function(x, ...) {
  UseMethod("ks_glance")
}

#' @export
ks_glance.ks_comparison <- function(x, ...) {
  s <- summary(x)
  tibble::tibble(
    n_base_rows = s$n_base_rows,
    n_comp_rows = s$n_comp_rows,
    n_matched_rows = s$n_matched_rows,
    n_base_only_rows = s$n_base_only_rows,
    n_comp_only_rows = s$n_comp_only_rows,
    n_matched_columns = s$n_matched_columns,
    n_value_diffs = s$n_value_diffs,
    n_columns_with_diffs = s$n_columns_with_diffs
  )
}
