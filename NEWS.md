# ksCompare 0.2.4

## New features — data validation engine

* `ks_check_rules(data, rules, mode)` applies a declarative list of
  one-sided-formula rules row-wise and records a `check_msgs` list-column.
  Two modes: `"first"` (stops at first failure per row, fast) and `"all"`
  (collects every failing message per row, thorough).

* `ks_collapse_check_msgs(data, col)` collapses the `check_msgs`
  list-column produced by `ks_check_rules()` to a plain character column,
  with a configurable `sep` argument.

* `ks_check_summary(data, col)` returns a named list with two tibbles:
  `overview` (total / passing / failing rows and pass rate) and
  `violations` (per-message counts and percentages, sorted descending).
  No file is written.

* `ks_check_report_html(data, path, ...)` renders a self-contained HTML
  check report using `htmltools` and `reactable` — no Quarto, no Pandoc.
  Follows the same `path = NULL / NA / "file.html"` routing as
  `ks_report_html()`.  Sections: KPI cards, violation summary table,
  row-detail table with OK / Issue status badges.

* `ks_compare_check_state(current, previous, key_cols, compare_cols)`
  diffs two successive `ks_check_rules()` runs and tags every row as
  `"new"`, `"changed"`, or `""` (unchanged). Designed for iterative QC
  workflows where only rows that regressed or improved need attention.

* `ks_save2xlsx_by(data, file, colors, split_col, msg_col, ...)` writes a
  grouped Excel workbook with one worksheet per unique value of `split_col`
  and applies conditional row highlighting based on substrings in `msg_col`.
  Built on `openxlsx2` for modern Excel output.

## Dependencies

* `purrr` moved from Suggests to Imports (required by validation engine).
* `ggplot2` added to Suggests (used in validation engine vignette).

# ksCompare 0.2.3

## New features

* `ks_compare()` gains a `loglevel` parameter to control console output verbosity.
* Comparison result object gains a `verdict` slot for pipeline integration.

# ksCompare 0.2.2

## Documentation

* Updated pkgdown site with complete reference documentation.
* Added comprehensive vignette "Powerful Data Validation Engine".

# ksCompare 0.2.1

## Bug fixes

* Improved robustness of key-column type coercion for mismatched/all-NA key types.

# ksCompare 0.2.0

## Performance

* Row matching for duplicate-key strategies `keep_all` and `all_pairs`
  (`ks_match_rows_keep_all()`, `ks_match_rows_all_pairs()`) now
  pre-allocates the per-group parts list and caches the level factor
  used for splitting, removing O(n^2) list-copy churn on data sets with
  many unique keys.
* The main comparison loop in `ks_compare()` now pre-allocates the
  `value_parts` accumulator and uses `order(..., method = "radix")` for
  the final stable sort of `value_diff`, materially speeding up wide
  comparisons (many matched column pairs) and large diff tables.
* `ks_schema_diff()` no longer rescans `meta_b$name` / `meta_c$name` for
  every column pair; it builds a name-indexed lookup once and reuses
  per-row metadata, and it hoists the `compare_labels` / `compare_formats`
  option checks out of the per-column loop.
* `ks_value_diff_one()` now slices the unequal cells once and reuses the
  result for `ks_explain_diffs()`, `format_cell()`, and `ks_na_flow()`,
  avoiding three redundant subsets on large columns; the type-mismatch
  branch likewise slices `base_col` / `comp_col` only once.

## Internal

* No user-facing API changes; all optimisations are behaviour-preserving.

# ksCompare 0.1.0

* Initial development release. ksCompare is a tidyverse-native engine for
  comparing two data frames in the spirit of SAS `PROC COMPARE`, with
  extensions for clinical/pharma workflows. Highlights:
  * `ks_compare()` orchestrator with explicit `by` keys, `by = "auto"`
    uniqueness inference, and explicit column `mapping`.
  * `ks_tol()` with absolute, relative, and ULP tolerance plus per-column
    overrides.
  * `ks_comp_options()` for NA semantics, SAS special-missing handling,
    label/format comparison, string trim/case/normalize toggles, and an
    optional `path` that pins every downstream `ks_report_*()` artefact
    to a single output folder. Every field also falls back to a
    `getOption("ksCompare.<arg>")` global, and `ks_set_comp_options()`
    forwards to `options()` for project-wide defaults.
  * `ks_comparison` S3 with `print()`, `summary()`, `as_tibble()`,
    `ks_tidy()`, `ks_glance()`.
  * Per-type cell diff for numeric (with ULP-aware compare), character,
    factor, date/datetime, and `haven_labelled` columns; encoding-safe
    string comparison.
  * Schema diff including label and SAS-format mismatches; format
    comparison is trailing-dot- and case-tolerant so haven import
    artefacts do not surface as false diffs.
  * Manifest with input hashes and package version for QC traceability.
  * `as_outbase()`, `as_outcomp()`, `as_outdif()`, `as_outnoequal()`
    helpers reshape diffs into PROC COMPARE-style tibbles, plus
    `ks_sysinfo()` for a SAS-compatible bitmask.
  * Smart features: auto-key inference, fuzzy column rename suggestions
    (Suggests-gated on `stringdist`), duplicate-key strategies (`first`,
    `last`, `keep_all`, `all_pairs`, `error`), and pattern detection
    (`constant_offset`, `constant_scale`, `sign_flip`, `integer_round`,
    `whitespace_only`, `trim_only`, `case_only`, `factor_recoded`).
  * Reports: `ks_report_html()` (self-contained `htmltools` +
    `reactable` page with sticky TOC, KPI cards, status pill, themed
    tables, an optional `group_by_key` mode that renders one collapsed
    block per key value, and a print-friendly stylesheet) and
    `ks_report_xlsx()` (openxlsx2 multi-sheet workbook with conditional
    formatting on `OUT_DIF`). The HTML report inlines every JS/CSS
    dependency into a single file, so no sibling `lib/` folder is
    written next to the report.
  * I/O: `ks_read_sas()` / `ks_read_xpt()` (haven wrappers preserving
    labelled metadata) and `ks_read_arrow()` (Parquet / Feather).
  * Ecosystem: `ks_assert_clean()` pipeline gate with
    `ks_pointblank_step()` adapter; `as_ks_comparison()` generic with
    method for `arsenal::comparedf`.
  * Documentation: pkgdown site, five long-form vignettes
    (`getting-started`, `from-proc-compare`, `smart-features`,
    `reports`, `pipeline-gates`), and CI workflows for `R-CMD-check`,
    `pkgdown`, and `test-coverage`.
