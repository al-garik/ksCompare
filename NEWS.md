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
