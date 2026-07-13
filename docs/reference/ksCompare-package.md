# ksCompare: smart data frame comparison

A tidyverse-native engine for comparing two data frames in the spirit of
SAS `PROC COMPARE`, with extensions for clinical / pharma QC workflows.

## Getting started

The single entry point is
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md):

    library(ksCompare)

    cmp <- ks_compare(adsl_prod, adsl_qc, by = "USUBJID")
    cmp                       # console summary
    tibble::as_tibble(cmp)    # long cell-level diff table
    ks_report_html(cmp, "qc.html")

## Building blocks

- [`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
  – main entry point.

- [`ks_tol()`](https://al-garik.github.io/ksCompare/reference/ks_tol.md)
  – numeric tolerance (abs / rel / ULP, per-column).

- [`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md)
  – NA, SAS special-missing, label / format, string normalisation,
  time-zone handling.

- [`ks_assert_clean()`](https://al-garik.github.io/ksCompare/reference/ks_assert_clean.md)
  /
  [`ks_pointblank_step()`](https://al-garik.github.io/ksCompare/reference/ks_assert_clean.md)
  – pipeline gates.

- [`ks_report_html()`](https://al-garik.github.io/ksCompare/reference/ks_report_html.md)
  /
  [`ks_report_xlsx()`](https://al-garik.github.io/ksCompare/reference/ks_report_xlsx.md)
  – shareable reports.

- [`as_outbase()`](https://al-garik.github.io/ksCompare/reference/sas-out.md)
  /
  [`as_outcomp()`](https://al-garik.github.io/ksCompare/reference/sas-out.md)
  /
  [`as_outdif()`](https://al-garik.github.io/ksCompare/reference/sas-out.md)
  /
  [`as_outnoequal()`](https://al-garik.github.io/ksCompare/reference/sas-out.md)
  – PROC COMPARE-style output datasets.

- [`ks_sysinfo()`](https://al-garik.github.io/ksCompare/reference/ks_sysinfo.md)
  – SAS `&SYSINFO`-compatible bitmask for CI.

## Data validation engine

- [`ks_check_rules()`](https://al-garik.github.io/ksCompare/reference/ks_check_rules.md)
  – apply declarative row-wise validation rules.

- [`ks_collapse_check_msgs()`](https://al-garik.github.io/ksCompare/reference/ks_collapse_check_msgs.md)
  – flatten list-column messages.

- [`ks_check_summary()`](https://al-garik.github.io/ksCompare/reference/ks_check_summary.md)
  – overview and violation statistics.

- [`ks_check_report_html()`](https://al-garik.github.io/ksCompare/reference/ks_check_report_html.md)
  – self-contained HTML validation report.

- [`ks_compare_check_state()`](https://al-garik.github.io/ksCompare/reference/ks_compare_check_state.md)
  – diff two validation runs.

- [`ks_save2xlsx_by()`](https://al-garik.github.io/ksCompare/reference/ks_save2xlsx_by.md)
  – grouped Excel workbook with conditional formatting.

[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
also accepts file paths directly (`.sas7bdat`, `.xpt`, `.parquet`,
`.feather`/`.arrow`, `.rds`, `.rdata`/`.rda`, `.csv`, `.tsv`) and
chooses a reader based on the file extension.

## Vignettes

- [`vignette("getting-started", package = "ksCompare")`](https://al-garik.github.io/ksCompare/articles/getting-started.md)

- [`vignette("from-proc-compare", package = "ksCompare")`](https://al-garik.github.io/ksCompare/articles/from-proc-compare.md)

- [`vignette("smart-features", package = "ksCompare")`](https://al-garik.github.io/ksCompare/articles/smart-features.md)

- [`vignette("reports", package = "ksCompare")`](https://al-garik.github.io/ksCompare/articles/reports.md)

- [`vignette("pipeline-gates", package = "ksCompare")`](https://al-garik.github.io/ksCompare/articles/pipeline-gates.md)

- [`vignette("powerful_data_validation_engine", package = "ksCompare")`](https://al-garik.github.io/ksCompare/articles/powerful_data_validation_engine.md)

## See also

Useful links:

- <https://al-garik.github.io/ksCompare/>

- <https://github.com/al-garik/ksCompare>

- <https://crow16384.r-universe.dev/ksCompare>

- Report bugs at <https://github.com/al-garik/ksCompare/issues>

## Author

**Maintainer**: Igor Aleschenkov <igor.aleschenkov@gmail.com>

Authors:

- Igor Aleschenkov <igor.aleschenkov@gmail.com>
