# ksCompare

> Smart data-frame comparison in the spirit of SAS `PROC COMPARE`.

[![Status: community-testing](https://img.shields.io/badge/status-community--testing-2A5B88.svg)](https://github.com/al-garik/ksCompare)

`ksCompare` is a tidyverse-native R package for comparing two data frames
with the rigor expected of clinical / pharma QC workflows. It combines
strict cell-by-cell comparison with review helpers, report generation,
and pipeline-friendly assertions.

## Why ksCompare exists

In clinical-trial programming, double programming is still one of the
most credible ways to QC derived datasets: one programmer builds the
production result, another independently recreates it, and the two
outputs are compared as evidence that they truly match. In SAS-based
workflows, `PROC COMPARE` has long been the standard tool for that final
check because it turns "these datasets look similar" into something
auditable and reviewable.

R has many general data-manipulation tools, but far less support for
this specific QC step as a first-class workflow. `ksCompare` is an
attempt to bring the spirit of `PROC COMPARE` into R and push it
further: compare structure and values, surface unmatched rows, support
tolerances and SAS-aware semantics, and produce interactive outputs that
make dataset QC easier to review, explain, and document.

<p align="center">
  <img src="man/figures/intro.gif" width="900" alt="Animated preview of generating a ksCompare HTML report and scrolling through the result" />
</p>

## Installation

Install the package from r-universe:

```r
install.packages(
  "ksCompare",
  repos = c(
    "https://crow16384.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

Some features rely on suggested packages:

- `htmltools` + `reactable` for self-contained HTML reports.
- `openxlsx2` for Excel export.
- `haven` and `arrow` for direct file-path input.
- `pointblank` and `arsenal` for workflow integrations.

## What it does

- Per-column tolerances: absolute, relative, **and ULP**.
- Intelligent type reconciliation, including `haven_labelled` and SAS
  special missings (`.A`–`.Z`, `._`).
- Automatic key inference (`by = "auto"`) and explicit duplicate-key
  strategies (`first`, `last`, `keep_all`, `all_pairs`, `error`).
- Triage helpers: `ks_glance()`, `ks_tidy()`, `ks_cause_summary()`,
  `ks_row_diff_summary()`, and `ks_unmatched_rows()`.
- Opt-in diff-pattern detection via `find_patterns = TRUE`.
- Rich console output (`cli`) plus self-contained HTML and Excel
  reports.
- Pipeline gates (`ks_assert_clean()`, `ks_pointblank_step()`).
- SAS-parity surface: `as_outbase()` / `as_outcomp()` / `as_outdif()` /
  `as_outnoequal()` and `ks_sysinfo()`.

`ks_compare()` accepts both in-memory data frames and file paths for
`.sas7bdat`, `.xpt`, `.parquet`, `.feather`, `.rds`, `.RData`, `.csv`,
and `.tsv` inputs.


## Quick start

```r
library(ksCompare)

base <- data.frame(
  id = 1:4,
  age = c(34, 41, 28, 55),
  arm = c("A", "A", "B", "B")
)
comp <- data.frame(
  id = 1:4,
  age = c(34, 41, 27, 55),
  arm = c("A", "A", "b", "B")
)

cmp <- ks_compare(base, comp, by = "id")
cmp
ks_glance(cmp)
ks_tidy(cmp)
ks_cause_summary(cmp)
ks_row_diff_summary(cmp)
```

If you want recurring diff shapes summarized, turn pattern detection on:

```r
ks_compare(base, comp, by = "id", find_patterns = TRUE)$pattern_summary
```

## Reports

```r
ks_report_html(cmp, "report.html")
ks_report_xlsx(cmp, "report.xlsx")
```

The HTML report is self-contained and does not require Quarto, Pandoc,
or internet access.

## SAS PROC COMPARE migration

```r
# PROC COMPARE BASE=a COMPARE=b ID id; OUT=outbase; OUTDIF; OUTNOEQUAL;
cmp <- ks_compare(a, b, by = "id")
as_outbase(cmp)
as_outdif(cmp)
as_outnoequal(cmp)
ks_sysinfo(cmp)
```

See `vignette("from-proc-compare")` for a full mapping.

## Pipeline gates

```r
ks_compare(target, qc, by = "USUBJID") |>
  ks_assert_clean(max_value_diffs = 0L)
```

## Learn more

- `vignette("getting-started")`
- `vignette("from-proc-compare")`
- `vignette("smart-features")`
- `vignette("reports")`
- `vignette("pipeline-gates")`
- Source repository: <https://github.com/al-garik/ksCompare>

## Optional integrations

Optional integrations include `ks_pointblank_step()` for pointblank
pipelines and `as_ks_comparison()` for teams moving from
`arsenal::comparedf()`.

## License

MIT. See [`LICENSE`](LICENSE).
