# ksCompare

> Smart data-frame comparison in the spirit of SAS `PROC COMPARE`.

[![R-CMD-check](https://github.com/ksCompare/ksCompare/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ksCompare/ksCompare/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

`ksCompare` is a tidyverse-native R package for comparing two data frames
with the rigor expected of clinical / pharma QC workflows, plus a layer
of smart features beyond `PROC COMPARE`:

- Per-column tolerances: absolute, relative, **and ULP**.
- Intelligent type reconciliation, including `haven_labelled` and SAS
  special missings (`.A`–`.Z`, `._`).
- Automatic key inference (`by = "auto"`) and fuzzy column-rename
  suggestions.
- Duplicate-key strategies (`first`, `last`, `all_pairs`).
- Diff-pattern detection (constant offset, sign flip, trim-only,
  case-only, …).
- Rich console output (`cli`), HTML (`reactable`), Excel (`openxlsx2`),
  and Quarto reports.
- Pipeline gates (`ks_assert_clean()`, `ks_pointblank_step()`).
- SAS-parity surface: `as_outbase()` / `as_outcomp()` / `as_outdif()` /
  `as_outnoequal()` and `ks_sysinfo()`.

## Installation

```r
# install.packages("pak")
pak::pak("ksCompare/ksCompare")
```

## Quick start

```r
library(ksCompare)

base <- data.frame(id = 1:3, x = c(1.0, 2.0, 3.0))
comp <- data.frame(id = 1:3, x = c(1.0, 2.0, 4.0))

cmp <- ks_compare(base, comp, by = "id")
cmp
summary(cmp)
tibble::as_tibble(cmp)
ks_report_html(cmp, "report.html")
```

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

## Articles

- `vignette("getting-started")`
- `vignette("from-proc-compare")`
- `vignette("smart-features")`
- `vignette("reports")`
- `vignette("pipeline-gates")`

## License

MIT © ksCompare authors. See [`LICENSE`](LICENSE).
