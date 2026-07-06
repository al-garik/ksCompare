# Render an Excel workbook report

Writes a multi-sheet workbook summarising a `ks_comparison`. Sheets:
`Summary`, `Schema`, `KeyDiff`, `Values`, `Patterns`, `OUT_BASE`,
`OUT_COMP`, `OUT_DIF`, `OUT_NOEQUAL`, `Manifest`. Cells with value
differences are highlighted on the wide `OUT_DIF` sheet.

## Usage

``` r
ks_report_xlsx(x, path = NULL, highlight = TRUE, threshold = 0)
```

## Arguments

- x:

  A `ks_comparison` object.

- path:

  File path for the workbook. When `NULL` (default), the filename is
  auto-generated from the comparison's `base_name` / `comp_name` and
  placed in `options$path` (if set on
  [`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md))
  or the current working directory, e.g.
  `ksCompare_adsl_vs_adsl_qc.xlsx`. A `.xlsx` extension is appended if
  missing. A bare filename is resolved against `options$path` when that
  has been set.

- highlight:

  If `TRUE` (default), apply conditional formatting to highlight numeric
  `OUT_DIF` cells whose magnitude is above `threshold`.

- threshold:

  Numeric threshold for highlighting (default `0`, so any non-zero diff
  is highlighted).

## Value

Invisibly returns `path`.

## Examples

``` r
if (FALSE) { # \dontrun{
  cmp <- ks_compare(iris, iris, by = "Species")
  ks_report_xlsx(cmp, "report.xlsx")

  # Auto-named output in a pinned folder
  cmp2 <- ks_compare(
    iris, iris, by = "Species",
    options = ks_comp_options(path = tempfile("ksCompare_"))
  )
  ks_report_xlsx(cmp2)
} # }
```
