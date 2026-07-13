# Render an HTML check report

Creates a self-contained HTML report for validation results produced by
[`ks_check_rules`](https://al-garik.github.io/ksCompare/reference/ks_check_rules.md).
The report includes overview KPI cards, a violation summary table, and a
row-level detail table.

## Usage

``` r
ks_check_report_html(
  data,
  path = NULL,
  title = "ksCompare check report",
  subtitle = NULL,
  col = check_msgs,
  max_rows = 500L,
  theme = c("default", "slate")
)
```

## Arguments

- data:

  A data frame containing a check-message column.

- path:

  Output file path. Three behaviours:

  - `NULL` (default): auto-generates a file name in the current working
    directory.

  - `NA`: returns the assembled
    [`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
    without writing a file.

  - character path: writes report to that path (`.html` appended if
    missing).

- title:

  Report title.

- subtitle:

  Optional subtitle.

- col:

  `<tidy-select>` Bare name of the check-message column. Default
  `check_msgs`.

- max_rows:

  Maximum number of rows shown in the detail table.

- theme:

  One of `"default"` or `"slate"`.

## Value

Invisibly returns `path` (or the assembled tag list when `path = NA`).

## Examples

``` r
if (FALSE) { # \dontrun{
checked <- ks_check_rules(adsl, rules, mode = "all")

# Write to disk
ks_check_report_html(checked, "check-report.html")

# In-memory tagList
tags <- ks_check_report_html(checked, path = NA)
} # }
```
