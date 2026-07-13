# Save a data frame to an XLSX workbook split by a grouping column

Writes `data` to an XLSX file, creating one worksheet per unique value
of `split_col`. Rows can be colour-highlighted based on the content of a
message column (e.g. the output of
[`ks_check_rules`](https://al-garik.github.io/ksCompare/reference/ks_check_rules.md)
collapsed with
[`ks_collapse_check_msgs`](https://al-garik.github.io/ksCompare/reference/ks_collapse_check_msgs.md))
and optionally a status column. Column labels (stored as `"label"`
attributes) are written as a grey italic row above the data headers when
present.

## Usage

``` r
ks_save2xlsx_by(
  data,
  file,
  colors,
  split_col = type,
  msg_col = check_msgs,
  na_to_empty = FALSE,
  status_col = NULL,
  status_colors = NULL
)
```

## Arguments

- data:

  A data frame.

- file:

  Character. Path to the output `.xlsx` file.

- colors:

  Named character vector mapping message substrings to hex colour codes
  (e.g. `c("Error" = "#FFCCCC", "OK" = "#CCFFCC")`). Values with or
  without leading `"#"` are accepted.

- split_col:

  `<tidy-select>` Bare name of the column used to split data into
  worksheets. Default `type`.

- msg_col:

  `<tidy-select>` Bare name of the column that contains check messages
  used for row colouring. Default `check_msgs`.

- na_to_empty:

  Logical. If `TRUE`, replace `NA` and `"#N/A"` in character columns
  with empty strings. Default `FALSE`.

- status_col:

  `<tidy-select>` Optional bare name of a status column (e.g. from
  [`ks_compare_check_state`](https://al-garik.github.io/ksCompare/reference/ks_compare_check_state.md))
  used for a secondary layer of row colouring that overrides `colors`.
  Pass `NULL` to disable. Default `NULL`.

- status_colors:

  Named character vector mapping status values to hex colour codes. Used
  only when `status_col` is not `NULL`. Values with or without leading
  `"#"` are accepted.

## Value

Invisibly returns `NULL`. The file is written as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
ks_save2xlsx_by(
  results,
  file = "output/checks.xlsx",
  colors = c("Error" = "#FFCCCC", "Warning" = "#FFFF99"),
  split_col = type,
  msg_col = check_msgs
)
} # }
```
