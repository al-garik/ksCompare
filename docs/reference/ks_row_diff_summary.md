# Per-row diff summary

Tallies the number of cell differences per matched observation, sorted
descending. Useful for spotting rows where most/all columns disagree
(often a sign of a wrong row match, a duplicated key resolved
differently, or a systemic shift on a single record).

## Usage

``` r
ks_row_diff_summary(x, n = NULL)
```

## Arguments

- x:

  A `ks_comparison`.

- n:

  Optional cap on the number of rows returned (default `NULL` = all).
  When set, rows beyond `n` are dropped.

## Value

A tibble with one row per matched observation that has at least one cell
diff:

- `key_id`, `base_row`, `comp_row`, `key_label`

- `n_diffs`: count of differing cells on this observation.

- `columns`: comma-separated sample of the affected columns (capped at 8
  names). Sorted by `n_diffs` descending. Zero-row tibble when there are
  no value differences.

## Details

Only matched rows are reported; unmatched rows are available via
[`ks_unmatched_rows()`](https://al-garik.github.io/ksCompare/reference/ks_unmatched_rows.md).

## See also

[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md),
[`ks_cause_summary()`](https://al-garik.github.io/ksCompare/reference/ks_cause_summary.md).

## Examples

``` r
a <- data.frame(id = 1:3, x = 1:3, y = 1:3)
b <- data.frame(id = 1:3, x = c(1, 9, 9), y = c(1, 9, 3))
cmp <- ks_compare(a, b, by = "id")
#> ✖ a vs b — 3 value diffs across 2 columns
ks_row_diff_summary(cmp)
#> # A tibble: 2 × 6
#>   key_id base_row comp_row key_label n_diffs columns
#>    <int>    <int>    <int> <chr>       <int> <chr>  
#> 1      2        2        2 id = 2          2 x, y   
#> 2      3        3        3 id = 3          1 x      
```
