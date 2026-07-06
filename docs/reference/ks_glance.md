# Glance at a ks_comparison

Returns a one-row tibble with the same headline counts as
[`summary.ks_comparison()`](https://al-garik.github.io/ksCompare/reference/summary.ks_comparison.md).
Convenient for binding many comparisons together in a QC loop.

## Usage

``` r
ks_glance(x, ...)
```

## Arguments

- x:

  A `ks_comparison`.

- ...:

  Unused.

## Value

A one-row tibble with columns `n_base_rows`, `n_comp_rows`,
`n_matched_rows`, `n_base_only_rows`, `n_comp_only_rows`,
`n_matched_columns`, `n_value_diffs`, `n_columns_with_diffs`.

## Examples

``` r
cmp <- ks_compare(
  data.frame(id = 1:2, x = c(1, 2)),
  data.frame(id = 1:2, x = c(1, 3)),
  by = "id"
)
#> ⚠ data.frame(id = 1:2, x = c(1, 2)) vs data.frame(id = 1:2, x = c(1, 3)) — 1
#> value diff across 1 column
ks_glance(cmp)
#> # A tibble: 1 × 8
#>   n_base_rows n_comp_rows n_matched_rows n_base_only_rows n_comp_only_rows
#>         <int>       <int>          <int>            <int>            <int>
#> 1           2           2              2                0                0
#> # ℹ 3 more variables: n_matched_columns <int>, n_value_diffs <int>,
#> #   n_columns_with_diffs <int>
```
