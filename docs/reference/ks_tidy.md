# Tidy a ks_comparison

Returns the long-format cell-level diff table. Equivalent to
`as_tibble(cmp)` and identical to `cmp$value_diff` when
`include_unmatched = FALSE`.

## Usage

``` r
ks_tidy(x, ...)

# S3 method for class 'ks_comparison'
ks_tidy(x, include_unmatched = FALSE, ...)
```

## Arguments

- x:

  A `ks_comparison`.

- ...:

  Unused.

- include_unmatched:

  Logical (default `FALSE`). When `TRUE`, append base-only / comp-only
  rows from `cmp$unmatched_rows`.

## Value

A tibble of value differences, optionally with appended unmatched-row
markers. Has columns `key_id`, `base_row`, `comp_row`, `column_base`,
`column_comp`, `kind`, `base`, `comp`, `diff`, `na_flow`, `note`.

## Details

When `include_unmatched = TRUE`, rows describing observations present on
only one side of the comparison are appended to the result with
`kind = "base_only"` / `"comp_only"` and `column_base` / `column_comp`
set to `NA`. One row is added per unmatched observation, capped at
`ks_compare(max_unmatched_rows = ...)`.

## Examples

``` r
cmp <- ks_compare(
  data.frame(id = 1:2, x = c(1, 2)),
  data.frame(id = 1:3, x = c(1, 3, 4)),
  by = "id"
)
#> ⚠ data.frame(id = 1:2, x = c(1, 2)) vs data.frame(id = 1:3, x = c(1, 3, 4)) — 1
#> value diff across 1 column
ks_tidy(cmp)
#> # A tibble: 1 × 11
#>   key_id base_row comp_row column_base column_comp kind   base  comp   diff
#>    <int>    <int>    <int> <chr>       <chr>       <chr>  <chr> <chr> <dbl>
#> 1      2        2        2 x           x           double 2     3        -1
#> # ℹ 2 more variables: na_flow <chr>, note <chr>
ks_tidy(cmp, include_unmatched = TRUE)
#> # A tibble: 2 × 11
#>   key_id base_row comp_row column_base column_comp kind      base  comp   diff
#>    <int>    <int>    <int> <chr>       <chr>       <chr>     <chr> <chr> <dbl>
#> 1      2        2        2 x           x           double    2     3        -1
#> 2      3       NA        3 NA          NA          comp_only NA    NA       NA
#> # ℹ 2 more variables: na_flow <chr>, note <chr>
```
