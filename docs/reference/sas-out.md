# SAS PROC COMPARE-style output datasets

These helpers reshape the long-format `value_diff` table inside a
`ks_comparison` into the four classic SAS PROC COMPARE output datasets:

## Usage

``` r
as_outbase(x)

as_outcomp(x)

as_outdif(x)

as_outnoequal(x)
```

## Arguments

- x:

  A `ks_comparison` object.

## Value

A tibble.

## Details

- `as_outbase()` — values from the base frame, only for matched rows
  that contain at least one differing column; one row per matched key,
  one column per compared variable.

- `as_outcomp()` — same shape as `as_outbase()` but values from the
  compare frame.

- `as_outdif()` — numeric difference (`base - comp`) for every matched
  row that contains at least one differing column; non-numeric cells are
  reported as `NA` with a `.note` column.

- `as_outnoequal()` — only the rows where at least one matched cell
  differs, in long format (one row per differing cell).

The shapes mirror SAS's `OUTBASE=`, `OUTCOMP=`, `OUTDIF=`, and
`OUTNOEQUAL=` datasets in spirit; we do not replicate the exact metadata
columns (`_TYPE_`, `_OBS_`, ...) but include `key_id` and the original
key columns so the rows can be related back to the source frames.

## Examples

``` r
a <- data.frame(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
b <- data.frame(id = 1:3, x = c(1, 2, 4), y = c("a", "B", "c"))
cmp <- ks_compare(a, b, by = "id")
#> ⚠ a vs b — 2 value diffs across 2 columns
as_outbase(cmp)
#> # A tibble: 2 × 4
#>      id key_id x     y    
#>   <int>  <int> <chr> <chr>
#> 1     2      2 NA    b    
#> 2     3      3 3     NA   
as_outcomp(cmp)
#> # A tibble: 2 × 4
#>      id key_id x     y    
#>   <int>  <int> <chr> <chr>
#> 1     2      2 NA    B    
#> 2     3      3 4     NA   
as_outdif(cmp)
#> # A tibble: 2 × 4
#>      id key_id     x     y
#>   <int>  <int> <dbl> <dbl>
#> 1     2      2    NA    NA
#> 2     3      3    -1    NA
as_outnoequal(cmp)
#> # A tibble: 2 × 12
#>      id key_id base_row comp_row column_base column_comp kind  base  comp   diff
#>   <int>  <int>    <int>    <int> <chr>       <chr>       <chr> <chr> <chr> <dbl>
#> 1     3      3        3        3 x           x           doub… 3     4        -1
#> 2     2      2        2        2 y           y           char… b     B        NA
#> # ℹ 2 more variables: na_flow <chr>, note <chr>
```
