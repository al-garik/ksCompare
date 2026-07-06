# Unmatched rows from a comparison

Returns the rows that are present on only one side of a comparison
(`"base_only"` or `"comp_only"`), with their original data columns.
Mirrors the "Observations in only" tables produced by SAS
`PROC COMPARE`.

## Usage

``` r
ks_unmatched_rows(x, side = c("both", "base_only", "comp_only"))
```

## Arguments

- x:

  A `ks_comparison`.

- side:

  One of `"both"` (default), `"base_only"`, or `"comp_only"`.

## Value

A tibble with columns `side`, `key_id`, `key_label`, `base_row`,
`comp_row`, followed by the data columns from the side that holds each
row (`base_row` is `NA` for `comp_only` rows and vice versa, making it
trivial to look an observation up in the original `base` / `comp`
frame). Empty (zero-row) tibble when there are no unmatched rows.

## Details

The result is precomputed at
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
time and capped by the `max_unmatched_rows` argument (default `100`).
When the cap kicks in, the `truncated` attribute is set and the full
counts are recorded on `n_total`.

## See also

[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md),
[`ks_tidy()`](https://al-garik.github.io/ksCompare/reference/ks_tidy.md).

## Examples

``` r
a <- data.frame(id = 1:3, x = c(1, 2, 3))
b <- data.frame(id = 2:4, x = c(2, 3, 4))
cmp <- ks_compare(a, b, by = "id")
#> ⚠ a vs b — no value diffs; 2 unmatched row(s)
ks_unmatched_rows(cmp)
#> # A tibble: 2 × 6
#>   side      key_id key_label base_row comp_row     x
#>   <chr>      <int> <chr>        <int>    <int> <dbl>
#> 1 base_only      1 id = 1           1       NA     1
#> 2 comp_only      4 id = 4          NA        3     4
```
