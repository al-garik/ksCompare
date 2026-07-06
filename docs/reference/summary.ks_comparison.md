# Summary statistics for a ks_comparison

Computes a small list of headline counts that drive the printed summary,
the HTML report KPI cards, and CI gates.

## Usage

``` r
# S3 method for class 'ks_comparison'
summary(object, ...)
```

## Arguments

- object:

  A `ks_comparison` returned by
  [`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md).

- ...:

  Unused; present for S3 conformance.

## Value

A `ks_comparison_summary` list (also pretty-printed via
[`print()`](https://rdrr.io/r/base/print.html)) with components:

- `n_base_rows`, `n_comp_rows`: input row counts.

- `n_matched_rows`, `n_base_only_rows`, `n_comp_only_rows`: row counts
  after matching.

- `n_matched_columns`, `n_base_only_columns`, `n_comp_only_columns`:
  schema-side counts.

- `n_value_diffs`: number of differing cells in matched rows.

- `n_columns_with_diffs`: how many distinct columns hold those
  differences.

## See also

[`ks_glance()`](https://al-garik.github.io/ksCompare/reference/ks_glance.md)
for a tibble version,
[`ks_tidy()`](https://al-garik.github.io/ksCompare/reference/ks_tidy.md)
for the long cell-level table.

## Examples

``` r
cmp <- ks_compare(
  data.frame(id = 1:3, x = c(1, 2, 3)),
  data.frame(id = 1:3, x = c(1, 2, 4)),
  by = "id"
)
#> ⚠ data.frame(id = 1:3, x = c(1, 2, 3)) vs data.frame(id = 1:3, x = c(1, 2, 4))
#> — 1 value diff across 1 column
summary(cmp)
#> 
#> ── ksCompare summary ───────────────────────────────────────────────────────────
#> Rows (base / comp): 3 / 3
#> Matched / base-only / comp-only rows: 3 / 0 / 0
#> Matched / base-only / comp-only columns: 2 / 0 / 0
#> Cells with diffs: 1 (in 1 column)
```
