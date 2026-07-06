# Diff causes summary

Aggregates the `note` column of `cmp$value_diff` into a tidy taxonomy:
one row per distinct cause, with counts and the columns it affects.
Helps answer "what kinds of discrepancies do we have?" before drilling
into individual cells.

## Usage

``` r
ks_cause_summary(x)
```

## Arguments

- x:

  A `ks_comparison`.

## Value

A tibble with columns:

- `cause`: short label (e.g. `"letter case differs"`).

- `n_cells`: number of cells contributing this cue.

- `n_columns`: number of distinct base columns affected.

- `columns`: comma-separated sample of the affected columns (capped at 5
  names). Sorted by `n_cells` descending. Zero-row tibble when there are
  no value differences.

## Details

Compound notes (multiple cues joined by `"; "`) are split into their
elementary cues, so a single cell can contribute to several causes.
Cells with `note = NA` (a "plain" value change with no recognised cue)
are bucketed as `"plain value change"`.

## See also

[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md),
[`ks_row_diff_summary()`](https://al-garik.github.io/ksCompare/reference/ks_row_diff_summary.md).

## Examples

``` r
a <- data.frame(id = 1:3, x = c("a", "b ", "c"), y = c(1, 2, 3))
b <- data.frame(id = 1:3, x = c("A", "b",  "c"), y = c(1, 2, 4))
cmp <- ks_compare(a, b, by = "id")
#> ✖ a vs b — 3 value diffs across 2 columns
ks_cause_summary(cmp)
#> # A tibble: 3 × 4
#>   cause                      n_cells n_columns columns
#>   <chr>                        <int>     <int> <chr>  
#> 1 letter case differs              1         1 x      
#> 2 plain value change               1         1 y      
#> 3 whitespace padding on base       1         1 x      
```
