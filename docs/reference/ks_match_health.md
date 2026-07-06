# Match-quality health check

Computes a small set of metrics that flag *suspicious* matching: high
diff density, many fully-different rows, duplicate keys resolved
positionally, and similar patterns that often indicate the wrong key was
used.

## Usage

``` r
ks_match_health(x)
```

## Arguments

- x:

  A `ks_comparison`.

## Value

A list with components:

- `diff_density`: `n_value_diffs / (n_matched_rows * n_matched_columns)`
  (0-1).

- `n_fully_diff_rows`: matched rows where every matched column differs
  (or, more permissively, \>= 80% of columns differ).

- `pct_fully_diff_rows`: as proportion of matched rows.

- `n_value_to_na`, `n_na_to_value`: structural NA transitions.

- `dup_keys`: `TRUE` if either side had duplicate key values.

- `dup_positional`: `TRUE` if duplicates were paired positionally
  (`keep_all`) — the most error-prone path.

- `row_count_delta`: `n_comp_rows - n_base_rows` (signed).

- `flags`: character vector of human-readable warnings (possibly empty).

- `severity`: `"ok"`, `"info"`, `"warn"`, or `"critical"`.

## See also

[`ks_recommendations()`](https://al-garik.github.io/ksCompare/reference/ks_recommendations.md),
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md).

## Examples

``` r
a <- data.frame(id = c(1, 1, 2), x = c(1, 2, 3))
b <- data.frame(id = c(1, 1, 2), x = c(9, 9, 3))
cmp <- ks_compare(a, b, by = "id", dup_keys = "keep_all")
#> ✖ a vs b — 2 value diffs across 1 column
ks_match_health(cmp)
#> $diff_density
#> [1] 0.3333333
#> 
#> $n_fully_diff_rows
#> [1] 0
#> 
#> $pct_fully_diff_rows
#> [1] 0
#> 
#> $n_value_to_na
#> [1] 0
#> 
#> $n_na_to_value
#> [1] 0
#> 
#> $dup_keys
#> [1] TRUE
#> 
#> $dup_positional
#> [1] TRUE
#> 
#> $position_match
#> [1] FALSE
#> 
#> $row_count_delta
#> [1] 0
#> 
#> $flags
#> [1] "Duplicate keys were paired positionally and diff density is high — the key is likely incomplete. Try adding columns to `by`."
#> 
#> $severity
#> [1] "critical"
#> 
```
