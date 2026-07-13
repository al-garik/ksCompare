# Summarise check results from `ks_check_rules()`

Computes row-level validation totals and a per-message breakdown from a
data frame containing a `check_msgs` column.

## Usage

``` r
ks_check_summary(data, col = check_msgs)
```

## Arguments

- data:

  A data frame containing `check_msgs`.

- col:

  `<tidy-select>` Bare name of the check-message column. Default
  `check_msgs`.

## Value

A list with two tibbles:

- `overview`:

  Single-row summary with total, passing, failing, and pass-rate
  columns.

- `violations`:

  Per-message counts and percentages, sorted by count descending.

## Examples

``` r
if (FALSE) { # \dontrun{
summary <- ks_check_summary(results)
summary$overview
summary$violations
} # }
```
