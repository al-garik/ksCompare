# Compare validation check state between two runs

Compares `current` results against a `previous` snapshot and adds a
`status` column indicating whether each row is new, changed, or
unchanged relative to the previous run.

## Usage

``` r
ks_compare_check_state(current, previous = NULL, key_cols, compare_cols)
```

## Arguments

- current:

  A data frame from the most recent
  [`ks_check_rules`](https://al-garik.github.io/ksCompare/reference/ks_check_rules.md)
  run.

- previous:

  A data frame from a previous run, or `NULL` (default). If `NULL` or
  empty, all rows in `current` receive `status = "new"`.

- key_cols:

  Character vector of column names used to match rows across runs. Must
  be provided explicitly.

- compare_cols:

  Character vector of column names whose values are used to detect
  changes. Must be provided explicitly.

## Value

`current` with an additional character column `status`:

- `"new"`:

  Row key not found in `previous`.

- `"changed"`:

  Row key found but comparison signature differs.

- `""`:

  Row is unchanged.

## Examples

``` r
if (FALSE) { # \dontrun{
result_new <- ks_check_rules(data_new, rules)
result_old <- readRDS("previous_check.rds")
ks_compare_check_state(result_new, result_old)
} # }
```
