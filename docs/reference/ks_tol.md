# Tolerance specification for numeric comparisons

Builds a tolerance object consumed by
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md).
Two numeric values `a` and `b` are considered equal when **any** of the
active rules pass:

## Usage

``` r
ks_tol(abs = 0, rel = 0, ulp = 0, per_column = NULL)
```

## Arguments

- abs:

  Non-negative absolute tolerance (default `0` — strict equality).

- rel:

  Non-negative relative tolerance, scaled by `max(abs(a), abs(b))`
  (default `0`).

- ulp:

  Non-negative integer ULP tolerance. Typical values are `4`–`16`;
  defaults to `0`.

- per_column:

  Optional named list of per-column `ks_tol()` overrides keyed by
  base-side column name.

## Value

A `ks_tol` S3 list.

## Details

- `abs(a - b) <= abs`

- `abs(a - b) <= rel * max(abs(a), abs(b))`

- `ulp_distance(a, b) <= ulp` — IEEE-754 units in the last place, useful
  for catching floating-point round-trip noise without false positives
  on genuine differences.

Per-column overrides may be supplied via `per_column`, a named list
whose names are **base-side** column names and whose values are
themselves the result of `ks_tol()`. Columns not listed fall back to the
top-level `abs` / `rel` / `ulp`.

## Examples

``` r
ks_tol(abs = 1e-9)
#> <ks_tol> abs = 1e-09, rel = 0, ulp = 0
ks_tol(rel = 1e-6, per_column = list(price = ks_tol(abs = 0.005)))
#> <ks_tol> abs = 0, rel = 1e-06, ulp = 0
#> Per-column overrides for: price
ks_tol(ulp = 4)
#> <ks_tol> abs = 0, rel = 0, ulp = 4
```
