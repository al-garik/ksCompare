# Use a comparison as a pipeline gate

Treats a `ks_comparison` as a pass/fail check. By default a "passing"
comparison has zero schema, key, and value differences; callers can
relax those expectations to allow a budget of known acceptable diffs.

## Usage

``` r
ks_assert_clean(
  x,
  max_value_diffs = 0L,
  max_schema_diffs = 0L,
  max_unmatched_rows = 0L
)

ks_pointblank_step(
  agent,
  comparison,
  max_value_diffs = 0L,
  max_schema_diffs = 0L,
  max_unmatched_rows = 0L,
  label = "ksCompare clean"
)
```

## Arguments

- x:

  A `ks_comparison` object returned by
  [`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md).

- max_value_diffs:

  Integer. Maximum number of differing cells allowed in matched rows.
  Default `0L` (strict).

- max_schema_diffs:

  Integer. Maximum schema differences allowed, counted as the sum of
  base-only columns, comp-only columns, and matched columns whose `kind`
  differs. Default `0L` (strict).

- max_unmatched_rows:

  Integer. Maximum number of unmatched rows allowed
  (`n_base_only_rows + n_comp_only_rows`). Default `0L` (strict).

- agent:

  A `ptblank_agent` (typically piped in from
  [`pointblank::create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.html))
  or a data frame / tibble. The comparison gate is added as a step on
  this object and `agent` is returned, so it composes with the rest of a
  pointblank pipeline.

- comparison:

  A `ks_comparison` produced by
  [`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
  that the step will gate on.

- label:

  Optional label for the pointblank step.

## Value

`ks_assert_clean()` returns `x` invisibly when expectations are met. On
failure, raises a condition of class `ksCompare_assertion_failed`
(catchable in CI with
`tryCatch(..., ksCompare_assertion_failed = function(e) ...)`).
`ks_pointblank_step()` returns a `pointblank` step.

## Details

Two flavours are provided:

- `ks_assert_clean()` throws a classed error
  (`ksCompare_assertion_failed`) and halts a pipeline when expectations
  are unmet. Returns the comparison invisibly on success, so it can be
  dropped into a `|>` chain.

- `ks_pointblank_step()` returns a `pointblank` step that wraps
  `ks_assert_clean()` for use inside
  [`pointblank::create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.html)
  /
  [`pointblank::action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.html)
  flows.

## Examples

``` r
a <- data.frame(id = 1:3, x = 1:3)
ks_compare(a, a, by = "id") |> ks_assert_clean()
#> ✔ a — identical

# Allow a small budget of known diffs
if (FALSE) { # \dontrun{
  ks_compare(a, b, by = "id") |>
    ks_assert_clean(max_value_diffs = 5)
} # }
```
