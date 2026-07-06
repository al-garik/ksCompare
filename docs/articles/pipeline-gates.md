# Pipeline gates

A `ks_comparison` makes a natural CI gate: fail loudly on unexpected
diffs, succeed silently otherwise.

## ks_assert_clean

``` r

a <- data.frame(id = 1:3, x = c(1, 2, 3))
b <- data.frame(id = 1:3, x = c(1, 2, 4))

ks_compare(a, b, by = "id") |>
  ks_assert_clean()
#> ⚠ a vs b — 1 value diff across 1 column
#> Error in `ks_assert_clean()`:
#> ! Comparison did not meet expectations:
#> ✖ 1 value differences (max 0 allowed)
```

The error is classed (`ksCompare_assertion_failed`) so test suites can
catch it precisely:

``` r

testthat::expect_error(
  ks_compare(a, b, by = "id") |> ks_assert_clean(),
  class = "ksCompare_assertion_failed"
)
```

## Tolerating known diffs

``` r

ks_compare(a, b, by = "id") |>
  ks_assert_clean(max_value_diffs = 1L) |>
  invisible()
#> ⚠ a vs b — 1 value diff across 1 column
```

Other knobs: `max_schema_diffs`, `max_unmatched_rows`. The function
returns the comparison invisibly on success, so it composes cleanly
inside `|>` chains.

- `max_value_diffs` – budget for differing cells in matched rows.
- `max_schema_diffs` – budget counted as base-only columns + comp-only
  columns + matched columns whose `kind` differs.
- `max_unmatched_rows` – budget for
  `n_base_only_rows + n_comp_only_rows`.

## CI integration

In a CI script you usually want a summary written before the gate fails
so the failure is debuggable:

``` r

cmp <- ks_compare(target, qc_baseline, by = "USUBJID")
ks_report_html(cmp, "qc.html")    # always written

tryCatch(
  ks_assert_clean(cmp),
  ksCompare_assertion_failed = function(e) {
    message(conditionMessage(e))
    quit(status = 1)
  }
)
```

## With pointblank

``` r

library(pointblank)

cmp <- ks_compare(target, qc_baseline, by = "USUBJID")

agent <- create_agent(tbl = mtcars) |>
  ks_pointblank_step(cmp, max_value_diffs = 0L) |>
  interrogate()
```

[`ks_pointblank_step()`](https://al-garik.github.io/ksCompare/reference/ks_assert_clean.md)
wraps `pointblank::specially()`, so the gate participates in the agent’s
standard pass / warn / fail action levels.

## With arsenal

If your codebase already uses `arsenal::comparedf()`, the
[`as_ks_comparison()`](https://al-garik.github.io/ksCompare/reference/as_ks_comparison.md)
method re-runs the comparison via
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
on the same source frames and BY keys:

``` r

arsenal::comparedf(adsl_a, adsl_b, by = "USUBJID") |>
  as_ks_comparison() |>
  ks_assert_clean()
```
