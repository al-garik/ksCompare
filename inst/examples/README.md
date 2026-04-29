# ksCompare examples

This folder contains runnable example scripts demonstrating `ksCompare`
on a range of realistic scenarios. They use only exported functions
and base R + Suggests packages already declared in `DESCRIPTION`.

After installing `ksCompare`, list the available examples with:

```r
list.files(system.file("examples", package = "ksCompare"))
```

and run one with:

```r
file.edit(system.file("examples", "01-basic-comparison.R", package = "ksCompare"))
# or
source(system.file("examples", "01-basic-comparison.R", package = "ksCompare"))
```

## Index

| Script                              | Topic                                                                |
|-------------------------------------|----------------------------------------------------------------------|
| `01-basic-comparison.R`             | Hello-world: two small frames, `print()`, `summary()`, `as_tibble()`.|
| `02-data-types.R`                   | All major R types: numeric, integer, character, factor, logical, Date, POSIXct, `haven_labelled`. |
| `03-tolerances-and-options.R`       | `ks_tol()` (abs / rel / ULP, per-column) + `ks_options()` (NA, labels, formats, strings, tz). |
| `04-grouping-and-keys.R`            | Explicit `by`, renamed key, `by = "auto"`, `dup_keys` strategies, `group_by_key` HTML. |
| `05-large-and-complex.R`            | Stress test: ~50k rows, multi-column key, many columns, patterns.    |
| `06-pointblank-recipes.R`           | Six recipes for combining `ks_assert_clean()` / `ks_pointblank_step()` with pointblank. |
