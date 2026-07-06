# Suggest additional key columns when duplicates exist

Scans matched columns to find candidates whose combination with the
current `by` key would make the join key unique on at least one side
(and ideally both). Returns columns ranked by how much they reduce
duplicate-key cardinality. Useful when
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
used `dup_keys = "keep_all"` and the result looks suspicious.

## Usage

``` r
ks_suggest_key(x, base = NULL, comp = NULL, top_n = 10L)
```

## Arguments

- x:

  A `ks_comparison`.

- base, comp:

  Optional: the original input data frames used to build `x`. Required
  to inspect candidate column uniqueness.

- top_n:

  Maximum number of candidate columns to return.

## Value

A tibble with columns `column`, `pct_unique_base`, `pct_unique_comp`,
`would_make_unique`. Zero rows when no key is in use, no duplicates
exist, or no improvement is possible.

## Details

Because
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
does not snapshot the input data, the original `base` and `comp` frames
must be passed in to inspect candidate columns. When `base` / `comp` are
not supplied (or do not contain the original columns) an empty tibble is
returned.

## See also

[`ks_match_health()`](https://al-garik.github.io/ksCompare/reference/ks_match_health.md),
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md).
