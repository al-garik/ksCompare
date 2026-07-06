# Convert an `arsenal::comparedf` result into a `ks_comparison`

Lossy interop: takes the two source frames stored on a
`arsenal::comparedf` object and re-runs
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
using the same BY keys. Useful for teams migrating from
`arsenal::comparedf()` to ksCompare without rewriting their inputs.

## Usage

``` r
as_ks_comparison(x, ...)
```

## Arguments

- x:

  An `arsenal::comparedf` object.

- ...:

  Additional arguments forwarded to
  [`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
  (e.g. `tolerance`, `options`).

## Value

A `ks_comparison` object.

## Details

Note that ksCompare's diff is computed from scratch — it does not
attempt to translate `comparedf`'s internal diff tables row-by-row.
