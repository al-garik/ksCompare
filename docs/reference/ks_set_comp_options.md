# Set ksCompare global options

Thin wrapper around
[`base::options()`](https://rdrr.io/r/base/options.html) for the
`ksCompare.*` namespace. Accepts the same argument names as
[`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md)
(without the `ksCompare.` prefix) and forwards them into
[`base::options()`](https://rdrr.io/r/base/options.html), returning the
previous values invisibly so the call can be unwound with
[`base::options()`](https://rdrr.io/r/base/options.html) or
[`withr::with_options()`](https://withr.r-lib.org/reference/with_options.html).

## Usage

``` r
ks_set_comp_options(...)
```

## Arguments

- ...:

  Named arguments. Recognised names match the formals of
  [`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md):
  `na_equal`, `sas_special_missing`, `compare_labels`,
  `compare_formats`, `str_trim`, `str_case`, `str_norm`, `tz`, `path`. A
  single unnamed `ks_comp_options` object may also be passed in, in
  which case all of its fields are pushed.

## Value

Invisibly, a named list of the previous values (suitable for
`do.call(options, prev)` to restore).

## Details

Globals set this way become the new defaults of
[`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md)
(and therefore of
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
when no `options =` argument is supplied).

## Examples

``` r
old <- ks_set_comp_options(path = tempfile("ksCompare_"), str_case = "fold")
ks_comp_options()$path
#> [1] "/tmp/RtmpLltSiF/ksCompare_430392a7725f3"
do.call(options, old)  # restore
```
