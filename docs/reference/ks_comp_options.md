# Comparison options

Builds an options object consumed by
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
that controls missing-value semantics, label / format comparison, string
normalisation, and time-zone handling.

## Usage

``` r
ks_comp_options(
  na_equal = getOption("ksCompare.na_equal", TRUE),
  sas_special_missing = getOption("ksCompare.sas_special_missing", TRUE),
  compare_labels = getOption("ksCompare.compare_labels", TRUE),
  compare_formats = getOption("ksCompare.compare_formats", TRUE),
  str_trim = getOption("ksCompare.str_trim", FALSE),
  str_case = getOption("ksCompare.str_case", "sensitive"),
  str_norm = getOption("ksCompare.str_norm", "none"),
  tz = getOption("ksCompare.tz", "preserve"),
  path = getOption("ksCompare.path", NULL)
)
```

## Arguments

- na_equal:

  Treat two `NA`s as equal? Default `TRUE`. With `FALSE`, any cell where
  one side is `NA` and the other is not is reported as a value diff;
  cells where *both* sides are `NA` are also reported (mirroring SAS
  `PROC COMPARE` behaviour with the `NOMISS` option turned off).

- sas_special_missing:

  Distinguish SAS special missings (`.A`-`.Z` and `._`) when comparing
  numeric columns imported via `haven`? Default `TRUE`. When `TRUE`,
  `.A` and `.B` compare as different even though both are `NA` in R;
  relies on the `tagged_na` tag attached by
  [`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html).

- compare_labels:

  Compare `attr(x, "label")` between matched columns and surface
  differences in `cmp$schema_diff`? Default `TRUE`. Set to `FALSE` to
  suppress label-only diffs (e.g. when metadata drift is expected).

- compare_formats:

  Compare `attr(x, "format.sas")` between matched columns? Default
  `TRUE`. Comparison is trailing-dot- and case-tolerant, so `DATE9.`,
  `DATE9`, and `date9.` all compare as equal.

- str_trim:

  Trim leading/trailing whitespace with
  [`stringi::stri_trim_both()`](https://rdrr.io/pkg/stringi/man/stri_trim.html)
  before comparing strings? Default `FALSE`. Useful when one source has
  been padded by a fixed-width exporter.

- str_case:

  One of `"sensitive"` (default) or `"fold"`. When `"fold"`, strings are
  compared after Unicode case folding
  ([`stringi::stri_trans_tolower()`](https://rdrr.io/pkg/stringi/man/stri_trans_casemap.html)).

- str_norm:

  One of `"none"` (default) or `"NFC"`. When `"NFC"`, strings are
  Unicode-normalised before comparing so that visually-identical
  sequences with different code-point compositions match.

- tz:

  One of `"preserve"` (default), `"UTC"`, or `"strip"`. Controls how
  `POSIXct` columns are reconciled when the two sides carry different
  `tzone` attributes:

  - `"preserve"`: values are compared as-is (a tz mismatch surfaces as a
    diff if it changes the underlying instant).

  - `"UTC"`: both sides are converted to UTC before compare.

  - `"strip"`: `tzone` is dropped and values compared as POSIXct.

- path:

  Optional output folder used by downstream writers
  ([`ks_report_html()`](https://al-garik.github.io/ksCompare/reference/ks_report_html.md),
  [`ks_report_xlsx()`](https://al-garik.github.io/ksCompare/reference/ks_report_xlsx.md))
  when their own `path =` argument is left blank. Default `NULL` means
  "use the current working directory". The folder is created on first
  use if it does not already exist. Setting it once on
  `ks_comp_options()` and passing the result through
  [`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
  keeps every artefact from a single comparison run in the same place.

## Value

A `ks_comp_options` S3 list.

## Global defaults via [`options()`](https://rdrr.io/r/base/options.html)

Each argument falls back to a global R option in the `ksCompare.*`
namespace, then to the package default. Precedence is: explicit argument
\> `getOption("ksCompare.<arg>")` \> built-in default. That makes it
possible to set project-wide defaults once and then call
[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
without repeating yourself:

    # In .Rprofile or at the top of a script
    options(ksCompare.path     = "out/qc",
            ksCompare.str_case = "fold")

    ks_compare(base, comp) |> ks_report_html()   # honours the globals

[`ks_set_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_set_comp_options.md)
is a small wrapper around
[`base::options()`](https://rdrr.io/r/base/options.html) that takes the
same argument names without the `ksCompare.` prefix.

Recognised options: `ksCompare.na_equal`,
`ksCompare.sas_special_missing`, `ksCompare.compare_labels`,
`ksCompare.compare_formats`, `ksCompare.str_trim`, `ksCompare.str_case`,
`ksCompare.str_norm`, `ksCompare.tz`, `ksCompare.path`.

## See also

[`ks_set_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_set_comp_options.md)
for setting the `ksCompare.*` globals from R code.

## Examples

``` r
# Defaults: strict NAs, label & format comparison on
ks_comp_options()
#> <ks_comp_options>
#> • na_equal: TRUE
#> • sas_special_missing: TRUE
#> • compare_labels: TRUE
#> • compare_formats: TRUE
#> • str_trim: FALSE
#> • str_case: "sensitive"
#> • str_norm: "none"
#> • tz: "preserve"
#> • path:

# Loose strings: trim padding and ignore case
ks_comp_options(str_trim = TRUE, str_case = "fold")
#> <ks_comp_options>
#> • na_equal: TRUE
#> • sas_special_missing: TRUE
#> • compare_labels: TRUE
#> • compare_formats: TRUE
#> • str_trim: TRUE
#> • str_case: "fold"
#> • str_norm: "none"
#> • tz: "preserve"
#> • path:

# Suppress label/format drift, but keep cell-level strictness
ks_comp_options(compare_labels = FALSE, compare_formats = FALSE)
#> <ks_comp_options>
#> • na_equal: TRUE
#> • sas_special_missing: TRUE
#> • compare_labels: FALSE
#> • compare_formats: FALSE
#> • str_trim: FALSE
#> • str_case: "sensitive"
#> • str_norm: "none"
#> • tz: "preserve"
#> • path:

# Treat any NA as a difference (PROC COMPARE without NOMISS)
ks_comp_options(na_equal = FALSE)
#> <ks_comp_options>
#> • na_equal: FALSE
#> • sas_special_missing: TRUE
#> • compare_labels: TRUE
#> • compare_formats: TRUE
#> • str_trim: FALSE
#> • str_case: "sensitive"
#> • str_norm: "none"
#> • tz: "preserve"
#> • path:

# Pin every report from this run to a dedicated folder
ks_comp_options(path = tempfile("ksCompare_"))
#> <ks_comp_options>
#> • na_equal: TRUE
#> • sas_special_missing: TRUE
#> • compare_labels: TRUE
#> • compare_formats: TRUE
#> • str_trim: FALSE
#> • str_case: "sensitive"
#> • str_norm: "none"
#> • tz: "preserve"
#> • path:
#>   "/var/folders/rn/3s0h46m118j426j_fmjr1z8m0000gn/T//RtmpsRNjFP/ksCompare_5ea57f56c0e9"

# Pick up a project-wide global path
withr::with_options(
  list(ksCompare.path = tempfile("ksCompare_")),
  ks_comp_options()$path
)
#> [1] "/var/folders/rn/3s0h46m118j426j_fmjr1z8m0000gn/T//RtmpsRNjFP/ksCompare_5ea51893424f"
```
