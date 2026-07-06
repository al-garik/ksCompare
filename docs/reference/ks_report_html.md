# Render a self-contained HTML report

Produces a polished, single-file HTML report summarising a
`ks_comparison`. The output uses `htmltools` for layout and `reactable`
for interactive tables. No internet access, no Quarto, and no Pandoc are
required.

## Usage

``` r
ks_report_html(
  x,
  path = NULL,
  title = "ksCompare report",
  subtitle = NULL,
  max_rows = 100L,
  theme = c("default", "slate"),
  group_by_key = FALSE,
  max_groups = 200L
)
```

## Arguments

- x:

  A `ks_comparison` object.

- path:

  File path for the report. Three behaviours:

  - missing / `NULL` (default): a filename is auto-generated from the
    comparison's `base_name` (and, when distinct, `comp_name`) written
    into `options$path` (if set on
    [`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md))
    or the current working directory, e.g.
    `ksCompare_adsl_vs_adsl_qc.html`.

  - `NA`: nothing is written; the assembled
    [`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
    is returned for embedding in another document.

  - a character path: written to that path. A `.html` extension is
    appended if missing. A bare filename (no directory component) is
    resolved relative to `options$path` when that has been set on
    [`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md),
    otherwise relative to the working directory.

- title:

  Title shown at the top of the report.

- subtitle:

  Optional subtitle (e.g. study identifier or run id).

- max_rows:

  Per-table row cap (default `100`). When the value-diff table exceeds
  this cap, the report shows a *smart stratified sample* covering each
  column and each distinct diff-cause (`note`), prioritising the largest
  numeric magnitudes (at least one example per column and per cause is
  always retained). A notice indicates the sample size and recommends
  `as_tibble(cmp)` for the full set. `as_tibble(x)` is never truncated.

- theme:

  One of `"default"` (steel blue) or `"slate"` (neutral dark headers on
  light background).

- group_by_key:

  Logical (default `FALSE`). When `TRUE` and a `by =` key was used in
  [`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md),
  the value-diff section is rendered as one collapsed `<details>` block
  per key value, sorted by number of diffs (most-affected first). For
  `dup_keys = "keep_all"` / `"all_pairs"`, each block also shows a
  *Pair* column so you can tell pairs apart. Silently falls back to the
  flat table when no `by =` was supplied (position match).

- max_groups:

  Maximum number of key-value blocks to render when
  `group_by_key = TRUE` (default `200`). Excess groups are summarised in
  a closing notice.

## Value

Invisibly returns `path` (or the assembled tag list when `path = NULL`).

## Details

Layout: a left-hand sticky table of contents, a header with dataset
names, a status pill, KPI cards, then sections for schema, row diff,
value diff, patterns, and the run manifest. Tables use friendly column
labels, column groups, and `tick`/`cross` markers instead of raw
`TRUE`/`FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
  cmp <- ks_compare(iris, iris, by = "Species")

  # Explicit path
  ks_report_html(cmp, "report.html", title = "ADSL QC")

  # Auto-generated filename in working directory
  ks_report_html(cmp)

  # In-memory tagList for embedding
  tags <- ks_report_html(cmp, path = NA)
} # }
```
