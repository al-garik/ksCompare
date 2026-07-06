# Compare two data frames

`ks_compare()` is the single entry point for the package. It compares
`base` against `comp` and returns a `ks_comparison` S3 object containing
schema, key, row, and value differences plus a small QC manifest.

## Usage

``` r
ks_compare(
  base,
  comp,
  by = NULL,
  mapping = NULL,
  tolerance = ks_tol(),
  coerce = c("safe", "strict", "lossy"),
  dup_keys = c("first", "last", "keep_all", "all_pairs", "error"),
  options = ks_comp_options(),
  base_name = NULL,
  comp_name = NULL,
  find_patterns = FALSE,
  n_first_last = 5L,
  max_unmatched_rows = 100L,
  loglevel = c("verdict", "verbose")
)
```

## Arguments

- base, comp:

  Either an in-memory data frame (or anything coercible via
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html):
  tibbles, data.tables, Arrow tables) or a single character file path.
  When given a path, the file is read automatically based on its
  extension:

  - `.sas7bdat`, `.xpt` (via `haven`)

  - `.parquet`, `.feather` / `.arrow` (via `arrow`)

  - `.rds`, `.rdata` / `.rda`

  - `.csv`, `.tsv` / `.txt`

  `base` is treated as the reference; `comp` is the candidate. When both
  inputs are file paths that share the same basename (e.g.
  `prod/adsl.sas7bdat` vs `qc/adsl.sas7bdat`), display names are
  automatically disambiguated with the parent directory.

- by:

  Optional column name(s) to use as the matching key.

  - `NULL` (default): rows are matched by row position. A warning banner
    is shown in the HTML report so this is never silent.

  - `character`: same column name on both sides. Use a named vector when
    the key is renamed: `c(USUBJID = "SUBJID")`.

  - `"auto"`: search the smallest combination of shared columns that is
    unique on **both** sides (capped at 4 columns). Falls back to
    position match with a warning if nothing qualifies.

- mapping:

  Optional explicit column mapping for renamed columns. Either a fully
  named character vector (`c(base_col = "comp_col", ...)`) or a 2-column
  data frame with columns `base` and `comp`. Mapping pairs always win
  over exact-name matching.

- tolerance:

  A
  [`ks_tol()`](https://al-garik.github.io/ksCompare/reference/ks_tol.md)
  specification. Defaults to strict equality. Per-column overrides
  supported via `ks_tol(per_column = list(col = ks_tol(...)))`.

- coerce:

  Type-coercion strictness. One of:

  - `"safe"` (default):
    [`vctrs::vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.html)
    plus integer↔double, factor↔character, `haven_labelled` unwrapping.

  - `"strict"`: only common types
    [`vctrs::vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.html)
    agrees on. Type mismatches surface as `kind = "type_mismatch"` rows.

  - `"lossy"`: additionally allows numeric↔character,
    date/datetime↔character (ISO-8601), factor↔integer (level codes).

- dup_keys:

  Strategy for duplicate keys (per-side independently). One of:

  - `"first"` (default): keep the first occurrence per side. Drops rows;
    emits a `ksCompare_dup_keys_resolved` info message.

  - `"last"`: keep the last occurrence per side. Same message.

  - `"keep_all"`: pair duplicate rows positionally within each key group
    (1↔1, 2↔2, …). Leftover rows on the longer side become base- or
    compare-only.

  - `"all_pairs"`: cartesian-pair every base row with every comp row
    sharing a key value. Emits a `ksCompare_all_pairs_cardinality`
    warning when group sizes disagree.

  - `"error"`: raise on any duplicate key.

- options:

  A
  [`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md)
  specification controlling NA / SAS special-missing semantics, label /
  format comparison, string normalisation, and the default output folder
  for reports.

- base_name, comp_name:

  Optional display names for the two frames (used in
  [`print()`](https://rdrr.io/r/base/print.html) and reports). Default
  to the names of the supplied expressions.

- find_patterns:

  Logical (default `FALSE`). When `TRUE`, run the pattern-detection pass
  (constant offsets, sign flips, unit rescales, epoch shifts, etc.) and
  populate `cmp$pattern_summary`. The detectors iterate over every
  column with a diff and can be noticeably slow on large value-diff
  tables, so they are off by default. When `FALSE`,
  `cmp$pattern_summary` is an empty tibble with the usual column layout.

- n_first_last:

  Non-negative integer (default `5`). Per matched column with at least
  one cell diff, capture the first and last `n_first_last` differences
  in `key_id` order on `cmp$first_last_unequal`. Mirrors the *First /
  Last N Obs With Some Compared Variables Unequal* tables of SAS
  `PROC COMPARE`. Set to `0` to skip.

- max_unmatched_rows:

  Non-negative integer (default `100`). Cap on how many full base-only /
  comp-only rows are stored on `cmp$unmatched_rows` for inspection (and
  surfaced by
  [`ks_unmatched_rows()`](https://al-garik.github.io/ksCompare/reference/ks_unmatched_rows.md)
  / the HTML / XLSX reports). The cap is apportioned proportionally
  between the two sides; truncation is recorded on the result's
  `truncated` and `n_total` attributes. Set to `0` to skip storing the
  row data entirely (the row counts on `cmp$key_diff` and
  [`summary()`](https://rdrr.io/r/base/summary.html) are unaffected).

- loglevel:

  Controls what [`print()`](https://rdrr.io/r/base/print.html) emits
  after the comparison. One of:

  - `"verdict"` (default): prints a single coloured verdict line
    (`cli_alert_success` / `_warning` / `_danger`) plus any `"critical"`
    recommendations. The full schema / rows / values breakdown is
    omitted. Pipeline info messages (duplicate-key resolution, auto-key
    selection, etc.) are unaffected.

  - `"verbose"`: [`print()`](https://rdrr.io/r/base/print.html) emits
    the complete schema / rows / values breakdown. Pipeline info
    messages fire normally as well.

## Value

A `ks_comparison` object with components:

- `meta`: counts, keys, matching strategy, row-key lookup table.

- `schema_diff`: one row per matched / unmatched column with kind /
  label / format comparison.

- `key_diff`: counts of matched / base-only / comp-only rows.

- `row_diff`: long table of `key_id`, `base_row`, `comp_row`, `status`.

- `value_diff`: long table of differing cells with `column_base`,
  `column_comp`, `kind`, `base`, `comp`, `diff`, `note`.

- `pattern_summary`: detected recurring shapes per column (only
  populated when `find_patterns = TRUE`; otherwise an empty tibble with
  the standard column layout).

- `unmatched_rows`: full base-only / comp-only rows (capped by
  `max_unmatched_rows`); see
  [`ks_unmatched_rows()`](https://al-garik.github.io/ksCompare/reference/ks_unmatched_rows.md).

- `first_last_unequal`: per-column first / last `n_first_last` differing
  observations in `key_id` order (PROC COMPARE-style).

- `verdict`: a list with `headline` (character), `severity` (`"ok"`,
  `"info"`, `"warn"`, or `"critical"`), `pct_match` (0–100 numeric),
  `n_cells`, and `n_diffs`; the same summary shown by
  [`print()`](https://rdrr.io/r/base/print.html).

- `options`, `tolerance`: the inputs (echoed for the manifest).

- `manifest`: input hashes + run metadata.

Inspect with [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
or render via
[`ks_report_html()`](https://al-garik.github.io/ksCompare/reference/ks_report_html.md)
/
[`ks_report_xlsx()`](https://al-garik.github.io/ksCompare/reference/ks_report_xlsx.md).

## Pipeline

1.  **Schema alignment** — columns are paired by an explicit `mapping`
    first, then by exact name match. Unmatched columns become
    `base_only` / `comp_only` and are reported in `cmp$schema_diff`.

2.  **Key resolution** — `by` is taken literally if supplied,
    auto-inferred from shared columns when `by = "auto"`, or rows are
    matched by row position when `by = NULL`.

3.  **Row matching** — keyed-unique join, duplicate-key resolution
    (`dup_keys`), or position-zip; details land in `cmp$meta$matching`
    (also displayed by [`print()`](https://rdrr.io/r/base/print.html)
    and in the HTML report).

4.  **Cell diff** — per-column equality respecting
    [`ks_tol()`](https://al-garik.github.io/ksCompare/reference/ks_tol.md)
    (abs / rel / ULP) for numerics,
    [`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md)
    for strings / NAs / SAS special missings, and
    [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html)
    for compatible types.

5.  **Pattern detection** — recurring shapes across diffs (constant
    offset, sign flip, trim-only, …) are scored and surfaced on
    `cmp$pattern_summary`.

6.  **Manifest** — `xxhash64` digests of inputs / options / tolerance
    plus a timestamp and package version, for QC traceability.

## Examples

``` r
a <- data.frame(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
b <- data.frame(id = 1:3, x = c(1, 2, 4), y = c("a", "B", "c"))

# Strict compare on `id`
cmp <- ks_compare(a, b, by = "id")
#> ⚠ a vs b — 2 value diffs across 2 columns
cmp
#> 
#> ── ksCompare comparison ────────────────────────────────────────────────────────
#> ℹ Verdict: 77.78% of matched cells equal; 2 diffs across 2 columns (top cause: letter case differs).
#> Base: a (3 × 3)
#> Comp: b (3 × 3)
#> Keys: id
#> 
#> ── Schema ──
#> 
#> • Matched columns: 3
#> 
#> ── Rows ──
#> 
#> • Strategy: keyed match on "id" (unique on both sides)
#> • Matched: 3
#> • Base-only: 0
#> • Comp-only: 0
#> 
#> ── Values ──
#> 
#> ! 2 value differences across 2 columns.
#> • x: 1 diff
#> • y: 1 diff
tibble::as_tibble(cmp)
#> # A tibble: 2 × 11
#>   key_id base_row comp_row column_base column_comp kind      base  comp   diff
#>    <int>    <int>    <int> <chr>       <chr>       <chr>     <chr> <chr> <dbl>
#> 1      3        3        3 x           x           double    3     4        -1
#> 2      2        2        2 y           y           character b     B        NA
#> # ℹ 2 more variables: na_flow <chr>, note <chr>

# Tolerant numeric compare
ks_compare(
  data.frame(id = 1, x = 1.0),
  data.frame(id = 1, x = 1.0001),
  by = "id",
  tolerance = ks_tol(abs = 1e-3)
)
#> ✔ data.frame(id = 1, x = 1) vs data.frame(id = 1, x = 1.0001) — identical

# Renamed key column
ks_compare(
  data.frame(USUBJID = "S001", x = 1),
  data.frame(SUBJID  = "S001", x = 1),
  by = c(USUBJID = "SUBJID")
)
#> ⚠ data.frame(USUBJID = "S001", x = 1) vs data.frame(SUBJID = "S001", x = 1) —
#> no value diffs; 2 schema difference(s)
```
