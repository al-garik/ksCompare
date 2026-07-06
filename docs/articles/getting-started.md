# Getting started with ksCompare

`ksCompare` compares two data frames in the spirit of SAS `PROC COMPARE`
— column-by-column, row-by-row, with tolerances and labelled-metadata
awareness — and returns a structured `ks_comparison` object you can
print, query, export, or render to HTML / Excel.

## A first comparison

``` r

base <- data.frame(
  id   = 1:5,
  age  = c(34, 41, 28, 55, 19),
  arm  = c("A", "A", "B", "B", "A"),
  stringsAsFactors = FALSE
)
comp <- data.frame(
  id   = 1:5,
  age  = c(34, 41, 27, 55, 19),
  arm  = c("A", "A", "b", "B", "A"),
  stringsAsFactors = FALSE
)

cmp <- ks_compare(base, comp, by = "id")
#> ⚠ base vs comp — 2 value diffs across 2 columns
cmp
#> 
#> ── ksCompare comparison ────────────────────────────────────────────────────────
#> ℹ Verdict: 86.67% of matched cells equal; 2 diffs across 2 columns (top cause: letter case differs).
#> Base: base (5 × 3)
#> Comp: comp (5 × 3)
#> Keys: id
#> 
#> ── Schema ──
#> 
#> • Matched columns: 3
#> 
#> ── Rows ──
#> 
#> • Strategy: keyed match on "id" (unique on both sides)
#> • Matched: 5
#> • Base-only: 0
#> • Comp-only: 0
#> 
#> ── Values ──
#> 
#> ! 2 value differences across 2 columns.
#> • age: 1 diff
#> • arm: 1 diff
```

`summary(cmp)` returns a structured object with per-section counts;
`as_tibble(cmp)` returns the long-format value-diff table.

``` r

summary(cmp)
#> 
#> ── ksCompare summary ───────────────────────────────────────────────────────────
#> Rows (base / comp): 5 / 5
#> Matched / base-only / comp-only rows: 5 / 0 / 0
#> Matched / base-only / comp-only columns: 3 / 0 / 0
#> Cells with diffs: 2 (in 2 columns)
tibble::as_tibble(cmp) |> head()
#> # A tibble: 2 × 11
#>   key_id base_row comp_row column_base column_comp kind      base  comp   diff
#>    <int>    <int>    <int> <chr>       <chr>       <chr>     <chr> <chr> <dbl>
#> 1      3        3        3 age         age         double    28    27        1
#> 2      3        3        3 arm         arm         character B     b        NA
#> # ℹ 2 more variables: na_flow <chr>, note <chr>
```

## What’s inside `cmp`?

A `ks_comparison` is a list with named slots; you can read them directly
when scripting QC dashboards:

``` r

names(cmp)
#>  [1] "meta"               "schema_diff"        "key_diff"          
#>  [4] "row_diff"           "value_diff"         "pattern_summary"   
#>  [7] "unmatched_rows"     "first_last_unequal" "options"           
#> [10] "tolerance"          "manifest"           "verdict"
```

- `cmp$schema_diff` – one row per column pair (matched, base-only, or
  comp-only) with `kind_match`, `label_match`, `format_match`.
- `cmp$row_diff` – one row per matched / unmatched row with `key_id`,
  `base_row`, `comp_row`, `status`.
- `cmp$value_diff` – one row per differing cell with `kind`, `base`,
  `comp`, `diff`, `note`. Same as `as_tibble(cmp)` and `ks_tidy(cmp)`.
- `cmp$pattern_summary` – recurring shapes per column (e.g. `case_only`,
  `constant_offset`) when `find_patterns = TRUE` was requested;
  otherwise an empty tibble with the usual columns.
- `cmp$unmatched_rows` – full base-only / comp-only observations, capped
  by `max_unmatched_rows`.
- `cmp$first_last_unequal` – PROC COMPARE-style first / last differing
  observations per matched column.
- `cmp$meta` – counts, key columns, row-matching strategy, and the
  rendered key labels used by reports.
- `cmp$manifest` – input hashes + run timestamp for QC traceability.
- `cmp$verdict` – executive headline used by `print(cmp)` and the HTML
  report.

``` r

cmp$schema_diff
#> # A tibble: 3 × 14
#>   base  comp  side    kind_base kind_comp kind_match label_base label_comp
#>   <chr> <chr> <chr>   <chr>     <chr>     <lgl>      <chr>      <chr>     
#> 1 id    id    matched integer   integer   TRUE       NA         NA        
#> 2 age   age   matched double    double    TRUE       NA         NA        
#> 3 arm   arm   matched character character TRUE       NA         NA        
#> # ℹ 6 more variables: label_match <lgl>, label_diff <chr>, format_base <chr>,
#> #   format_comp <chr>, format_match <lgl>, format_diff <chr>
cmp$value_diff
#> # A tibble: 2 × 11
#>   key_id base_row comp_row column_base column_comp kind      base  comp   diff
#>    <int>    <int>    <int> <chr>       <chr>       <chr>     <chr> <chr> <dbl>
#> 1      3        3        3 age         age         double    28    27        1
#> 2      3        3        3 arm         arm         character B     b        NA
#> # ℹ 2 more variables: na_flow <chr>, note <chr>
ks_glance(cmp)
#> # A tibble: 1 × 8
#>   n_base_rows n_comp_rows n_matched_rows n_base_only_rows n_comp_only_rows
#>         <int>       <int>          <int>            <int>            <int>
#> 1           5           5              5                0                0
#> # ℹ 3 more variables: n_matched_columns <int>, n_value_diffs <int>,
#> #   n_columns_with_diffs <int>
```

## Triage helpers

When a comparison is not clean, these helpers narrow the search space
quickly:

``` r

ks_cause_summary(cmp)
#> # A tibble: 2 × 4
#>   cause               n_cells n_columns columns
#>   <chr>                 <int>     <int> <chr>  
#> 1 letter case differs       1         1 arm    
#> 2 plain value change        1         1 age
ks_row_diff_summary(cmp)
#> # A tibble: 1 × 6
#>   key_id base_row comp_row key_label n_diffs columns 
#>    <int>    <int>    <int> <chr>       <int> <chr>   
#> 1      3        3        3 id = 3          2 age, arm
```

Use `ks_unmatched_rows(cmp)` to inspect full base-only / comp-only
records, and rerun with `find_patterns = TRUE` when you want
`cmp$pattern_summary` populated.

## Tolerances

``` r

ks_compare(
  data.frame(id = 1, x = 1.0),
  data.frame(id = 1, x = 1.0001),
  by = "id",
  tolerance = ks_tol(abs = 1e-3)
)
#> ✔ data.frame(id = 1, x = 1) vs data.frame(id = 1, x =
#> 1.0001) — identical
```

Per-column overrides:

``` r

ks_tol(
  abs = 0,
  per_column = list(
    weight = ks_tol(abs = 0.01),
    height = ks_tol(rel = 0.001)
  )
)
```

ULP tolerance is useful when you only want to absorb floating-point
round-trip noise:

``` r

ks_compare(base, comp, by = "id", tolerance = ks_tol(ulp = 4))
```

## Options

[`ks_comp_options()`](https://al-garik.github.io/ksCompare/reference/ks_comp_options.md)
controls semantics that apply to all columns:

- `na_equal`: treat `NA == NA` as equal (default `TRUE`).
- `sas_special_missing`: distinguish `.A`-`.Z` and `._` SAS missings
  (haven-imported numerics only).
- `compare_labels` / `compare_formats`: schema-level checks. Format
  comparison is trailing-dot- and case-tolerant (`DATE9.` == `DATE9` ==
  `date9.`).
- `str_trim`, `str_case`, `str_norm`: string normalisation. Strings are
  always compared encoding-safely (Latin1 vs UTF-8 will not produce
  false diffs).
- `tz`: `"preserve"` (default), `"UTC"`, or `"strip"` for `POSIXct`.

``` r

ks_compare(
  base, comp, by = "id",
  options = ks_comp_options(str_trim = TRUE, str_case = "fold")
)
```

## Renamed columns

Use `mapping =` for renamed non-key columns and a named `by =` for a
renamed key:

``` r

ks_compare(
  base, comp,
  by      = c(USUBJID = "SUBJID"),
  mapping = c(AGE = "AGE_YEARS")
)
```

## Reading files directly

[`ks_compare()`](https://al-garik.github.io/ksCompare/reference/ks_compare.md)
accepts file paths in addition to in-memory data frames and dispatches
the right reader by extension:

``` r

# SAS transport file
ks_compare("prod/adsl.xpt", "qc/adsl.xpt", by = "USUBJID")

# .sas7bdat
ks_compare("prod/adsl.sas7bdat", "qc/adsl.sas7bdat", by = "USUBJID")

# Parquet / Feather
ks_compare("a.parquet", "b.parquet", by = "id")

# RDS / RData / CSV / TSV are also recognised
ks_compare("a.rds", "b.rds", by = "id")
```

When both paths share the same basename (`prod/adsl.xpt` vs
`qc/adsl.xpt`), display names are automatically disambiguated using the
parent folder, so `print(cmp)` and the HTML report show `prod/adsl` vs
`qc/adsl` instead of two identical labels.

## Duplicate keys

By default `dup_keys = "first"` keeps the first occurrence per side
(emitting a `ksCompare_dup_keys_resolved` info message). Other
strategies:

- `"last"` – keep the last per side.
- `"keep_all"` – pair duplicates positionally within each key.
- `"all_pairs"` – cartesian-pair duplicates (warns when group sizes
  differ).
- `"error"` – raise on any duplicate key.

``` r

ks_compare(base, comp, by = "id", dup_keys = "keep_all")
```

## Next steps

- **From SAS:** see
  [`vignette("from-proc-compare")`](https://al-garik.github.io/ksCompare/articles/from-proc-compare.md)
  for a side-by-side mapping of `PROC COMPARE` options.
- **Smart features:** see
  [`vignette("smart-features")`](https://al-garik.github.io/ksCompare/articles/smart-features.md)
  for auto-key inference, duplicate-key handling, pattern detection, and
  SAS-style first / last unequal summaries.
- **Reports:** see
  [`vignette("reports")`](https://al-garik.github.io/ksCompare/articles/reports.md)
  for HTML and Excel output.
- **CI / pipelines:** see
  [`vignette("pipeline-gates")`](https://al-garik.github.io/ksCompare/articles/pipeline-gates.md)
  for using comparisons as assertions.
