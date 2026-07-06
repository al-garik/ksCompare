# Reports

A `ks_comparison` can be exported to two report formats. Both are opt-in
via `Suggests` packages so the core install stays small, and neither
requires Quarto, Pandoc, or any internet access.

## HTML

``` r

iris2 <- transform(iris, row_id = seq_len(nrow(iris)))
cmp <- ks_compare(iris2, iris2, by = "row_id")
ks_report_html(
  cmp,
  "report.html",
  title = "ADSL QC",
  subtitle = "Study A1234"
)
```

The HTML report uses `htmltools` for layout and `reactable` for
filterable, searchable tables. Layout includes:

- an executive verdict bar and recommendations card,
- a sticky left-hand table of contents with section badges,
- KPI cards summarising matched / unmatched rows and columns,
- Schema, Row matching, Unmatched rows, Columns with differences, Diff
  causes, Most-affected rows, Value differences, Patterns, and Manifest
  sections,
- a print-friendly stylesheet.

Pass `path = NA` to get the assembled
[`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
for embedding in another document. Pass `theme = "slate"` for a neutral
dark-header theme. If the comparison was not run with
`find_patterns = TRUE`, the Patterns section is still present but will
be empty.

### Smart sampling

Every report table respects `max_rows` (default `100`). For large
value-diff tables, rows above the cap are replaced by a *stratified
sample* covering each affected column and each distinct diff cause
(`note`), prioritising the largest numeric magnitudes. A notice
indicates the sample size and recommends `as_tibble(cmp)` for the full
table
([`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
is never truncated).

### Group by key

Pass `group_by_key = TRUE` to render the value-diff section as one
collapsed `<details>` block per key value, sorted by number of diffs
(most-affected first). Useful for clinical reviews where you want to
inspect “what changed for subject X” rather than scrolling a flat table.

``` r

ks_report_html(cmp, "report.html", group_by_key = TRUE, max_groups = 100)
```

When `dup_keys = "keep_all"` or `"all_pairs"` was used in
\[ks_compare()\], each key block also shows a *Pair* column so you can
tell duplicate-row pairings apart. If no `by =` was supplied (row-
position match), `group_by_key` is silently ignored.

## Excel

``` r

ks_report_xlsx(cmp, "report.xlsx")
```

Sheets:

- `Summary` – headline counts.
- `Schema` – per-column metadata diff.
- `KeyDiff` – matched / base-only / comp-only row counts.
- `Values` – long cell-level diff table.
- `DiffCauses` – grouped causes of cell differences.
- `RowHotspots` – matched rows with the most changed cells.
- `Patterns` – detected recurring shapes (empty unless the comparison
  was run with `find_patterns = TRUE`).
- `UnmatchedRows` – full base-only / comp-only rows stored on the
  comparison object.
- `FirstLastUnequal` – first / last differing observations per matched
  column.
- `OUT_BASE`, `OUT_COMP`, `OUT_DIF`, `OUT_NOEQUAL` – PROC COMPARE- style
  wide outputs (see \[as_outbase()\] et al.).
- `Manifest` – input hashes, package version, run timestamp.

Numeric `OUT_DIF` cells whose magnitude exceeds `threshold` (default
`0`) are highlighted via openxlsx2 conditional formatting. Pass
`highlight = FALSE` to suppress.
