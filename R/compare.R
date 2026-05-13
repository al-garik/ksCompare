#' Compare two data frames
#'
#' `ks_compare()` is the single entry point for the package. It compares
#' `base` against `comp` and returns a `ks_comparison` S3 object containing
#' schema, key, row, and value differences plus a small QC manifest.
#'
#' # Pipeline
#'
#' 1. **Schema alignment** — columns are paired by an explicit `mapping`
#'    first, then by exact name match. Unmatched columns become
#'    `base_only` / `comp_only` and are reported in `cmp$schema_diff`.
#' 2. **Key resolution** — `by` is taken literally if supplied,
#'    auto-inferred from shared columns when `by = "auto"`, or rows are
#'    matched by row position when `by = NULL`.
#' 3. **Row matching** — keyed-unique join, duplicate-key resolution
#'    (`dup_keys`), or position-zip; details land in
#'    `cmp$meta$matching` (also displayed by [print()] and in the HTML
#'    report).
#' 4. **Cell diff** — per-column equality respecting [ks_tol()] (abs /
#'    rel / ULP) for numerics, [ks_comp_options()] for strings / NAs / SAS
#'    special missings, and `vctrs::vec_cast()` for compatible types.
#' 5. **Pattern detection** — recurring shapes across diffs (constant
#'    offset, sign flip, trim-only, …) are scored and surfaced on
#'    `cmp$pattern_summary`.
#' 6. **Manifest** — `xxhash64` digests of inputs / options / tolerance
#'    plus a timestamp and package version, for QC traceability.
#'
#' @param base,comp Either an in-memory data frame (or anything coercible
#'   via [tibble::as_tibble()]: tibbles, data.tables, Arrow tables) or a
#'   single character file path. When given a path, the file is read
#'   automatically based on its extension:
#'   - `.sas7bdat`, `.xpt` (via `haven`)
#'   - `.parquet`, `.feather` / `.arrow` (via `arrow`)
#'   - `.rds`, `.rdata` / `.rda`
#'   - `.csv`, `.tsv` / `.txt`
#'
#'   `base` is treated as the reference; `comp` is the candidate. When
#'   both inputs are file paths that share the same basename (e.g.
#'   `prod/adsl.sas7bdat` vs `qc/adsl.sas7bdat`), display names are
#'   automatically disambiguated with the parent directory.
#' @param by Optional column name(s) to use as the matching key.
#'   - `NULL` (default): rows are matched by row position. A warning
#'     banner is shown in the HTML report so this is never silent.
#'   - `character`: same column name on both sides. Use a named vector
#'     when the key is renamed: `c(USUBJID = "SUBJID")`.
#'   - `"auto"`: search the smallest combination of shared columns that
#'     is unique on **both** sides (capped at 4 columns). Falls back to
#'     position match with a warning if nothing qualifies.
#' @param mapping Optional explicit column mapping for renamed columns.
#'   Either a fully named character vector
#'   (`c(base_col = "comp_col", ...)`) or a 2-column data frame with
#'   columns `base` and `comp`. Mapping pairs always win over
#'   exact-name matching.
#' @param tolerance A [ks_tol()] specification. Defaults to strict
#'   equality. Per-column overrides supported via
#'   `ks_tol(per_column = list(col = ks_tol(...)))`.
#' @param coerce Type-coercion strictness. One of:
#'   - `"safe"` (default): `vctrs::vec_ptype2()` plus integer↔double,
#'     factor↔character, `haven_labelled` unwrapping.
#'   - `"strict"`: only common types `vctrs::vec_ptype2()` agrees on.
#'     Type mismatches surface as `kind = "type_mismatch"` rows.
#'   - `"lossy"`: additionally allows numeric↔character,
#'     date/datetime↔character (ISO-8601), factor↔integer (level codes).
#' @param dup_keys Strategy for duplicate keys (per-side independently).
#'   One of:
#'   - `"first"` (default): keep the first occurrence per side. Drops
#'     rows; emits a `ksCompare_dup_keys_resolved` info message.
#'   - `"last"`: keep the last occurrence per side. Same message.
#'   - `"keep_all"`: pair duplicate rows positionally within each key
#'     group (1↔1, 2↔2, …). Leftover rows on the longer side become
#'     base- or compare-only.
#'   - `"all_pairs"`: cartesian-pair every base row with every comp row
#'     sharing a key value. Emits a
#'     `ksCompare_all_pairs_cardinality` warning when group sizes
#'     disagree.
#'   - `"error"`: raise on any duplicate key.
#' @param options A [ks_comp_options()] specification controlling NA /
#'   SAS special-missing semantics, label / format comparison, string
#'   normalisation, and the default output folder for reports.
#' @param base_name,comp_name Optional display names for the two frames
#'   (used in `print()` and reports). Default to the names of the
#'   supplied expressions.
#' @param find_patterns Logical (default `FALSE`). When `TRUE`, run the
#'   pattern-detection pass (constant offsets, sign flips, unit
#'   rescales, epoch shifts, etc.) and populate `cmp$pattern_summary`.
#'   The detectors iterate over every column with a diff and can be
#'   noticeably slow on large value-diff tables, so they are off by
#'   default. When `FALSE`, `cmp$pattern_summary` is an empty tibble
#'   with the usual column layout.
#' @param n_first_last Non-negative integer (default `5`). Per matched
#'   column with at least one cell diff, capture the first and last
#'   `n_first_last` differences in `key_id` order on
#'   `cmp$first_last_unequal`. Mirrors the *First / Last N Obs With
#'   Some Compared Variables Unequal* tables of SAS `PROC COMPARE`.
#'   Set to `0` to skip.
#' @param max_unmatched_rows Non-negative integer (default `100`).
#'   Cap on how many full base-only / comp-only rows are stored on
#'   `cmp$unmatched_rows` for inspection (and surfaced by
#'   [ks_unmatched_rows()] / the HTML / XLSX reports). The cap is
#'   apportioned proportionally between the two sides; truncation is
#'   recorded on the result's `truncated` and `n_total` attributes.
#'   Set to `0` to skip storing the row data entirely (the row counts
#'   on `cmp$key_diff` and `summary()` are unaffected).
#'
#' @return A `ks_comparison` object with components:
#'   - `meta`: counts, keys, matching strategy, row-key lookup table.
#'   - `schema_diff`: one row per matched / unmatched column with kind
#'     / label / format comparison.
#'   - `key_diff`: counts of matched / base-only / comp-only rows.
#'   - `row_diff`: long table of `key_id`, `base_row`, `comp_row`,
#'     `status`.
#'   - `value_diff`: long table of differing cells with `column_base`,
#'     `column_comp`, `kind`, `base`, `comp`, `diff`, `note`.
#'   - `pattern_summary`: detected recurring shapes per column (only
#'     populated when `find_patterns = TRUE`; otherwise an empty
#'     tibble with the standard column layout).
#'   - `unmatched_rows`: full base-only / comp-only rows (capped by
#'     `max_unmatched_rows`); see [ks_unmatched_rows()].
#'   - `first_last_unequal`: per-column first / last `n_first_last`
#'     differing observations in `key_id` order (PROC COMPARE-style).
#'   - `options`, `tolerance`: the inputs (echoed for the manifest).
#'   - `manifest`: input hashes + run metadata.
#'
#'   Inspect with [print()], [summary()], [tibble::as_tibble()], or
#'   render via [ks_report_html()] / [ks_report_xlsx()].
#'
#' @export
#' @examples
#' a <- data.frame(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
#' b <- data.frame(id = 1:3, x = c(1, 2, 4), y = c("a", "B", "c"))
#'
#' # Strict compare on `id`
#' cmp <- ks_compare(a, b, by = "id")
#' cmp
#' tibble::as_tibble(cmp)
#'
#' # Tolerant numeric compare
#' ks_compare(
#'   data.frame(id = 1, x = 1.0),
#'   data.frame(id = 1, x = 1.0001),
#'   by = "id",
#'   tolerance = ks_tol(abs = 1e-3)
#' )
#'
#' # Renamed key column
#' ks_compare(
#'   data.frame(USUBJID = "S001", x = 1),
#'   data.frame(SUBJID  = "S001", x = 1),
#'   by = c(USUBJID = "SUBJID")
#' )
ks_compare <- function(
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
  max_unmatched_rows = 100L
) {
  base_label <- rlang::as_label(rlang::enexpr(base))
  comp_label <- rlang::as_label(rlang::enexpr(comp))

  coerce <- rlang::arg_match(coerce)
  dup_keys <- rlang::arg_match(dup_keys)
  if (!inherits(tolerance, "ks_tol")) {
    ks_abort("{.arg tolerance} must be created with {.fn ks_tol}.")
  }
  if (!inherits(options, "ks_comp_options")) {
    ks_abort("{.arg options} must be created with {.fn ks_comp_options}.")
  }
  if (!is.logical(find_patterns) || length(find_patterns) != 1L || is.na(find_patterns)) {
    ks_abort("{.arg find_patterns} must be a single {.code TRUE} or {.code FALSE}.")
  }
  if (!is.numeric(n_first_last) || length(n_first_last) != 1L ||
      is.na(n_first_last) || n_first_last < 0) {
    ks_abort("{.arg n_first_last} must be a single non-negative integer.")
  }
  n_first_last <- as.integer(n_first_last)
  if (!is.numeric(max_unmatched_rows) || length(max_unmatched_rows) != 1L ||
      is.na(max_unmatched_rows) || max_unmatched_rows < 0) {
    ks_abort("{.arg max_unmatched_rows} must be a single non-negative integer.")
  }
  max_unmatched_rows <- as.integer(max_unmatched_rows)

  base <- ks_load_input(base, arg_name = "base")
  comp <- ks_load_input(comp, arg_name = "comp")

  auto_names <- ks_display_names(base, comp, base_label, comp_label)
  base_name <- base_name %||% auto_names$base
  comp_name <- comp_name %||% auto_names$comp

  base_source <- ks_input_path(base)
  comp_source <- ks_input_path(comp)

  base <- tibble::as_tibble(base)
  comp <- tibble::as_tibble(comp)

  alignment <- ks_align_columns(base, comp, mapping = mapping)
  schema_diff <- ks_schema_diff(base, comp, alignment, options)
  keys <- ks_resolve_by(base, comp, by)
  match_result <- ks_match_rows(base, comp, keys, dup_keys = dup_keys)
  row_diff <- match_result$row_diff
  matching <- match_result$matching
  matching$keys <- keys
  row_keys <- ks_build_row_keys(row_diff, base, comp, keys)

  # Compute value diffs over matched columns.
  n_pairs <- nrow(alignment$pairs)
  value_parts <- vector("list", n_pairs)
  vp_idx <- 0L
  if (n_pairs > 0L && nrow(row_diff) > 0L) {
    key_base <- keys$base
    key_comp <- keys$comp
    pair_b <- alignment$pairs$base
    pair_c <- alignment$pairs$comp
    for (i in seq_len(n_pairs)) {
      bn <- pair_b[[i]]
      cn <- pair_c[[i]]
      # Skip key columns from value-diff (they always match by construction
      # when keyed; show as separate diagnostics if they were renamed).
      if (bn %in% key_base && cn %in% key_comp) {
        next
      }
      part <- ks_value_diff_one(
        base_col = base[[bn]],
        comp_col = comp[[cn]],
        base_name = bn,
        comp_name = cn,
        match = row_diff,
        tol = tolerance,
        options = options
      )
      if (nrow(part) > 0L) {
        vp_idx <- vp_idx + 1L
        value_parts[[vp_idx]] <- part
      }
    }
  }

  value_diff <- if (vp_idx == 0L) {
    ks_empty_value_diff()
  } else {
    vctrs::vec_rbind(!!!value_parts[seq_len(vp_idx)])
  }

  # Stable ordering for reproducibility.
  if (nrow(value_diff) > 0L) {
    ord <- order(value_diff$column_base, value_diff$key_id, method = "radix")
    value_diff <- value_diff[ord, , drop = FALSE]
  }

  pattern_summary <- if (isTRUE(find_patterns)) {
    ks_detect_patterns(value_diff)
  } else {
    ks_empty_pattern_summary()
  }

  unmatched_rows <- ks_build_unmatched_rows(
    row_diff, base, comp, row_keys,
    max_rows = max_unmatched_rows
  )
  first_last_unequal <- ks_build_first_last_unequal(value_diff, n = n_first_last)

  meta <- list(
    base_name = base_name,
    comp_name = comp_name,
    base_source = base_source,
    comp_source = comp_source,
    n_base_rows = nrow(base),
    n_comp_rows = nrow(comp),
    n_base_cols = ncol(base),
    n_comp_cols = ncol(comp),
    keys = keys,
    base_keys = if (nrow(keys) > 0L) base[, keys$base, drop = FALSE] else NULL,
    coerce = coerce,
    matching = matching,
    row_keys = row_keys
  )

  manifest <- list(
    base_hash = ks_digest(base),
    comp_hash = ks_digest(comp),
    options_hash = ks_digest(options),
    tolerance_hash = ks_digest(tolerance),
    created_at = Sys.time(),
    package_version = utils::packageVersion("ksCompare")
  )

  new_ks_comparison(
    meta = meta,
    schema_diff = schema_diff,
    key_diff = tibble::tibble(
      base_only = sum(row_diff$status == "base_only"),
      comp_only = sum(row_diff$status == "comp_only"),
      matched = sum(row_diff$status == "matched")
    ),
    row_diff = row_diff,
    value_diff = value_diff,
    pattern_summary = pattern_summary,
    unmatched_rows = unmatched_rows,
    first_last_unequal = first_last_unequal,
    options = options,
    tolerance = tolerance,
    manifest = manifest
  )
}
