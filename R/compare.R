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
#' @param allow_fuzzy_columns Reserved for future use; must be `FALSE`.
#'   Fuzzy column rename *suggestions* are always returned on
#'   `cmp$column_suggestions` (when `stringdist` is installed) but are
#'   never applied silently.
#' @param options A [ks_comp_options()] specification controlling NA /
#'   SAS special-missing semantics, label / format comparison, string
#'   normalisation, and the default output folder for reports.
#' @param base_name,comp_name Optional display names for the two frames
#'   (used in `print()` and reports). Default to the names of the
#'   supplied expressions.
#'
#' @return A `ks_comparison` object with components:
#'   - `meta`: counts, keys, matching strategy, row-key lookup table.
#'   - `schema_diff`: one row per matched / unmatched column with kind
#'     / label / format comparison.
#'   - `column_suggestions`: fuzzy rename candidates (or empty).
#'   - `key_diff`: counts of matched / base-only / comp-only rows.
#'   - `row_diff`: long table of `key_id`, `base_row`, `comp_row`,
#'     `status`.
#'   - `value_diff`: long table of differing cells with `column_base`,
#'     `column_comp`, `kind`, `base`, `comp`, `diff`, `note`.
#'   - `pattern_summary`: detected recurring shapes per column.
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
  allow_fuzzy_columns = FALSE,
  options = ks_comp_options(),
  base_name = NULL,
  comp_name = NULL
) {
  base_label <- rlang::as_label(rlang::enexpr(base))
  comp_label <- rlang::as_label(rlang::enexpr(comp))

  coerce <- rlang::arg_match(coerce)
  dup_keys <- rlang::arg_match(dup_keys)
  if (!isFALSE(allow_fuzzy_columns)) {
    ks_abort(
      "{.arg allow_fuzzy_columns} is reserved for a future release; must be {.code FALSE}."
    )
  }
  if (!inherits(tolerance, "ks_tol")) {
    ks_abort("{.arg tolerance} must be created with {.fn ks_tol}.")
  }
  if (!inherits(options, "ks_comp_options")) {
    ks_abort("{.arg options} must be created with {.fn ks_comp_options}.")
  }

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
  suggestions <- ks_suggest_columns(base, comp, alignment)
  keys <- ks_resolve_by(base, comp, by)
  match_result <- ks_match_rows(base, comp, keys, dup_keys = dup_keys)
  row_diff <- match_result$row_diff
  matching <- match_result$matching
  matching$keys <- keys
  row_keys <- ks_build_row_keys(row_diff, base, comp, keys)

  # Compute value diffs over matched columns.
  value_parts <- list()
  if (nrow(alignment$pairs) > 0L && nrow(row_diff) > 0L) {
    for (i in seq_len(nrow(alignment$pairs))) {
      bn <- alignment$pairs$base[[i]]
      cn <- alignment$pairs$comp[[i]]
      # Skip key columns from value-diff (they always match by construction
      # when keyed; show as separate diagnostics if they were renamed).
      if (bn %in% keys$base && cn %in% keys$comp) {
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
        value_parts[[length(value_parts) + 1L]] <- part
      }
    }
  }

  value_diff <- if (length(value_parts) == 0L) {
    ks_empty_value_diff()
  } else {
    vctrs::vec_rbind(!!!value_parts)
  }

  # Stable ordering for reproducibility.
  if (nrow(value_diff) > 0L) {
    ord <- order(value_diff$column_base, value_diff$key_id)
    value_diff <- value_diff[ord, , drop = FALSE]
  }

  pattern_summary <- ks_detect_patterns(value_diff)

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
    column_suggestions = suggestions,
    key_diff = tibble::tibble(
      base_only = sum(row_diff$status == "base_only"),
      comp_only = sum(row_diff$status == "comp_only"),
      matched = sum(row_diff$status == "matched")
    ),
    row_diff = row_diff,
    value_diff = value_diff,
    pattern_summary = pattern_summary,
    options = options,
    tolerance = tolerance,
    manifest = manifest
  )
}
