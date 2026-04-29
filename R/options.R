#' Tolerance specification for numeric comparisons
#'
#' Builds a tolerance object consumed by [ks_compare()]. Two numeric
#' values `a` and `b` are considered equal when **any** of the active
#' rules pass:
#'
#' - `abs(a - b) <= abs`
#' - `abs(a - b) <= rel * max(abs(a), abs(b))`
#' - `ulp_distance(a, b) <= ulp` — IEEE-754 units in the last place,
#'   useful for catching floating-point round-trip noise without false
#'   positives on genuine differences.
#'
#' Per-column overrides may be supplied via `per_column`, a named list
#' whose names are **base-side** column names and whose values are
#' themselves the result of `ks_tol()`. Columns not listed fall back to
#' the top-level `abs` / `rel` / `ulp`.
#'
#' @param abs Non-negative absolute tolerance (default `0` — strict
#'   equality).
#' @param rel Non-negative relative tolerance, scaled by
#'   `max(abs(a), abs(b))` (default `0`).
#' @param ulp Non-negative integer ULP tolerance. Typical values are
#'   `4`–`16`; defaults to `0`.
#' @param per_column Optional named list of per-column [ks_tol()]
#'   overrides keyed by base-side column name.
#' @return A `ks_tol` S3 list.
#' @export
#' @examples
#' ks_tol(abs = 1e-9)
#' ks_tol(rel = 1e-6, per_column = list(price = ks_tol(abs = 0.005)))
#' ks_tol(ulp = 4)
ks_tol <- function(abs = 0, rel = 0, ulp = 0, per_column = NULL) {
  if (!is.numeric(abs) || length(abs) != 1L || abs < 0 || is.na(abs)) {
    ks_abort("{.arg abs} must be a single non-negative number.")
  }
  if (!is.numeric(rel) || length(rel) != 1L || rel < 0 || is.na(rel)) {
    ks_abort("{.arg rel} must be a single non-negative number.")
  }
  if (!is.numeric(ulp) || length(ulp) != 1L || ulp < 0 || is.na(ulp)) {
    ks_abort("{.arg ulp} must be a single non-negative number.")
  }
  if (!is.null(per_column)) {
    if (!is.list(per_column) || is.null(names(per_column))) {
      ks_abort("{.arg per_column} must be a named list of {.fn ks_tol} objects.")
    }
    bad <- !vapply(per_column, inherits, logical(1L), what = "ks_tol")
    if (any(bad)) {
      ks_abort(
        "Each element of {.arg per_column} must be created with {.fn ks_tol}."
      )
    }
  }
  structure(
    list(abs = abs, rel = rel, ulp = ulp, per_column = per_column),
    class = "ks_tol"
  )
}

#' @export
print.ks_tol <- function(x, ...) {
  cli::cli_text(
    "<ks_tol> abs = {.val {x$abs}}, rel = {.val {x$rel}}, ulp = {.val {x$ulp}}"
  )
  if (!is.null(x$per_column)) {
    cli::cli_text("Per-column overrides for: {.field {names(x$per_column)}}")
  }
  invisible(x)
}

#' Resolve the effective tolerance for a column
#'
#' @keywords internal
#' @noRd
ks_tol_for <- function(tol, column) {
  if (!is.null(tol$per_column) && column %in% names(tol$per_column)) {
    return(tol$per_column[[column]])
  }
  tol
}

#' Comparison options
#'
#' Builds an options object consumed by [ks_compare()] that controls
#' missing-value semantics, label / format comparison, string
#' normalisation, and time-zone handling.
#'
#' @param na_equal Treat two `NA`s as equal? Default `TRUE`. With
#'   `FALSE`, any cell where one side is `NA` and the other is not is
#'   reported as a value diff; cells where *both* sides are `NA` are
#'   also reported (mirroring SAS `PROC COMPARE` behaviour with the
#'   `NOMISS` option turned off).
#' @param sas_special_missing Distinguish SAS special missings (`.A`-`.Z`
#'   and `._`) when comparing numeric columns imported via `haven`?
#'   Default `TRUE`. When `TRUE`, `.A` and `.B` compare as different
#'   even though both are `NA` in R; relies on the `tagged_na` tag
#'   attached by `haven::read_sas()`.
#' @param compare_labels Compare `attr(x, "label")` between matched
#'   columns and surface differences in `cmp$schema_diff`? Default
#'   `TRUE`. Set to `FALSE` to suppress label-only diffs (e.g. when
#'   metadata drift is expected).
#' @param compare_formats Compare `attr(x, "format.sas")` between
#'   matched columns? Default `TRUE`. Comparison is trailing-dot- and
#'   case-tolerant, so `DATE9.`, `DATE9`, and `date9.` all compare as
#'   equal.
#' @param str_trim Trim leading/trailing whitespace with
#'   [stringi::stri_trim_both()] before comparing strings? Default
#'   `FALSE`. Useful when one source has been padded by a fixed-width
#'   exporter.
#' @param str_case One of `"sensitive"` (default) or `"fold"`. When
#'   `"fold"`, strings are compared after Unicode case folding
#'   (`stringi::stri_trans_tolower()`).
#' @param str_norm One of `"none"` (default) or `"NFC"`. When `"NFC"`,
#'   strings are Unicode-normalised before comparing so that
#'   visually-identical sequences with different code-point
#'   compositions match.
#' @param tz One of `"preserve"` (default), `"UTC"`, or `"strip"`.
#'   Controls how `POSIXct` columns are reconciled when the two sides
#'   carry different `tzone` attributes:
#'   - `"preserve"`: values are compared as-is (a tz mismatch surfaces
#'     as a diff if it changes the underlying instant).
#'   - `"UTC"`: both sides are converted to UTC before compare.
#'   - `"strip"`: `tzone` is dropped and values compared as POSIXct.
#' @return A `ks_options` S3 list.
#' @export
#' @examples
#' # Defaults: strict NAs, label & format comparison on
#' ks_options()
#'
#' # Loose strings: trim padding and ignore case
#' ks_options(str_trim = TRUE, str_case = "fold")
#'
#' # Suppress label/format drift, but keep cell-level strictness
#' ks_options(compare_labels = FALSE, compare_formats = FALSE)
#'
#' # Treat any NA as a difference (PROC COMPARE without NOMISS)
#' ks_options(na_equal = FALSE)
ks_options <- function(
  na_equal = TRUE,
  sas_special_missing = TRUE,
  compare_labels = TRUE,
  compare_formats = TRUE,
  str_trim = FALSE,
  str_case = c("sensitive", "fold"),
  str_norm = c("none", "NFC"),
  tz = c("preserve", "UTC", "strip")
) {
  str_case <- rlang::arg_match(str_case)
  str_norm <- rlang::arg_match(str_norm)
  tz <- rlang::arg_match(tz)
  for (nm in c(
    "na_equal", "sas_special_missing", "compare_labels",
    "compare_formats", "str_trim"
  )) {
    val <- get(nm)
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      ks_abort(c(
        "!" = "{.arg {nm}} must be a single {.code TRUE} or {.code FALSE}."
      ))
    }
  }
  structure(
    list(
      na_equal = na_equal,
      sas_special_missing = sas_special_missing,
      compare_labels = compare_labels,
      compare_formats = compare_formats,
      str_trim = str_trim,
      str_case = str_case,
      str_norm = str_norm,
      tz = tz
    ),
    class = "ks_options"
  )
}

#' @export
print.ks_options <- function(x, ...) {
  cli::cli_text("<ks_options>")
  for (nm in names(x)) {
    cli::cli_bullets(c("*" = "{.field {nm}}: {.val {x[[nm]]}}"))
  }
  invisible(x)
}
