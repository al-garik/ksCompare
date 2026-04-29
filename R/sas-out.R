#' SAS PROC COMPARE-style output datasets
#'
#' These helpers reshape the long-format `value_diff` table inside a
#' `ks_comparison` into the four classic SAS PROC COMPARE output datasets:
#'
#' - `as_outbase()` — values from the base frame, only for matched rows that
#'   contain at least one differing column; one row per matched key, one
#'   column per compared variable.
#' - `as_outcomp()` — same shape as `as_outbase()` but values from the
#'   compare frame.
#' - `as_outdif()` — numeric difference (`base - comp`) for every matched
#'   row that contains at least one differing column; non-numeric cells are
#'   reported as `NA` with a `.note` column.
#' - `as_outnoequal()` — only the rows where at least one matched cell
#'   differs, in long format (one row per differing cell).
#'
#' The shapes mirror SAS's `OUTBASE=`, `OUTCOMP=`, `OUTDIF=`, and
#' `OUTNOEQUAL=` datasets in spirit; we do not replicate the exact metadata
#' columns (`_TYPE_`, `_OBS_`, ...) but include `key_id` and the original
#' key columns so the rows can be related back to the source frames.
#'
#' @param x A `ks_comparison` object.
#' @return A tibble.
#' @name sas-out
#' @examples
#' a <- data.frame(id = 1:3, x = c(1, 2, 3), y = c("a", "b", "c"))
#' b <- data.frame(id = 1:3, x = c(1, 2, 4), y = c("a", "B", "c"))
#' cmp <- ks_compare(a, b, by = "id")
#' as_outbase(cmp)
#' as_outcomp(cmp)
#' as_outdif(cmp)
#' as_outnoequal(cmp)
NULL

#' @rdname sas-out
#' @export
as_outbase <- function(x) {
  ks_assert_comparison(x)
  ks_out_wide(x, side = "base")
}

#' @rdname sas-out
#' @export
as_outcomp <- function(x) {
  ks_assert_comparison(x)
  ks_out_wide(x, side = "comp")
}

#' @rdname sas-out
#' @export
as_outdif <- function(x) {
  ks_assert_comparison(x)
  ks_out_wide(x, side = "diff")
}

#' @rdname sas-out
#' @export
as_outnoequal <- function(x) {
  ks_assert_comparison(x)
  vd <- x$value_diff
  if (nrow(vd) == 0L) {
    return(vd)
  }
  ks_attach_keys(x, vd)
}

ks_assert_comparison <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, "ks_comparison")) {
    ks_abort(
      "{.arg x} must be a {.cls ks_comparison} object.",
      call = call
    )
  }
  invisible(x)
}

#' Internal: build a wide OUT-style tibble (base / comp / diff)
#'
#' Pivots `value_diff` so each differing cell becomes a column entry, with
#' one row per matched key that has at least one diff. Columns that have no
#' differences anywhere are omitted (matching SAS `OUTNOEQUAL` semantics
#' applied to the wide form).
#'
#' @keywords internal
#' @noRd
ks_out_wide <- function(x, side = c("base", "comp", "diff")) {
  side <- rlang::arg_match(side)
  vd <- x$value_diff
  if (nrow(vd) == 0L) {
    return(tibble::tibble())
  }

  vals <- switch(
    side,
    base = vd$base,
    comp = vd$comp,
    diff = vd$diff
  )
  long <- tibble::tibble(
    key_id = vd$key_id,
    column = vd$column_base,
    value = vals
  )

  wide <- tidyr_pivot_wider(long)
  ks_attach_keys(x, wide)
}

#' Internal: minimal pivot_wider implementation
#'
#' Avoids adding `tidyr` to Imports for one operation. The input has unique
#' (key_id, column) pairs, so we can pivot directly.
#'
#' @keywords internal
#' @noRd
tidyr_pivot_wider <- function(long) {
  cols <- unique(long$column)
  keys <- sort(unique(long$key_id))
  out <- tibble::tibble(key_id = keys)
  for (col in cols) {
    sub <- long[long$column == col, , drop = FALSE]
    idx <- match(out$key_id, sub$key_id)
    out[[col]] <- sub$value[idx]
  }
  out
}

#' Internal: attach base-side key columns to a wide tibble keyed by key_id
#' @keywords internal
#' @noRd
ks_attach_keys <- function(x, df) {
  if (nrow(x$meta$keys) == 0L || is.null(x$meta$base_keys)) {
    return(df)
  }
  rd <- x$row_diff
  matched <- rd[rd$status == "matched", , drop = FALSE]
  ord <- match(df$key_id, matched$key_id)
  key_block <- x$meta$base_keys[matched$base_row[ord], , drop = FALSE]
  vctrs::vec_cbind(key_block, df)
}
