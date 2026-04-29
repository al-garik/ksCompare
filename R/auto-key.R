#' Internal: infer a key from common columns of `base` and `comp`
#'
#' Returns a tibble of `base`/`comp` column names that uniquely identifies
#' rows in **both** frames, or `NULL` if none can be found within the
#' search budget.
#'
#' Strategy:
#' 1. Restrict to columns that appear (by exact name) in both frames.
#' 2. For subset sizes 1, 2, ..., `max_key_cols`, try every combination,
#'    in column order, and pick the first one that is unique on both sides.
#' 3. Skip combinations that include all-`NA` columns or list columns.
#'
#' We deliberately do **not** suggest a key if uniqueness is only achieved
#' on one side; clinical QC requires both sides to be uniquely keyed for
#' a meaningful keyed compare.
#'
#' @keywords internal
#' @noRd
ks_infer_key <- function(base, comp, max_key_cols = 4L) {
  shared <- intersect(names(base), names(comp))
  if (length(shared) == 0L) {
    return(NULL)
  }
  # Filter to "key-eligible" columns: not list, not entirely NA on either side
  eligible <- vapply(
    shared,
    function(nm) {
      bv <- base[[nm]]
      cv <- comp[[nm]]
      if (is.list(bv) || is.list(cv)) {
        return(FALSE)
      }
      if (all(is.na(bv)) || all(is.na(cv))) {
        return(FALSE)
      }
      TRUE
    },
    logical(1L)
  )
  shared <- shared[eligible]
  if (length(shared) == 0L) {
    return(NULL)
  }

  max_k <- min(max_key_cols, length(shared))
  for (k in seq_len(max_k)) {
    combos <- utils::combn(shared, k, simplify = FALSE)
    for (cols in combos) {
      if (ks_is_unique(base, cols) && ks_is_unique(comp, cols)) {
        return(tibble::tibble(base = cols, comp = cols))
      }
    }
  }
  NULL
}

#' Internal: are these columns jointly unique in `df`?
#' @keywords internal
#' @noRd
ks_is_unique <- function(df, cols) {
  sub <- df[, cols, drop = FALSE]
  !vctrs::vec_duplicate_any(sub)
}
