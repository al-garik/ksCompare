#' Internal: build the unmatched-rows tibble
#'
#' Returns a tibble describing rows present on only one side of a
#' comparison. Used by [ks_unmatched_rows()], the HTML / XLSX reports,
#' and the `include_unmatched` mode of [ks_tidy()].
#'
#' Result columns:
#' - `side`     : `"base_only"` or `"comp_only"`.
#' - `key_id`   : matching `key_id` from `cmp$row_diff`.
#' - `key_label`: rendered key tuple (or `NA` for position match).
#' - `base_row` : original 1-based row index in `base` (NA for
#'   `comp_only`).
#' - `comp_row` : original 1-based row index in `comp` (NA for
#'   `base_only`).
#' - then **all data columns** taken from the side that holds the row,
#'   in their original order.
#'
#' Capped at `max_rows` total rows (base + comp). Truncation is
#' communicated via the `truncated` attribute (a logical vector named
#' `c("base", "comp")`) and the `n_total` attribute (full counts before
#' capping).
#'
#' @keywords internal
#' @noRd
ks_build_unmatched_rows <- function(row_diff, base, comp, row_keys, keys = NULL, max_rows = 100L) {
  empty <- tibble::tibble(
    side = character(),
    key_id = integer(),
    key_label = character(),
    base_row = integer(),
    comp_row = integer()
  )
  attr(empty, "truncated") <- c(base = FALSE, comp = FALSE)
  attr(empty, "n_total") <- c(base = 0L, comp = 0L)

  if (is.null(row_diff) || nrow(row_diff) == 0L) {
    return(empty)
  }

  bo_idx <- which(row_diff$status == "base_only")
  co_idx <- which(row_diff$status == "comp_only")
  n_bo_total <- length(bo_idx)
  n_co_total <- length(co_idx)

  if (n_bo_total == 0L && n_co_total == 0L) {
    return(empty)
  }

  # Determine which columns to exclude (key columns, since they're in key_label)
  exclude_base <- if (!is.null(keys) && nrow(keys) > 0L) keys$base else character()
  exclude_comp <- if (!is.null(keys) && nrow(keys) > 0L) keys$comp else character()

  # Apportion the cap between the two sides proportionally so neither
  # side hides the other entirely.
  cap <- as.integer(max_rows)
  if (is.na(cap) || cap < 0L) cap <- 0L
  total <- n_bo_total + n_co_total
  if (cap >= total) {
    keep_bo <- n_bo_total
    keep_co <- n_co_total
  } else if (cap == 0L) {
    keep_bo <- 0L
    keep_co <- 0L
  } else {
    keep_bo <- min(n_bo_total, as.integer(round(cap * n_bo_total / total)))
    keep_co <- cap - keep_bo
    if (keep_co > n_co_total) {
      keep_co <- n_co_total
      keep_bo <- min(n_bo_total, cap - keep_co)
    }
  }

  bo_idx <- utils::head(bo_idx, keep_bo)
  co_idx <- utils::head(co_idx, keep_co)

  make_part <- function(side, idx, src, exclude_cols) {
    if (length(idx) == 0L) {
      return(NULL)
    }
    is_base <- identical(side, "base_only")
    src_rows <- if (is_base) row_diff$base_row[idx] else row_diff$comp_row[idx]
    n <- length(idx)
    head_df <- tibble::tibble(
      side = rep(side, n),
      key_id = row_diff$key_id[idx],
      key_label = if (!is.null(row_keys)) row_keys$key_label[match(row_diff$key_id[idx], row_keys$key_id)] else rep(NA_character_, n),
      base_row = if (is_base) as.integer(src_rows) else rep(NA_integer_, n),
      comp_row = if (!is_base) as.integer(src_rows) else rep(NA_integer_, n)
    )
    # Extract data, excluding key columns to avoid type mismatch issues
    data <- src[src_rows, , drop = FALSE]
    if (length(exclude_cols) > 0L) {
      keep_cols <- setdiff(names(data), exclude_cols)
      data <- data[, keep_cols, drop = FALSE]
    }
    vctrs::vec_cbind(head_df, tibble::as_tibble(data))
  }

  parts <- list(
    make_part("base_only", bo_idx, base, exclude_base),
    make_part("comp_only", co_idx, comp, exclude_comp)
  )
  parts <- parts[!vapply(parts, is.null, logical(1L))]

  out <- if (length(parts) == 0L) {
    empty
  } else if (length(parts) == 1L) {
    parts[[1L]]
  } else {
    # Different sides may have different non-key columns; bind by name
    # and let missing columns become NA.
    vctrs::vec_rbind(!!!parts)
  }

  attr(out, "truncated") <- c(
    base = keep_bo < n_bo_total,
    comp = keep_co < n_co_total
  )
  attr(out, "n_total") <- c(base = n_bo_total, comp = n_co_total)
  out
}

#' Internal: build the first/last-unequal summary
#'
#' For each column with at least one cell diff, capture the first `n`
#' and last `n` differences in `key_id` order. Mirrors the
#' "First/Last N Obs With Some Compared Variables Unequal" tables of
#' SAS `PROC COMPARE`.
#'
#' Result columns: `column_base`, `column_comp`, `position`
#' (`"first"` / `"last"`), `rank` (1..n), plus the relevant subset of
#' `value_diff` (`key_id`, `kind`, `base`, `comp`, `diff`, `note`).
#'
#' @keywords internal
#' @noRd
ks_build_first_last_unequal <- function(value_diff, n = 5L) {
  empty <- tibble::tibble(
    column_base = character(),
    column_comp = character(),
    position = character(),
    rank = integer(),
    key_id = integer(),
    base_row = integer(),
    comp_row = integer(),
    kind = character(),
    base = character(),
    comp = character(),
    diff = numeric(),
    na_flow = character(),
    note = character()
  )
  if (is.null(value_diff) || nrow(value_diff) == 0L || n <= 0L) {
    return(empty)
  }
  cols <- unique(value_diff$column_base)
  parts <- list()
  for (col in cols) {
    sub <- value_diff[value_diff$column_base == col, , drop = FALSE]
    sub <- sub[order(sub$key_id), , drop = FALSE]
    nr <- nrow(sub)
    take_first <- seq_len(min(nr, n))
    take_last <- if (nr > n) seq.int(nr - min(nr, n) + 1L, nr) else integer()
    # Avoid duplicating rows when first and last overlap (small column).
    if (length(take_last) > 0L) {
      take_last <- setdiff(take_last, take_first)
    }
    if (length(take_first) > 0L) {
      first_part <- sub[take_first, , drop = FALSE]
      parts[[length(parts) + 1L]] <- tibble::tibble(
        column_base = first_part$column_base,
        column_comp = first_part$column_comp,
        position = "first",
        rank = seq_along(take_first),
        key_id = first_part$key_id,
        base_row = first_part$base_row,
        comp_row = first_part$comp_row,
        kind = first_part$kind,
        base = first_part$base,
        comp = first_part$comp,
        diff = first_part$diff,
        na_flow = first_part$na_flow,
        note = first_part$note
      )
    }
    if (length(take_last) > 0L) {
      last_part <- sub[take_last, , drop = FALSE]
      parts[[length(parts) + 1L]] <- tibble::tibble(
        column_base = last_part$column_base,
        column_comp = last_part$column_comp,
        position = "last",
        rank = seq_along(take_last),
        key_id = last_part$key_id,
        base_row = last_part$base_row,
        comp_row = last_part$comp_row,
        kind = last_part$kind,
        base = last_part$base,
        comp = last_part$comp,
        diff = last_part$diff,
        na_flow = last_part$na_flow,
        note = last_part$note
      )
    }
  }
  if (length(parts) == 0L) {
    return(empty)
  }
  vctrs::vec_rbind(!!!parts)
}

#' Unmatched rows from a comparison
#'
#' Returns the rows that are present on only one side of a comparison
#' (`"base_only"` or `"comp_only"`), with their original data columns.
#' Mirrors the "Observations in <side> only" tables produced by SAS
#' `PROC COMPARE`.
#'
#' The result is precomputed at [ks_compare()] time and capped by the
#' `max_unmatched_rows` argument (default `100`). When the cap kicks
#' in, the `truncated` attribute is set and the full counts are
#' recorded on `n_total`.
#'
#' @param x A `ks_comparison`.
#' @param side One of `"both"` (default), `"base_only"`, or
#'   `"comp_only"`.
#' @return A tibble with columns `side`, `key_id`, `key_label`,
#'   `base_row`, `comp_row`, followed by the data columns from the
#'   side that holds each row (`base_row` is `NA` for `comp_only`
#'   rows and vice versa, making it trivial to look an observation
#'   up in the original `base` / `comp` frame).
#'   Empty (zero-row) tibble when there are no unmatched rows.
#' @seealso [ks_compare()], [ks_tidy()].
#' @export
#' @examples
#' a <- data.frame(id = 1:3, x = c(1, 2, 3))
#' b <- data.frame(id = 2:4, x = c(2, 3, 4))
#' cmp <- ks_compare(a, b, by = "id")
#' ks_unmatched_rows(cmp)
ks_unmatched_rows <- function(x, side = c("both", "base_only", "comp_only")) {
  ks_assert_comparison(x)
  side <- rlang::arg_match(side)
  ur <- x$unmatched_rows %||% tibble::tibble(
    side = character(),
    key_id = integer(),
    key_label = character(),
    base_row = integer(),
    comp_row = integer()
  )
  if (side == "both" || nrow(ur) == 0L) {
    return(ur)
  }
  out <- ur[ur$side == side, , drop = FALSE]
  attr(out, "truncated") <- attr(ur, "truncated")
  attr(out, "n_total") <- attr(ur, "n_total")
  out
}
