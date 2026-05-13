#' Internal: validate the explicit `by =` argument
#'
#' Accepts either an unnamed character vector (same column name on both
#' sides) or a named character vector (`names = base`, `values = comp`).
#' Returns a tibble with columns `base` and `comp`.
#'
#' `by = "auto"` triggers `ks_infer_key()`. If no shared key is
#' jointly unique on both sides, falls back to position matching with a
#' warning so the user is never silently surprised.
#'
#' @keywords internal
#' @noRd
ks_resolve_by <- function(base, comp, by) {
  if (is.null(by)) {
    return(tibble::tibble(base = character(), comp = character()))
  }
  if (identical(by, "auto")) {
    inferred <- ks_infer_key(base, comp)
    if (is.null(inferred)) {
      ks_warn(
        c(
          "Could not infer a unique key from shared columns.",
          "i" = "Falling back to row-position match. Pass {.arg by} explicitly to override."
        ),
        class = "ksCompare_auto_key_failed"
      )
      return(tibble::tibble(base = character(), comp = character()))
    }
    ks_inform(
      "Inferred key: {.val {inferred$base}}.",
      class = "ksCompare_auto_key_inferred"
    )
    return(inferred)
  }
  if (!is.character(by) || length(by) == 0L) {
    ks_abort("{.arg by} must be a non-empty character vector or {.code NULL}.")
  }

  if (is.null(names(by))) {
    base_keys <- by
    comp_keys <- by
  } else {
    base_keys <- ifelse(names(by) == "", unname(by), names(by))
    comp_keys <- unname(by)
  }

  bad_b <- setdiff(base_keys, names(base))
  bad_c <- setdiff(comp_keys, names(comp))
  if (length(bad_b) > 0L) {
    ks_abort(
      "Key column{?s} {.val {bad_b}} not found in {.field base}."
    )
  }
  if (length(bad_c) > 0L) {
    ks_abort(
      "Key column{?s} {.val {bad_c}} not found in {.field comp}."
    )
  }

  tibble::tibble(base = base_keys, comp = comp_keys)
}

#' Internal: check uniqueness of the key in each frame
#'
#' Returns a list with `base_unique`, `comp_unique`, `base_dups`, `comp_dups`
#' (data frames of duplicated key rows with their counts).
#'
#' @keywords internal
#' @noRd
ks_check_key_unique <- function(base, comp, keys) {
  if (nrow(keys) == 0L) {
    return(list(
      base_unique = TRUE,
      comp_unique = TRUE,
      base_dups = tibble::tibble(),
      comp_dups = tibble::tibble()
    ))
  }
  bsel <- base[, keys$base, drop = FALSE]
  csel <- comp[, keys$comp, drop = FALSE]
  bcount <- vctrs::vec_count(bsel, sort = "count")
  ccount <- vctrs::vec_count(csel, sort = "count")
  bdup <- bcount[bcount$count > 1L, , drop = FALSE]
  cdup <- ccount[ccount$count > 1L, , drop = FALSE]
  list(
    base_unique = nrow(bdup) == 0L,
    comp_unique = nrow(cdup) == 0L,
    base_dups = bdup,
    comp_dups = cdup
  )
}

#' Internal: build a per-`key_id` lookup of user-visible key labels
#'
#' Returns a tibble with one row per `key_id` from `row_diff`:
#' - `key_id`
#' - `key_label`: the key tuple rendered as a string (composite keys joined
#'   with " | "). For position match, `NA_character_`.
#' - `pair_rank`: integer rank within the key_label group (1-based), useful
#'   for `keep_all` / `all_pairs` to disambiguate duplicates.
#' - `pair_total`: total number of `key_id`s sharing this label.
#'
#' @keywords internal
#' @noRd
ks_build_row_keys <- function(row_diff, base, comp, keys) {
  if (nrow(row_diff) == 0L) {
    return(tibble::tibble(
      key_id = integer(),
      key_label = character(),
      pair_rank = integer(),
      pair_total = integer()
    ))
  }
  if (nrow(keys) == 0L) {
    # Position match: no meaningful key label.
    n <- nrow(row_diff)
    return(tibble::tibble(
      key_id = row_diff$key_id,
      key_label = rep(NA_character_, n),
      pair_rank = rep(1L, n),
      pair_total = rep(1L, n)
    ))
  }
  # For each row in row_diff, take the key tuple from base if base_row is
  # present, else from comp.
  bsel <- base[, keys$base, drop = FALSE]
  csel <- comp[, keys$comp, drop = FALSE]
  names(bsel) <- paste0("k", seq_along(bsel))
  names(csel) <- paste0("k", seq_along(csel))

  n <- nrow(row_diff)
  # Pull key values per row, filling from whichever side is non-NA.
  key_vals <- vector("list", length(keys$base))
  for (j in seq_along(keys$base)) {
    bcol <- bsel[[j]]
    ccol <- csel[[j]]
    out <- vctrs::vec_init(bcol, n = n)
    has_b <- !is.na(row_diff$base_row)
    has_c <- !is.na(row_diff$comp_row)
    if (any(has_b)) {
      vctrs::vec_slice(out, has_b) <- vctrs::vec_slice(bcol, row_diff$base_row[has_b])
    }
    if (any(has_c & !has_b)) {
      idx <- has_c & !has_b
      vctrs::vec_slice(out, idx) <- vctrs::vec_slice(ccol, row_diff$comp_row[idx])
    }
    key_vals[[j]] <- out
  }
  # Render labels: "name = value" per key column, joined with " | " for
  # composite keys. Vectorize: format() each key column once, then paste.
  formatted <- vector("list", length(keys$base))
  for (j in seq_along(keys$base)) {
    v <- key_vals[[j]]
    fv <- format(v, trim = TRUE)
    fv[is.na(v)] <- "<NA>"
    formatted[[j]] <- paste0(keys$base[[j]], " = ", fv)
  }
  labels <- if (length(formatted) == 1L) {
    formatted[[1L]]
  } else {
    do.call(paste, c(formatted, sep = " | "))
  }

  # Compute pair_rank / pair_total within key_label groups, preserving the
  # original row_diff order within each group.
  rank_in_group <- integer(n)
  group_total <- integer(n)
  splits <- split(seq_len(n), labels)
  for (g in splits) {
    rank_in_group[g] <- seq_along(g)
    group_total[g] <- length(g)
  }

  tibble::tibble(
    key_id = row_diff$key_id,
    key_label = labels,
    pair_rank = as.integer(rank_in_group),
    pair_total = as.integer(group_total)
  )
}
