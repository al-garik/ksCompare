#' Internal: match rows between base and compare frames
#'
#' Returns a list with two elements:
#' - `row_diff`: tibble with `key_id`, `base_row`, `comp_row`, `status`
#'   (`"matched"` / `"base_only"` / `"comp_only"`).
#' - `matching`: a list describing the strategy actually used
#'   (`strategy`, `keys`, `dup_strategy`, `dup_summary`,
#'   `n_dropped_base`, `n_dropped_comp`, `n_pairs_created`,
#'   `n_base_dup_keys`, `n_comp_dup_keys`).
#'
#' Strategies:
#' - `"position"`: when `keys` is empty, zips by row index.
#' - `"keyed_unique"`: when keys are present and unique on both sides.
#' - `"keyed_dup_<dup_strategy>"`: keys present but duplicates resolved
#'   by `dup_keys` (`first`, `last`, `keep_all`, `all_pairs`).
#'
#' @keywords internal
#' @noRd
ks_match_rows <- function(base, comp, keys, dup_keys = "first") {
  if (nrow(keys) == 0L) {
    rd <- ks_match_rows_position(base, comp)
    matching <- list(
      strategy = "position",
      keys = keys,
      dup_strategy = NA_character_,
      dup_summary = ks_empty_dup_summary(),
      n_dropped_base = 0L,
      n_dropped_comp = 0L,
      n_pairs_created = 0L,
      n_base_dup_keys = 0L,
      n_comp_dup_keys = 0L
    )
    return(list(row_diff = rd, matching = matching))
  }
  ks_match_rows_keyed(base, comp, keys, dup_keys = dup_keys)
}

ks_empty_dup_summary <- function() {
  tibble::tibble(
    key = character(),
    n_base = integer(),
    n_comp = integer(),
    action = character()
  )
}

ks_match_rows_position <- function(base, comp) {
  nb <- nrow(base)
  nc <- nrow(comp)
  n <- max(nb, nc)
  if (n == 0L) {
    return(tibble::tibble(
      key_id = integer(),
      base_row = integer(),
      comp_row = integer(),
      status = character()
    ))
  }
  base_row <- ifelse(seq_len(n) <= nb, seq_len(n), NA_integer_)
  comp_row <- ifelse(seq_len(n) <= nc, seq_len(n), NA_integer_)
  status <- dplyr::case_when(
    !is.na(base_row) & !is.na(comp_row) ~ "matched",
    is.na(comp_row) ~ "base_only",
    TRUE ~ "comp_only"
  )
  tibble::tibble(
    key_id = seq_len(n),
    base_row = base_row,
    comp_row = comp_row,
    status = status
  )
}

ks_match_rows_keyed <- function(base, comp, keys, dup_keys = "first") {
  bkey <- base[, keys$base, drop = FALSE]
  ckey <- comp[, keys$comp, drop = FALSE]
  # Ensure both key tables have the same column names so vctrs can compare.
  names(bkey) <- paste0("k", seq_along(bkey))
  names(ckey) <- paste0("k", seq_along(ckey))

  uniq_check <- ks_check_key_unique(base, comp, keys)
  if (!uniq_check$base_unique || !uniq_check$comp_unique) {
    return(ks_match_rows_dup(bkey, ckey, dup_keys, uniq_check))
  }

  # Both sides unique on key — straight outer join via vctrs.
  union_keys <- vctrs::vec_unique(vctrs::vec_rbind(bkey, ckey))
  base_row <- vctrs::vec_match(union_keys, bkey)
  comp_row <- vctrs::vec_match(union_keys, ckey)

  status <- dplyr::case_when(
    !is.na(base_row) & !is.na(comp_row) ~ "matched",
    is.na(comp_row) ~ "base_only",
    TRUE ~ "comp_only"
  )
  rd <- tibble::tibble(
    key_id = seq_len(nrow(union_keys)),
    base_row = base_row,
    comp_row = comp_row,
    status = status
  )
  matching <- list(
    strategy = "keyed_unique",
    keys = keys,
    dup_strategy = NA_character_,
    dup_summary = ks_empty_dup_summary(),
    n_dropped_base = 0L,
    n_dropped_comp = 0L,
    n_pairs_created = 0L,
    n_base_dup_keys = 0L,
    n_comp_dup_keys = 0L
  )
  list(row_diff = rd, matching = matching)
}

#' Internal: handle non-unique keys per `dup_keys` strategy
#'
#' Strategies:
#' - `"error"`: raise.
#' - `"first"` / `"last"`: collapse duplicates on each side by keeping the
#'   first / last occurrence (per side independently).
#' - `"keep_all"`: pair duplicates positionally within each key group
#'   (1\u21941, 2\u21942, \u2026); pad shorter side with NA.
#' - `"all_pairs"`: cartesian-pair all base rows with all comp rows that
#'   share a key value, plus base-only / comp-only rows for unmatched
#'   keys.
#'
#' @keywords internal
#' @noRd
ks_match_rows_dup <- function(bkey, ckey, dup_keys, uniq_check) {
  if (identical(dup_keys, "error")) {
    bad_side <- c(
      if (!uniq_check$base_unique) "base",
      if (!uniq_check$comp_unique) "comp"
    )
    ks_abort(
      c(
        "Duplicate key values detected in {.field {bad_side}}.",
        "i" = "Pass {.arg dup_keys} (e.g. {.val first}, {.val last}, {.val keep_all}, {.val all_pairs}) to choose a strategy."
      ),
      class = "ksCompare_duplicate_key"
    )
  }

  n_base_dup_keys <- nrow(uniq_check$base_dups)
  n_comp_dup_keys <- nrow(uniq_check$comp_dups)
  dup_summary <- ks_build_dup_summary(uniq_check, bkey, ckey)

  if (dup_keys %in% c("first", "last")) {
    bidx <- ks_collapse_dup(bkey, dup_keys)
    cidx <- ks_collapse_dup(ckey, dup_keys)
    n_dropped_base <- nrow(bkey) - length(bidx)
    n_dropped_comp <- nrow(ckey) - length(cidx)
    bkey2 <- bkey[bidx, , drop = FALSE]
    ckey2 <- ckey[cidx, , drop = FALSE]
    union_keys <- vctrs::vec_unique(vctrs::vec_rbind(bkey2, ckey2))
    bm <- vctrs::vec_match(union_keys, bkey2)
    cm <- vctrs::vec_match(union_keys, ckey2)
    base_row <- ifelse(is.na(bm), NA_integer_, bidx[bm])
    comp_row <- ifelse(is.na(cm), NA_integer_, cidx[cm])
    status <- dplyr::case_when(
      !is.na(base_row) & !is.na(comp_row) ~ "matched",
      is.na(comp_row) ~ "base_only",
      TRUE ~ "comp_only"
    )
    rd <- tibble::tibble(
      key_id = seq_len(nrow(union_keys)),
      base_row = base_row,
      comp_row = comp_row,
      status = status
    )
    ks_inform_dup(dup_keys, n_base_dup_keys, n_comp_dup_keys,
                  n_dropped_base = n_dropped_base,
                  n_dropped_comp = n_dropped_comp)
    return(list(
      row_diff = rd,
      matching = list(
        strategy = paste0("keyed_dup_", dup_keys),
        keys = NULL,  # filled by caller
        dup_strategy = dup_keys,
        dup_summary = dup_summary,
        n_dropped_base = n_dropped_base,
        n_dropped_comp = n_dropped_comp,
        n_pairs_created = 0L,
        n_base_dup_keys = n_base_dup_keys,
        n_comp_dup_keys = n_comp_dup_keys
      )
    ))
  }

  if (identical(dup_keys, "keep_all")) {
    rd <- ks_match_rows_keep_all(bkey, ckey)
    n_pairs_created <- sum(rd$status == "matched")
    ks_inform_dup(dup_keys, n_base_dup_keys, n_comp_dup_keys,
                  n_pairs_created = n_pairs_created)
    return(list(
      row_diff = rd,
      matching = list(
        strategy = "keyed_dup_keep_all",
        keys = NULL,
        dup_strategy = "keep_all",
        dup_summary = dup_summary,
        n_dropped_base = 0L,
        n_dropped_comp = 0L,
        n_pairs_created = n_pairs_created,
        n_base_dup_keys = n_base_dup_keys,
        n_comp_dup_keys = n_comp_dup_keys
      )
    ))
  }

  if (identical(dup_keys, "all_pairs")) {
    rd <- ks_match_rows_all_pairs(bkey, ckey)
    n_pairs_created <- sum(rd$status == "matched")
    # Cardinality mismatch warning: keys where n_base != n_comp.
    mismatched <- dup_summary[
      dup_summary$n_base > 0L &
        dup_summary$n_comp > 0L &
        dup_summary$n_base != dup_summary$n_comp,
      ,
      drop = FALSE
    ]
    if (nrow(mismatched) > 0L) {
      ks_warn(
        c(
          "{.arg dup_keys} = {.val all_pairs}: cardinality mismatch on {nrow(mismatched)} key{?s}.",
          "i" = "Cartesian pairing inflates row counts; consider {.val keep_all} for positional within-group pairing."
        ),
        class = "ksCompare_all_pairs_cardinality"
      )
    }
    ks_inform_dup(dup_keys, n_base_dup_keys, n_comp_dup_keys,
                  n_pairs_created = n_pairs_created)
    return(list(
      row_diff = rd,
      matching = list(
        strategy = "keyed_dup_all_pairs",
        keys = NULL,
        dup_strategy = "all_pairs",
        dup_summary = dup_summary,
        n_dropped_base = 0L,
        n_dropped_comp = 0L,
        n_pairs_created = n_pairs_created,
        n_base_dup_keys = n_base_dup_keys,
        n_comp_dup_keys = n_comp_dup_keys
      )
    ))
  }

  ks_abort(
    "{.arg dup_keys} = {.val {dup_keys}} is not a recognised strategy.",
    class = "ksCompare_unknown_dup_strategy"
  )
}

# Build a per-key duplicate summary (key value, n_base, n_comp, action).
ks_build_dup_summary <- function(uniq_check, bkey, ckey) {
  if (uniq_check$base_unique && uniq_check$comp_unique) {
    return(ks_empty_dup_summary())
  }
  # Walk every distinct key value that appears on either side and count how
  # many times it occurs in base vs. comp. Keep only those duplicated on at
  # least one side.
  union_keys <- vctrs::vec_unique(vctrs::vec_rbind(bkey, ckey))
  bm <- vctrs::vec_match(bkey, union_keys)
  cm <- vctrs::vec_match(ckey, union_keys)
  n_base <- tabulate(bm, nbins = nrow(union_keys))
  n_comp <- tabulate(cm, nbins = nrow(union_keys))
  keep <- (n_base > 1L) | (n_comp > 1L)
  if (!any(keep)) return(ks_empty_dup_summary())
  uk <- union_keys[keep, , drop = FALSE]
  n_base <- n_base[keep]
  n_comp <- n_comp[keep]

  # Build a human-readable label per key row by joining its column values
  # with " | ". Columns of `uk` are always atomic (we renamed and selected
  # them in the caller).
  key_label <- vapply(seq_len(nrow(uk)), function(i) {
    parts <- vapply(seq_along(uk), function(j) {
      v <- uk[[j]][i]
      if (is.na(v)) "<NA>" else format(v, trim = TRUE)
    }, character(1L))
    paste(parts, collapse = " | ")
  }, character(1L))

  tibble::tibble(
    key = key_label,
    n_base = as.integer(n_base),
    n_comp = as.integer(n_comp),
    action = NA_character_
  )
}

ks_inform_dup <- function(strategy,
                          n_base_dup_keys,
                          n_comp_dup_keys,
                          n_dropped_base = 0L,
                          n_dropped_comp = 0L,
                          n_pairs_created = 0L) {
  msg <- switch(
    strategy,
    first = sprintf(
      "Duplicate keys detected (base: %d, comp: %d). Strategy 'first' kept the first occurrence per side; dropped %d base and %d comp row(s).",
      n_base_dup_keys, n_comp_dup_keys, n_dropped_base, n_dropped_comp
    ),
    last = sprintf(
      "Duplicate keys detected (base: %d, comp: %d). Strategy 'last' kept the last occurrence per side; dropped %d base and %d comp row(s).",
      n_base_dup_keys, n_comp_dup_keys, n_dropped_base, n_dropped_comp
    ),
    keep_all = sprintf(
      "Duplicate keys detected (base: %d, comp: %d). Strategy 'keep_all' paired rows positionally within each key group, producing %d matched pair(s).",
      n_base_dup_keys, n_comp_dup_keys, n_pairs_created
    ),
    all_pairs = sprintf(
      "Duplicate keys detected (base: %d, comp: %d). Strategy 'all_pairs' generated %d cartesian pair(s).",
      n_base_dup_keys, n_comp_dup_keys, n_pairs_created
    ),
    return(invisible(NULL))
  )
  ks_inform(msg, class = "ksCompare_dup_keys_resolved")
}

ks_collapse_dup <- function(key_df, which = c("first", "last")) {
  which <- match.arg(which)
  ids <- vctrs::vec_group_id(key_df)
  if (which == "first") {
    keep <- !duplicated(ids)
  } else {
    keep <- !duplicated(ids, fromLast = TRUE)
  }
  which(keep)
}

# Pair duplicates positionally within each key group. For each unique key:
# - matched = min(n_base, n_comp) pairs (1<->1, 2<->2, ...)
# - base_only = leftover base rows (if n_base > n_comp)
# - comp_only = leftover comp rows (if n_comp > n_base)
# - if key only on one side: that whole group becomes base_only / comp_only.
ks_match_rows_keep_all <- function(bkey, ckey) {
  union_keys <- vctrs::vec_unique(vctrs::vec_rbind(bkey, ckey))
  if (nrow(union_keys) == 0L) {
    return(tibble::tibble(
      key_id = integer(),
      base_row = integer(),
      comp_row = integer(),
      status = character()
    ))
  }
  bgrp <- vctrs::vec_match(bkey, union_keys)
  cgrp <- vctrs::vec_match(ckey, union_keys)
  parts <- list()
  next_id <- 1L
  for (i in seq_len(nrow(union_keys))) {
    bm <- which(bgrp == i)
    cm <- which(cgrp == i)
    nb <- length(bm); nc <- length(cm); n_pair <- min(nb, nc)
    if (n_pair > 0L) {
      parts[[length(parts) + 1L]] <- tibble::tibble(
        key_id = seq.int(next_id, length.out = n_pair),
        base_row = bm[seq_len(n_pair)],
        comp_row = cm[seq_len(n_pair)],
        status = rep_len("matched", n_pair)
      )
      next_id <- next_id + n_pair
    }
    if (nb > n_pair) {
      n_extra <- nb - n_pair
      parts[[length(parts) + 1L]] <- tibble::tibble(
        key_id = seq.int(next_id, length.out = n_extra),
        base_row = bm[(n_pair + 1L):nb],
        comp_row = rep(NA_integer_, n_extra),
        status = rep_len("base_only", n_extra)
      )
      next_id <- next_id + n_extra
    }
    if (nc > n_pair) {
      n_extra <- nc - n_pair
      parts[[length(parts) + 1L]] <- tibble::tibble(
        key_id = seq.int(next_id, length.out = n_extra),
        base_row = rep(NA_integer_, n_extra),
        comp_row = cm[(n_pair + 1L):nc],
        status = rep_len("comp_only", n_extra)
      )
      next_id <- next_id + n_extra
    }
  }
  if (length(parts) == 0L) {
    return(tibble::tibble(
      key_id = integer(),
      base_row = integer(),
      comp_row = integer(),
      status = character()
    ))
  }
  vctrs::vec_rbind(!!!parts)
}

ks_match_rows_all_pairs <- function(bkey, ckey) {
  union_keys <- vctrs::vec_unique(vctrs::vec_rbind(bkey, ckey))
  if (nrow(union_keys) == 0L) {
    return(tibble::tibble(
      key_id = integer(),
      base_row = integer(),
      comp_row = integer(),
      status = character()
    ))
  }
  bgrp <- vctrs::vec_match(bkey, union_keys)
  cgrp <- vctrs::vec_match(ckey, union_keys)
  parts <- list()
  next_id <- 1L
  for (i in seq_len(nrow(union_keys))) {
    bm <- which(bgrp == i)
    cm <- which(cgrp == i)
    if (length(bm) > 0L && length(cm) > 0L) {
      grid <- expand.grid(b = bm, c = cm, KEEP.OUT.ATTRS = FALSE)
      n <- nrow(grid)
      parts[[length(parts) + 1L]] <- tibble::tibble(
        key_id = seq.int(next_id, length.out = n),
        base_row = grid$b,
        comp_row = grid$c,
        status = rep_len("matched", n)
      )
      next_id <- next_id + n
    } else if (length(bm) > 0L) {
      n <- length(bm)
      parts[[length(parts) + 1L]] <- tibble::tibble(
        key_id = seq.int(next_id, length.out = n),
        base_row = bm,
        comp_row = rep(NA_integer_, n),
        status = rep_len("base_only", n)
      )
      next_id <- next_id + n
    } else {
      n <- length(cm)
      parts[[length(parts) + 1L]] <- tibble::tibble(
        key_id = seq.int(next_id, length.out = n),
        base_row = rep(NA_integer_, n),
        comp_row = cm,
        status = rep_len("comp_only", n)
      )
      next_id <- next_id + n
    }
  }
  if (length(parts) == 0L) {
    return(tibble::tibble(
      key_id = integer(),
      base_row = integer(),
      comp_row = integer(),
      status = character()
    ))
  }
  vctrs::vec_rbind(!!!parts)
}
