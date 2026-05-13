#' Diff causes summary
#'
#' Aggregates the `note` column of `cmp$value_diff` into a tidy
#' taxonomy: one row per distinct cause, with counts and the columns
#' it affects. Helps answer "what kinds of discrepancies do we have?"
#' before drilling into individual cells.
#'
#' Compound notes (multiple cues joined by `"; "`) are split into
#' their elementary cues, so a single cell can contribute to several
#' causes. Cells with `note = NA` (a "plain" value change with no
#' recognised cue) are bucketed as `"plain value change"`.
#'
#' @param x A `ks_comparison`.
#' @return A tibble with columns:
#'   - `cause`: short label (e.g. `"letter case differs"`).
#'   - `n_cells`: number of cells contributing this cue.
#'   - `n_columns`: number of distinct base columns affected.
#'   - `columns`: comma-separated sample of the affected columns
#'     (capped at 5 names).
#' Sorted by `n_cells` descending. Zero-row tibble when there are no
#' value differences.
#' @seealso [ks_compare()], [ks_row_diff_summary()].
#' @export
#' @examples
#' a <- data.frame(id = 1:3, x = c("a", "b ", "c"), y = c(1, 2, 3))
#' b <- data.frame(id = 1:3, x = c("A", "b",  "c"), y = c(1, 2, 4))
#' cmp <- ks_compare(a, b, by = "id")
#' ks_cause_summary(cmp)
ks_cause_summary <- function(x) {
  ks_assert_comparison(x)
  vd <- x$value_diff
  empty <- tibble::tibble(
    cause = character(),
    n_cells = integer(),
    n_columns = integer(),
    columns = character()
  )
  if (is.null(vd) || nrow(vd) == 0L) {
    return(empty)
  }
  note <- vd$note
  col  <- vd$column_base
  # Treat NA notes as "plain value change" so they still show up.
  is_na_note <- is.na(note)
  # Vectorize: do one strsplit on the whole non-NA vector, then reassemble.
  split_notes <- strsplit(ifelse(is_na_note, "", note), ";\\s*")
  causes <- lapply(seq_along(note), function(i) {
    if (is_na_note[[i]]) return("plain value change")
    parts <- trimws(split_notes[[i]])
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0L) "plain value change" else parts
  })
  reps <- vapply(causes, length, integer(1L))
  cause_v <- unlist(causes, use.names = FALSE)
  col_v   <- rep(col, reps)
  agg <- split(col_v, cause_v)
  out <- tibble::tibble(
    cause = names(agg),
    n_cells = vapply(agg, length, integer(1L)),
    n_columns = vapply(agg, function(z) length(unique(z)), integer(1L)),
    columns = vapply(
      agg,
      function(z) {
        u <- unique(z)
        if (length(u) <= 5L) paste(u, collapse = ", ")
        else paste0(paste(utils::head(u, 5L), collapse = ", "), ", \u2026")
      },
      character(1L)
    )
  )
  out[order(-out$n_cells, out$cause), , drop = FALSE]
}

#' Per-row diff summary
#'
#' Tallies the number of cell differences per matched observation,
#' sorted descending. Useful for spotting rows where most/all columns
#' disagree (often a sign of a wrong row match, a duplicated key
#' resolved differently, or a systemic shift on a single record).
#'
#' Only matched rows are reported; unmatched rows are available via
#' [ks_unmatched_rows()].
#'
#' @param x A `ks_comparison`.
#' @param n Optional cap on the number of rows returned (default
#'   `NULL` = all). When set, rows beyond `n` are dropped.
#' @return A tibble with one row per matched observation that has at
#'   least one cell diff:
#'   - `key_id`, `base_row`, `comp_row`, `key_label`
#'   - `n_diffs`: count of differing cells on this observation.
#'   - `columns`: comma-separated sample of the affected columns
#'     (capped at 8 names).
#' Sorted by `n_diffs` descending. Zero-row tibble when there are no
#' value differences.
#' @seealso [ks_compare()], [ks_cause_summary()].
#' @export
#' @examples
#' a <- data.frame(id = 1:3, x = 1:3, y = 1:3)
#' b <- data.frame(id = 1:3, x = c(1, 9, 9), y = c(1, 9, 3))
#' cmp <- ks_compare(a, b, by = "id")
#' ks_row_diff_summary(cmp)
ks_row_diff_summary <- function(x, n = NULL) {
  ks_assert_comparison(x)
  vd <- x$value_diff
  empty <- tibble::tibble(
    key_id = integer(),
    base_row = integer(),
    comp_row = integer(),
    key_label = character(),
    n_diffs = integer(),
    columns = character()
  )
  if (is.null(vd) || nrow(vd) == 0L) {
    return(empty)
  }
  groups <- split(seq_len(nrow(vd)), vd$key_id)
  row_keys <- x$meta$row_keys
  rows <- lapply(names(groups), function(kid_str) {
    idx <- groups[[kid_str]]
    kid <- as.integer(kid_str)
    cols <- unique(vd$column_base[idx])
    cols_txt <- if (length(cols) <= 8L) {
      paste(cols, collapse = ", ")
    } else {
      paste0(paste(utils::head(cols, 8L), collapse = ", "), ", \u2026")
    }
    label <- if (!is.null(row_keys)) {
      hit <- match(kid, row_keys$key_id)
      if (is.na(hit)) NA_character_ else row_keys$key_label[[hit]]
    } else {
      NA_character_
    }
    tibble::tibble(
      key_id = kid,
      base_row = vd$base_row[idx][[1L]],
      comp_row = vd$comp_row[idx][[1L]],
      key_label = label,
      n_diffs = length(idx),
      columns = cols_txt
    )
  })
  out <- vctrs::vec_rbind(!!!rows)
  out <- out[order(-out$n_diffs, out$key_id), , drop = FALSE]
  if (!is.null(n)) {
    n <- as.integer(n)
    if (is.na(n) || n < 0L) ks_abort("{.arg n} must be a non-negative integer.")
    out <- utils::head(out, n)
  }
  out
}

#' Internal: build a one-line executive verdict for a comparison
#'
#' Returns a list with `headline` (single sentence) and `details`
#' (named character vector) for use in `print.ks_comparison()` and
#' the HTML executive summary card.
#'
#' @keywords internal
#' @noRd
ks_executive_verdict <- function(x) {
  s <- summary(x)
  n_matched <- s$n_matched_rows
  n_matched_cols <- s$n_matched_columns
  n_cells <- as.numeric(n_matched) * as.numeric(n_matched_cols)
  n_diffs <- s$n_value_diffs
  pct_match <- if (n_cells > 0) {
    100 * (n_cells - n_diffs) / n_cells
  } else {
    NA_real_
  }
  pct_txt <- if (is.na(pct_match)) {
    "n/a"
  } else if (pct_match == 100) {
    "100%"
  } else if (pct_match > 99.99) {
    ">99.99%"
  } else {
    sprintf("%.2f%%", pct_match)
  }
  health <- tryCatch(ks_match_health(x), error = function(e) NULL)
  sev <- if (is.null(health)) "ok" else health$severity
  verdict <- if (n_diffs == 0L &&
                 s$n_base_only_rows == 0L && s$n_comp_only_rows == 0L &&
                 s$n_base_only_columns == 0L && s$n_comp_only_columns == 0L) {
    "Datasets are identical on compared cells."
  } else if (n_diffs == 0L) {
    sprintf(
      "All matched cells equal; row coverage: %d / %d matched, %d base-only, %d comp-only.",
      n_matched, max(s$n_base_rows, s$n_comp_rows),
      s$n_base_only_rows, s$n_comp_only_rows
    )
  } else if (identical(sev, "critical") && !is.null(health)) {
    sprintf(
      "%s: %s of matched cells equal, but %d diff%s across %d column%s suggest the match itself is unreliable. See recommendations.",
      "Comparison may be unreliable",
      pct_txt,
      n_diffs, if (n_diffs == 1L) "" else "s",
      s$n_columns_with_diffs, if (s$n_columns_with_diffs == 1L) "" else "s"
    )
  } else {
    cs <- ks_cause_summary(x)
    top_cause <- if (nrow(cs) > 0L) cs$cause[[1L]] else NA_character_
    sprintf(
      "%s of matched cells equal; %d diff%s across %d column%s%s.",
      pct_txt,
      n_diffs, if (n_diffs == 1L) "" else "s",
      s$n_columns_with_diffs, if (s$n_columns_with_diffs == 1L) "" else "s",
      if (!is.na(top_cause)) sprintf(" (top cause: %s)", top_cause) else ""
    )
  }
  list(
    headline = verdict,
    pct_match = pct_match,
    n_cells = n_cells,
    n_diffs = n_diffs,
    severity = sev
  )
}

#' Match-quality health check
#'
#' Computes a small set of metrics that flag *suspicious* matching:
#' high diff density, many fully-different rows, duplicate keys
#' resolved positionally, and similar patterns that often indicate
#' the wrong key was used.
#'
#' @param x A `ks_comparison`.
#' @return A list with components:
#'   - `diff_density`: `n_value_diffs / (n_matched_rows *
#'     n_matched_columns)` (0-1).
#'   - `n_fully_diff_rows`: matched rows where every matched column
#'     differs (or, more permissively, >= 80% of columns differ).
#'   - `pct_fully_diff_rows`: as proportion of matched rows.
#'   - `n_value_to_na`, `n_na_to_value`: structural NA transitions.
#'   - `dup_keys`: `TRUE` if either side had duplicate key values.
#'   - `dup_positional`: `TRUE` if duplicates were paired positionally
#'     (`keep_all`) — the most error-prone path.
#'   - `row_count_delta`: `n_comp_rows - n_base_rows` (signed).
#'   - `flags`: character vector of human-readable warnings (possibly
#'     empty).
#'   - `severity`: `"ok"`, `"info"`, `"warn"`, or `"critical"`.
#' @seealso [ks_recommendations()], [ks_compare()].
#' @export
#' @examples
#' a <- data.frame(id = c(1, 1, 2), x = c(1, 2, 3))
#' b <- data.frame(id = c(1, 1, 2), x = c(9, 9, 3))
#' cmp <- ks_compare(a, b, by = "id", dup_keys = "keep_all")
#' ks_match_health(cmp)
ks_match_health <- function(x) {
  ks_assert_comparison(x)
  s <- summary(x)
  m <- x$meta$matching
  n_matched <- s$n_matched_rows
  n_cols <- s$n_matched_columns
  n_cells <- as.numeric(n_matched) * as.numeric(n_cols)
  n_diffs <- s$n_value_diffs

  diff_density <- if (n_cells > 0) n_diffs / n_cells else 0

  vd <- x$value_diff
  if (!is.null(vd) && nrow(vd) > 0L && n_cols > 0L) {
    per_row <- tabulate(match(vd$key_id, unique(vd$key_id)))
    # rows where >= 80% of matched columns differ
    threshold <- max(1L, as.integer(ceiling(0.80 * n_cols)))
    n_fully <- sum(per_row >= threshold)
    n_value_to_na <- sum(vd$na_flow == "value_to_na", na.rm = TRUE)
    n_na_to_value <- sum(vd$na_flow == "na_to_value", na.rm = TRUE)
  } else {
    n_fully <- 0L
    n_value_to_na <- 0L
    n_na_to_value <- 0L
  }
  pct_fully <- if (n_matched > 0L) n_fully / n_matched else 0

  dup_keys <- !is.null(m) &&
    ((m$n_base_dup_keys %||% 0L) > 0L ||
       (m$n_comp_dup_keys %||% 0L) > 0L)
  dup_positional <- !is.null(m) &&
    identical(m$strategy, "keyed_dup_keep_all")
  position_match <- !is.null(m) && identical(m$strategy, "position")

  delta <- (s$n_comp_rows %||% 0L) - (s$n_base_rows %||% 0L)

  flags <- character()
  if (position_match && diff_density > 0.10) {
    flags <- c(flags,
      "Position-based matching produced a high diff density; consider passing `by = ...` with key columns."
    )
  }
  if (dup_positional && diff_density > 0.05) {
    flags <- c(flags,
      "Duplicate keys were paired positionally and diff density is high \u2014 the key is likely incomplete. Try adding columns to `by`."
    )
  }
  if (!dup_positional && !position_match && diff_density > 0.25) {
    flags <- c(flags,
      sprintf("Very high diff density (%.1f%%) on matched cells \u2014 verify the key is correct.",
              100 * diff_density)
    )
  }
  if (pct_fully > 0.10 && n_fully >= 5L) {
    flags <- c(flags,
      sprintf("%d row%s (%.1f%%) differ on \u2265 80%% of matched columns \u2014 likely wrong row pairings.",
              n_fully, if (n_fully == 1L) "" else "s", 100 * pct_fully)
    )
  }
  if (abs(delta) > 0L) {
    flags <- c(flags,
      sprintf("Row counts differ: compare has %s%d row%s than base.",
              if (delta > 0L) "+" else "", delta,
              if (abs(delta) == 1L) "" else "s")
    )
  }

  severity <- if (
    (dup_positional && diff_density > 0.05) ||
    (position_match && diff_density > 0.10) ||
    diff_density > 0.25
  ) {
    "critical"
  } else if (pct_fully > 0.10 && n_fully >= 5L) {
    "warn"
  } else if (length(flags) > 0L) {
    "info"
  } else {
    "ok"
  }

  list(
    diff_density = diff_density,
    n_fully_diff_rows = n_fully,
    pct_fully_diff_rows = pct_fully,
    n_value_to_na = as.integer(n_value_to_na),
    n_na_to_value = as.integer(n_na_to_value),
    dup_keys = dup_keys,
    dup_positional = dup_positional,
    position_match = position_match,
    row_count_delta = as.integer(delta),
    flags = flags,
    severity = severity
  )
}

#' Suggest additional key columns when duplicates exist
#'
#' Scans matched columns to find candidates whose combination with the
#' current `by` key would make the join key unique on at least one
#' side (and ideally both). Returns columns ranked by how much they
#' reduce duplicate-key cardinality. Useful when [ks_compare()] used
#' `dup_keys = "keep_all"` and the result looks suspicious.
#'
#' Because `ks_compare()` does not snapshot the input data, the
#' original `base` and `comp` frames must be passed in to inspect
#' candidate columns. When `base` / `comp` are not supplied (or do
#' not contain the original columns) an empty tibble is returned.
#'
#' @param x A `ks_comparison`.
#' @param base,comp Optional: the original input data frames used to
#'   build `x`. Required to inspect candidate column uniqueness.
#' @param top_n Maximum number of candidate columns to return.
#' @return A tibble with columns `column`, `pct_unique_base`,
#'   `pct_unique_comp`, `would_make_unique`. Zero rows when no key is
#'   in use, no duplicates exist, or no improvement is possible.
#' @seealso [ks_match_health()], [ks_compare()].
#' @export
ks_suggest_key <- function(x, base = NULL, comp = NULL, top_n = 10L) {
  ks_assert_comparison(x)
  empty <- tibble::tibble(
    column = character(),
    pct_unique_base = double(),
    pct_unique_comp = double(),
    would_make_unique = logical()
  )
  m <- x$meta$matching
  if (is.null(m) || !identical(substr(m$strategy %||% "", 1, 9), "keyed_dup")) {
    return(empty)
  }
  keys <- m$keys
  if (is.null(keys) || nrow(keys) == 0L) return(empty)
  if (is.null(base) || is.null(comp)) return(empty)

  matched_cols <- x$schema_diff$base[x$schema_diff$side == "matched"]
  cand <- setdiff(matched_cols, keys$base)
  if (length(cand) == 0L) return(empty)

  pct_uniq <- function(df, key_cols, extra) {
    if (!all(c(key_cols, extra) %in% names(df))) return(NA_real_)
    sub <- df[, c(key_cols, extra), drop = FALSE]
    n <- nrow(sub)
    if (n == 0L) return(NA_real_)
    nu <- nrow(vctrs::vec_unique(sub))
    nu / n
  }

  rows <- lapply(cand, function(col) {
    pb <- pct_uniq(base, keys$base, col)
    pc <- pct_uniq(comp, keys$comp, col)
    tibble::tibble(
      column = col,
      pct_unique_base = pb,
      pct_unique_comp = pc,
      would_make_unique = isTRUE(pb >= 1) && isTRUE(pc >= 1)
    )
  })
  out <- vctrs::vec_rbind(!!!rows)
  out <- out[!(is.na(out$pct_unique_base) & is.na(out$pct_unique_comp)), , drop = FALSE]
  if (nrow(out) == 0L) return(empty)
  ord <- order(-as.integer(out$would_make_unique),
               -pmin(out$pct_unique_base, out$pct_unique_comp, na.rm = FALSE))
  out <- out[ord, , drop = FALSE]
  utils::head(out, top_n)
}

#' Actionable recommendations for a comparison
#'
#' Synthesises [ks_match_health()] flags, [ks_suggest_key()] hints,
#' and a few other heuristics into a short list of user-facing
#' "what to look at first" recommendations.
#'
#' @param x A `ks_comparison`.
#' @return A tibble with one row per recommendation:
#'   - `severity` (`"critical"`, `"warn"`, `"info"`, `"ok"`)
#'   - `title` short label
#'   - `message` longer human-readable text
#'   - `action` suggested next step (may be `NA`)
#' @seealso [ks_match_health()], [ks_suggest_key()].
#' @export
ks_recommendations <- function(x) {
  ks_assert_comparison(x)
  h <- ks_match_health(x)
  recs <- list()

  add <- function(severity, title, message, action = NA_character_) {
    recs[[length(recs) + 1L]] <<- tibble::tibble(
      severity = severity, title = title,
      message = message, action = action
    )
  }

  if (h$dup_positional && h$diff_density > 0.05) {
    add("critical", "Likely wrong or incomplete key",
        sprintf("Duplicate keys (%d on base, %d on comp) were paired positionally; %.1f%% of matched cells differ. The key column(s) are likely incomplete.",
                x$meta$matching$n_base_dup_keys %||% 0L,
                x$meta$matching$n_comp_dup_keys %||% 0L,
                100 * h$diff_density),
        sprintf("Add columns that make `by` unique within each %s group, e.g. `ks_suggest_key(cmp, base, comp)`.",
                paste(x$meta$matching$keys$base, collapse = " + ")))
  } else if (h$position_match && h$diff_density > 0.10) {
    add("critical", "No key columns",
        sprintf("Rows were zipped by position (no `by`); %.1f%% of cells differ.", 100 * h$diff_density),
        "Pass `by = c(\"<id_col>\")` so rows align by identity, not position.")
  } else if (h$diff_density > 0.25) {
    add("warn", "Very high diff density",
        sprintf("%.1f%% of matched cells differ \u2014 unusual for a clean comparison.", 100 * h$diff_density),
        "Verify the key columns and that you compared the right snapshots.")
  }

  if (h$n_fully_diff_rows >= 5L && h$pct_fully_diff_rows > 0.05) {
    add("warn", "Rows that differ on almost every column",
        sprintf("%d matched row%s differ on \u2265 80%% of columns. Often a wrong-row-pairing symptom.",
                h$n_fully_diff_rows,
                if (h$n_fully_diff_rows == 1L) "" else "s"),
        "Open the \"Most-affected rows\" section to inspect.")
  }

  if (h$row_count_delta != 0L) {
    add("info", "Row counts differ",
        sprintf("Compare has %s%d row%s than base.",
                if (h$row_count_delta > 0L) "+" else "", h$row_count_delta,
                if (abs(h$row_count_delta) == 1L) "" else "s"),
        "Open the \"Unmatched rows\" section for the diff.")
  }

  if (h$n_value_to_na + h$n_na_to_value > 0L) {
    add("info", "NA transitions",
        sprintf("%d value\u2192NA and %d NA\u2192value transitions detected.",
                h$n_value_to_na, h$n_na_to_value),
        "Look for the \u2192NA / \u2190NA chips in the Note column of the value-diff table, or filter `na_flow` on `as_tibble(cmp)`.")
  }

  ko <- x$key_diff
  if (!is.null(ko) && nrow(ko) > 0L) {
    add("info", "Key duplications",
        sprintf("%d duplicated key value%s reported.", nrow(ko),
                if (nrow(ko) == 1L) "" else "s"),
        "Review the \"Row matching\" section.")
  }

  if (length(recs) == 0L) {
    return(tibble::tibble(
      severity = character(), title = character(),
      message = character(), action = character()
    ))
  }
  out <- vctrs::vec_rbind(!!!recs)
  sev_rank <- c(critical = 0L, warn = 1L, info = 2L, ok = 3L)
  out <- out[order(sev_rank[out$severity]), , drop = FALSE]
  out
}
