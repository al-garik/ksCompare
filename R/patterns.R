#' Internal: detect patterns in the long-format value-diff table
#'
#' For each column with at least one diff, run a small set of cheap
#' detectors and return ranked "explanations". Result columns:
#'
#' - `column`: the base-side column name.
#' - `pattern`: one of `"constant_offset"`, `"constant_scale"`,
#'   `"sign_flip"`, `"integer_round"`, `"whitespace_only"`, `"case_only"`,
#'   `"trim_only"`, `"factor_recoded"`.
#' - `coverage`: fraction of diffs in the column explained by this pattern.
#' - `detail`: numeric (offset / scale) or character description.
#'
#' Patterns are ordered by descending coverage; only patterns with
#' coverage \eqn{\\ge} `min_coverage` (default `0.5`) are returned.
#'
#' @keywords internal
#' @noRd
ks_detect_patterns <- function(value_diff, min_coverage = 0.5) {
  empty <- tibble::tibble(
    column = character(),
    pattern = character(),
    coverage = numeric(),
    detail = character()
  )
  if (nrow(value_diff) == 0L) {
    return(empty)
  }
  cols <- unique(value_diff$column_base)
  parts <- list()
  for (col in cols) {
    sub <- value_diff[value_diff$column_base == col, , drop = FALSE]
    parts[[length(parts) + 1L]] <- ks_detect_patterns_one(col, sub)
  }
  out <- vctrs::vec_rbind(!!!parts)
  if (nrow(out) == 0L) {
    return(empty)
  }
  out <- out[out$coverage >= min_coverage, , drop = FALSE]
  out <- out[order(-out$coverage), , drop = FALSE]
  out
}

ks_detect_patterns_one <- function(col, sub) {
  kind <- sub$kind[[1L]]
  rows <- list()

  if (kind %in% c("integer", "double")) {
    b <- suppressWarnings(as.numeric(sub$base))
    c <- suppressWarnings(as.numeric(sub$comp))
    ok <- is.finite(b) & is.finite(c)
    if (any(ok)) {
      d <- b[ok] - c[ok]
      # constant offset
      if (length(unique(round(d, 12))) == 1L) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          column = col,
          pattern = "constant_offset",
          coverage = mean(ok),
          detail = format(d[1L], trim = TRUE)
        )
      }
      # constant scale (multiplicative)
      ok2 <- ok & b != 0 & c != 0
      if (any(ok2)) {
        ratio <- b[ok2[ok]] / c[ok2[ok]]
        if (length(unique(round(ratio, 9))) == 1L) {
          rows[[length(rows) + 1L]] <- tibble::tibble(
            column = col,
            pattern = "constant_scale",
            coverage = mean(ok2),
            detail = format(ratio[1L], trim = TRUE)
          )
        }
      }
      # sign flip: b == -c
      sf <- ok & abs(b + c) < 1e-12 & abs(b) > 0
      if (any(sf) && mean(sf) >= 0.5) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          column = col,
          pattern = "sign_flip",
          coverage = mean(sf),
          detail = NA_character_
        )
      }
      # integer rounding: one side equals round(other)
      ir <- ok & (abs(b - round(c)) < 1e-12 | abs(c - round(b)) < 1e-12)
      if (any(ir) && mean(ir) >= 0.5) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          column = col,
          pattern = "integer_round",
          coverage = mean(ir),
          detail = NA_character_
        )
      }
    }
  } else if (kind %in% c("character", "factor", "labelled")) {
    b <- as.character(sub$base)
    c <- as.character(sub$comp)
    diff <- !is.na(b) & !is.na(c) & b != c
    # trim-only (leading/trailing whitespace differs but trimmed strings match)
    tr <- diff &
      stringi::stri_trim_both(b) == stringi::stri_trim_both(c)
    tr[is.na(tr)] <- FALSE
    if (any(tr)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        column = col,
        pattern = "trim_only",
        coverage = mean(tr),
        detail = NA_character_
      )
    }
    # whitespace-only differences (collapsing all whitespace), excluding pure trim
    ws <- diff &
      !tr &
      gsub("\\s+", "", b) == gsub("\\s+", "", c)
    ws[is.na(ws)] <- FALSE
    if (any(ws)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        column = col,
        pattern = "whitespace_only",
        coverage = mean(ws),
        detail = NA_character_
      )
    }
    # case-only
    cs <- diff &
      !tr &
      !ws &
      stringi::stri_trans_tolower(b) == stringi::stri_trans_tolower(c)
    cs[is.na(cs)] <- FALSE
    if (any(cs)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        column = col,
        pattern = "case_only",
        coverage = mean(cs),
        detail = NA_character_
      )
    }
    if (kind == "factor") {
      # If base and comp use the same level set with consistent recoding
      bc <- paste(b, "->", c)
      consistent <- length(unique(bc)) <= length(unique(b))
      if (consistent) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          column = col,
          pattern = "factor_recoded",
          coverage = 1,
          detail = paste(unique(bc), collapse = "; ")
        )
      }
    }
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(
      column = character(),
      pattern = character(),
      coverage = numeric(),
      detail = character()
    ))
  }
  vctrs::vec_rbind(!!!rows)
}
