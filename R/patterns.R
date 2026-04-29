#' Internal: detect patterns in the long-format value-diff table
#'
#' For each column with at least one diff, run a small set of cheap
#' detectors and return ranked "explanations". A handful of cross-column
#' detectors run after the per-column ones and are emitted with
#' `column = NA_character_` to flag them as global.
#'
#' Result columns:
#'
#' - `column`: the base-side column name (or `NA` for cross-column patterns).
#' - `pattern`: pattern label (see `ks_pattern_glossary()`).
#' - `coverage`: fraction of diffs explained by this pattern.
#' - `detail`: numeric (offset / scale) or character description.
#'
#' Patterns are ordered by descending coverage; only patterns with
#' coverage \eqn{\\ge} `min_coverage` (default `0.5`) are returned.
#' Cross-column patterns bypass the coverage filter.
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
  if (nrow(out) > 0L) {
    out <- out[out$coverage >= min_coverage, , drop = FALSE]
  }
  cross <- ks_detect_patterns_cross(value_diff)
  out <- vctrs::vec_rbind(out, cross)
  if (nrow(out) == 0L) {
    return(empty)
  }
  is_cross <- is.na(out$column)
  per <- out[!is_cross, , drop = FALSE]
  per <- per[order(-per$coverage), , drop = FALSE]
  vctrs::vec_rbind(per, out[is_cross, , drop = FALSE])
}

# ---- helpers ---------------------------------------------------------

ks_pat_row <- function(column, pattern, coverage, detail = NA_character_) {
  tibble::tibble(
    column = column,
    pattern = pattern,
    coverage = coverage,
    detail = detail
  )
}

# Common SAS / Excel / Unix epoch deltas (in days).
.ks_epoch_days <- c(
  sas_to_unix    = 3653,
  excel1900_unix = 25569,
  excel1904_unix = 24107
)

# Recognised unit conversion factors. Tolerance is relative (1e-6).
.ks_unit_scales <- list(
  list(ratio = 1000,         label = "x1000 (e.g. k -> base unit)"),
  list(ratio = 1 / 1000,     label = "x0.001 (e.g. base unit -> k)"),
  list(ratio = 60,           label = "x60 (e.g. minutes -> seconds)"),
  list(ratio = 1 / 60,       label = "x1/60 (e.g. seconds -> minutes)"),
  list(ratio = 3600,         label = "x3600 (hours -> seconds)"),
  list(ratio = 1 / 3600,     label = "x1/3600 (seconds -> hours)"),
  list(ratio = 1.609344,     label = "x1.609344 (mi -> km)"),
  list(ratio = 1 / 1.609344, label = "x0.621371 (km -> mi)"),
  list(ratio = 2.20462,      label = "x2.20462 (kg -> lb)"),
  list(ratio = 0.453592,     label = "x0.453592 (lb -> kg)"),
  list(ratio = 3.28084,      label = "x3.28084 (m -> ft)"),
  list(ratio = 0.3048,       label = "x0.3048 (ft -> m)"),
  list(ratio = 2.54,         label = "x2.54 (in -> cm)"),
  list(ratio = 1 / 2.54,     label = "x0.3937 (cm -> in)")
)

# Common NA sentinels in legacy data
.ks_na_sentinels <- c(
  -999, -9999, -99999, -999999,
  9999, 99999, 999999,
  99999997, 99999998, 99999999
)

isTRUE_vec  <- function(x) !is.na(x) & x
isFALSE_vec <- function(x) !is.na(x) & !x

# ---- per-column dispatch --------------------------------------------

ks_detect_patterns_one <- function(col, sub) {
  kind <- sub$kind[[1L]]
  rows <- list()

  if (kind %in% c("integer", "double")) {
    rows <- c(rows, ks_pat_numeric(col, sub))
  } else if (kind == "logical") {
    rows <- c(rows, ks_pat_logical(col, sub))
  } else if (kind %in% c("date", "datetime")) {
    rows <- c(rows, ks_pat_temporal(col, sub, kind))
  } else if (kind %in% c("character", "factor", "labelled")) {
    rows <- c(rows, ks_pat_string(col, sub, kind))
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

# ---- numeric ---------------------------------------------------------

ks_pat_numeric <- function(col, sub) {
  out <- list()
  b <- suppressWarnings(as.numeric(sub$base))
  cc <- suppressWarnings(as.numeric(sub$comp))
  ok <- is.finite(b) & is.finite(cc)

  na_b <- is.na(b) & is.finite(cc)
  na_c <- is.finite(b) & is.na(cc)
  na_either <- na_b | na_c

  # null_as_zero
  if (any(na_either)) {
    nz <- (na_b & cc == 0) | (na_c & b == 0)
    nz[is.na(nz)] <- FALSE
    cov <- mean(nz)
    if (cov >= 0.5) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "null_as_zero", cov,
        "NA replaced with 0 on one side"
      )
    }
  }

  # null_as_sentinel
  if (any(na_either)) {
    sent <- (na_b & cc %in% .ks_na_sentinels) |
            (na_c & b  %in% .ks_na_sentinels)
    sent[is.na(sent)] <- FALSE
    cov <- mean(sent)
    if (cov >= 0.5) {
      vals <- c(b[na_c & b %in% .ks_na_sentinels],
                cc[na_b & cc %in% .ks_na_sentinels])
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "null_as_sentinel", cov,
        sprintf("NA encoded as %s",
                paste(unique(vals), collapse = ", "))
      )
    }
  }

  if (any(ok)) {
    bo <- b[ok]
    co <- cc[ok]
    d  <- bo - co

    # constant_offset
    if (length(unique(round(d, 12))) == 1L) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "constant_offset", mean(ok),
        format(d[1L], trim = TRUE)
      )
    }

    # constant_scale, percentage_scale, unit_scale (multiplicative)
    nz <- bo != 0 & co != 0
    if (any(nz)) {
      ratio <- bo[nz] / co[nz]
      if (length(unique(round(ratio, 9))) == 1L) {
        r1 <- ratio[1L]
        out[[length(out) + 1L]] <- ks_pat_row(
          col, "constant_scale", mean(ok),
          format(r1, trim = TRUE)
        )
        if (isTRUE(all.equal(r1, 100, tolerance = 1e-6)) ||
            isTRUE(all.equal(r1, 0.01, tolerance = 1e-6))) {
          out[[length(out) + 1L]] <- ks_pat_row(
            col, "percentage_scale", mean(ok),
            sprintf("ratio %.6g (likely fraction vs percent)", r1)
          )
        } else {
          for (u in .ks_unit_scales) {
            if (isTRUE(all.equal(r1, u$ratio, tolerance = 1e-6))) {
              out[[length(out) + 1L]] <- ks_pat_row(
                col, "unit_scale", mean(ok), u$label
              )
              break
            }
          }
        }
      }
    }

    # sign_flip
    sf <- abs(bo + co) < 1e-12 & abs(bo) > 0
    if (mean(sf) >= 0.5) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "sign_flip", mean(sf)
      )
    }

    # signed_vs_abs
    sa <- (abs(bo - abs(co)) < 1e-12 | abs(co - abs(bo)) < 1e-12) &
          (sign(bo) != sign(co)) &
          !sf
    if (mean(sa) >= 0.5) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "signed_vs_abs", mean(sa),
        "one side stores the absolute value"
      )
    }

    # integer_round
    ir <- abs(bo - round(co)) < 1e-12 | abs(co - round(bo)) < 1e-12
    if (mean(ir) >= 0.5) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "integer_round", mean(ir)
      )
    }

    # precision_truncated: smallest k s.t. one side equals round(other, k)
    for (k in 1:6) {
      pt <- abs(co - round(bo, k)) < 10^(-k - 4) |
            abs(bo - round(co, k)) < 10^(-k - 4)
      if (length(pt) >= 3L && all(pt)) {
        out[[length(out) + 1L]] <- ks_pat_row(
          col, "precision_truncated", 1,
          sprintf("rounded to %d decimal place%s on one side",
                  k, if (k == 1) "" else "s")
        )
        break
      }
    }

    # monotone_drift
    if (length(d) >= 10L && length(unique(d)) > 1L) {
      rho <- suppressWarnings(stats::cor(seq_along(d), d, method = "spearman"))
      if (is.finite(rho) && abs(rho) >= 0.9) {
        out[[length(out) + 1L]] <- ks_pat_row(
          col, "monotone_drift", abs(rho),
          sprintf("diff trends with row order (rho = %+.2f)", rho)
        )
      }
    }
  }

  out
}

# ---- logical ---------------------------------------------------------

ks_pat_logical <- function(col, sub) {
  out <- list()
  b  <- suppressWarnings(as.logical(sub$base))
  cc <- suppressWarnings(as.logical(sub$comp))
  both <- !is.na(b) & !is.na(cc)

  if (any(both)) {
    swap <- both & (b == !cc)
    cov <- mean(swap)
    if (cov >= 0.5) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "flag_polarity_swapped", cov,
        "TRUE/FALSE inverted between sides"
      )
    }
  }

  na_b <- is.na(b)  & !is.na(cc)
  na_c <- !is.na(b) & is.na(cc)

  if (any(na_b | na_c)) {
    t2n <- (na_b & isTRUE_vec(cc)) | (na_c & isTRUE_vec(b))
    cov <- mean(t2n)
    if (cov >= 0.5) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "true_to_na", cov,
        "TRUE on one side, NA on the other"
      )
    }
    f2n <- (na_b & isFALSE_vec(cc)) | (na_c & isFALSE_vec(b))
    cov <- mean(f2n)
    if (cov >= 0.5) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "false_to_na", cov,
        "FALSE on one side, NA on the other"
      )
    }
  }
  out
}

# ---- temporal --------------------------------------------------------

ks_pat_temporal <- function(col, sub, kind) {
  out <- list()
  d <- suppressWarnings(as.numeric(sub$diff))
  ok <- is.finite(d)
  if (!any(ok)) return(out)

  if (length(unique(round(d[ok], 9))) == 1L) {
    unit <- if (kind == "date") "days" else "s"
    out[[length(out) + 1L]] <- ks_pat_row(
      col, "constant_offset", mean(ok),
      paste(format(d[ok][1L], trim = TRUE), unit)
    )

    delta_days <- if (kind == "date") d[ok][1L] else d[ok][1L] / 86400
    for (nm in names(.ks_epoch_days)) {
      if (isTRUE(all.equal(abs(delta_days), .ks_epoch_days[[nm]],
                           tolerance = 1L))) {
        out[[length(out) + 1L]] <- ks_pat_row(
          col, "epoch_swap", mean(ok),
          sprintf("%s delta (%+d days) -- likely epoch mismatch",
                  nm, as.integer(delta_days))
        )
        break
      }
    }

    if (kind == "datetime") {
      sec <- d[ok][1L]
      if (sec != 0 && abs(sec %% 3600) < 1) {
        h <- sec / 3600
        if (abs(h) <= 24) {
          out[[length(out) + 1L]] <- ks_pat_row(
            col, "tz_hour_offset", mean(ok),
            sprintf("%+dh constant offset (likely DST or timezone)",
                    as.integer(h))
          )
        }
      }
    }

    if (kind == "date") {
      yrs <- delta_days / 365.25
      if (abs(round(yrs)) >= 1 && abs(yrs - round(yrs)) < 0.02) {
        out[[length(out) + 1L]] <- ks_pat_row(
          col, "year_offset", mean(ok),
          sprintf("%+d year offset (constant)", as.integer(round(yrs)))
        )
      }
    }
  }

  if (kind == "datetime") {
    bs <- as.character(sub$base)
    cs <- as.character(sub$comp)
    # A datetime string is "midnight" either when it ends in 00:00:00
    # or when format() dropped the time component entirely (date-only).
    midnight <- function(x) {
      !grepl("\\d{2}:\\d{2}:\\d{2}", x) |
        grepl("00:00:00(\\.0+)?\\s*$", x)
    }
    mt <- (midnight(cs) & !midnight(bs)) | (midnight(bs) & !midnight(cs))
    mt[is.na(mt)] <- FALSE
    if (any(mt) && mean(mt) >= 0.5) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "midnight_truncation", mean(mt),
        "datetime collapsed to date (00:00:00) on one side"
      )
    }
  }
  out
}

# ---- string / factor / labelled --------------------------------------

ks_pat_string <- function(col, sub, kind) {
  out <- list()
  b  <- as.character(sub$base)
  cc <- as.character(sub$comp)
  diff <- !is.na(b) & !is.na(cc) & b != cc
  if (!any(diff)) return(out)

  bd <- b[diff]; cd <- cc[diff]

  trim_eq <- stringi::stri_trim_both(bd) == stringi::stri_trim_both(cd)
  trim_eq[is.na(trim_eq)] <- FALSE
  if (any(trim_eq)) {
    out[[length(out) + 1L]] <- ks_pat_row(
      col, "trim_only", mean(trim_eq)
    )
  }

  ws <- !trim_eq &
    (gsub("\\s+", "", bd) == gsub("\\s+", "", cd))
  ws[is.na(ws)] <- FALSE
  if (any(ws)) {
    out[[length(out) + 1L]] <- ks_pat_row(
      col, "whitespace_only", mean(ws)
    )
  }

  cs_eq <- !trim_eq & !ws &
    (stringi::stri_trans_tolower(bd) == stringi::stri_trans_tolower(cd))
  cs_eq[is.na(cs_eq)] <- FALSE
  if (any(cs_eq)) {
    out[[length(out) + 1L]] <- ks_pat_row(
      col, "case_only", mean(cs_eq)
    )
  }

  norm_eq <- !trim_eq & !ws & !cs_eq &
    (stringi::stri_trans_nfc(bd) == stringi::stri_trans_nfc(cd))
  norm_eq[is.na(norm_eq)] <- FALSE
  if (any(norm_eq)) {
    out[[length(out) + 1L]] <- ks_pat_row(
      col, "unicode_normalization_only", mean(norm_eq),
      "Unicode composed/decomposed forms differ"
    )
  }

  pun_eq <- !trim_eq & !ws & !cs_eq & !norm_eq &
    (gsub("[[:punct:][:space:]]+", "", bd) ==
     gsub("[[:punct:][:space:]]+", "", cd))
  pun_eq[is.na(pun_eq)] <- FALSE
  if (any(pun_eq)) {
    out[[length(out) + 1L]] <- ks_pat_row(
      col, "punctuation_only", mean(pun_eq),
      "differs only in punctuation"
    )
  }

  # prefix_added: c == paste0(<constant prefix>, b)
  if (all(nchar(cd) > nchar(bd))) {
    cand <- substr(cd, 1, nchar(cd) - nchar(bd))
    rest <- substr(cd, nchar(cd) - nchar(bd) + 1, nchar(cd))
    if (all(rest == bd) && length(unique(cand)) == 1L) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "prefix_added", 1,
        sprintf("comp side gained prefix \"%s\"", cand[1L])
      )
    }
  }
  # prefix_removed: b == paste0(<constant prefix>, c)
  if (all(nchar(bd) > nchar(cd))) {
    cand <- substr(bd, 1, nchar(bd) - nchar(cd))
    rest <- substr(bd, nchar(bd) - nchar(cd) + 1, nchar(bd))
    if (all(rest == cd) && length(unique(cand)) == 1L) {
      out[[length(out) + 1L]] <- ks_pat_row(
        col, "prefix_removed", 1,
        sprintf("base side has extra prefix \"%s\"", cand[1L])
      )
    }
  }
  # suffix_added: c == paste0(b, <constant suffix>)
  if (all(nchar(cd) > nchar(bd))) {
    head_eq <- substr(cd, 1, nchar(bd)) == bd
    if (all(head_eq)) {
      cand <- substr(cd, nchar(bd) + 1, nchar(cd))
      if (length(unique(cand)) == 1L) {
        out[[length(out) + 1L]] <- ks_pat_row(
          col, "suffix_added", 1,
          sprintf("comp side gained suffix \"%s\"", cand[1L])
        )
      }
    }
  }
  # suffix_removed: b == paste0(c, <constant suffix>)
  if (all(nchar(bd) > nchar(cd))) {
    head_eq <- substr(bd, 1, nchar(cd)) == cd
    if (all(head_eq)) {
      cand <- substr(bd, nchar(cd) + 1, nchar(bd))
      if (length(unique(cand)) == 1L) {
        out[[length(out) + 1L]] <- ks_pat_row(
          col, "suffix_removed", 1,
          sprintf("base side has extra suffix \"%s\"", cand[1L])
        )
      }
    }
  }

  # zero_padded
  zp <- ks_detect_zero_padded(bd, cd)
  if (!is.null(zp)) {
    out[[length(out) + 1L]] <- ks_pat_row(
      col, "zero_padded", zp$coverage,
      sprintf("one side is zero-padded to width %d", zp$width)
    )
  }

  # truncated_to_width
  tw <- ks_detect_truncated(bd, cd)
  if (!is.null(tw)) {
    out[[length(out) + 1L]] <- ks_pat_row(
      col, "truncated_to_width", tw$coverage,
      sprintf("truncated to %d characters on one side", tw$width)
    )
  }

  # abbreviation (varying-length prefix, not constant -> truncated_to_width
  # already covers constant width)
  ab <- ks_detect_abbreviation(bd, cd)
  if (!is.null(ab)) {
    out[[length(out) + 1L]] <- ks_pat_row(
      col, "abbreviation", ab$coverage,
      "one side stores an abbreviation (varying length prefix)"
    )
  }

  # coded_decode / factor_recoded: bijective per-value mapping
  bs_unique <- unique(bd)
  if (length(bs_unique) >= 2L && length(bs_unique) < length(bd)) {
    per_b <- vapply(
      bs_unique,
      function(v) length(unique(cd[bd == v])),
      integer(1L)
    )
    if (all(per_b == 1L)) {
      cd_pairs <- unique(data.frame(b = bd, c = cd, stringsAsFactors = FALSE))
      label <- if (kind == "factor") "factor_recoded" else "coded_decode"
      pairs <- paste(cd_pairs$b, "->", cd_pairs$c)
      out[[length(out) + 1L]] <- ks_pat_row(
        col, label, 1,
        paste(utils::head(pairs, 6L), collapse = "; ")
      )
    }
  }

  # near_match (typo cluster) -- requires stringdist
  if (requireNamespace("stringdist", quietly = TRUE) &&
      length(bd) >= 3L) {
    not_already <- !(trim_eq | ws | cs_eq | norm_eq | pun_eq)
    if (any(not_already)) {
      bd2 <- stringi::stri_trans_tolower(stringi::stri_trim_both(bd[not_already]))
      cd2 <- stringi::stri_trans_tolower(stringi::stri_trim_both(cd[not_already]))
      dst <- stringdist::stringdist(bd2, cd2, method = "osa")
      m <- pmax(nchar(bd2), nchar(cd2), 1L)
      near <- dst <= 2L | (dst / m) <= 0.2
      cov <- sum(near) / length(bd)
      if (mean(near) >= 0.5 && cov >= 0.3) {
        out[[length(out) + 1L]] <- ks_pat_row(
          col, "near_match", cov,
          sprintf("median Levenshtein distance %g (likely typos)",
                  stats::median(dst[near]))
        )
      }
    }
  }

  out
}

ks_detect_zero_padded <- function(bd, cd) {
  pad_side <- function(x, y) {
    yn <- suppressWarnings(as.integer(y))
    if (any(is.na(yn))) return(NULL)
    widths <- nchar(x)
    if (length(unique(widths)) != 1L) return(NULL)
    w <- widths[1L]
    if (w < 2L) return(NULL)
    expected <- formatC(yn, width = w, flag = "0", format = "d")
    if (all(expected == x)) {
      return(list(coverage = mean(x == expected), width = w))
    }
    NULL
  }
  pad_side(cd, bd) %||% pad_side(bd, cd)
}

ks_detect_truncated <- function(bd, cd) {
  trunc_side <- function(short, long) {
    if (any(nchar(short) >= nchar(long))) return(NULL)
    widths <- nchar(short)
    if (length(unique(widths)) != 1L) return(NULL)
    w <- widths[1L]
    if (w < 3L) return(NULL)
    if (all(substr(long, 1, w) == short)) {
      return(list(coverage = 1, width = w))
    }
    NULL
  }
  trunc_side(cd, bd) %||% trunc_side(bd, cd)
}

ks_detect_abbreviation <- function(bd, cd) {
  abbrev_side <- function(short, long) {
    if (any(nchar(short) >= nchar(long))) return(NULL)
    if (length(unique(nchar(short))) <= 1L) return(NULL)
    if (all(substr(long, 1, nchar(short)) == short)) {
      return(list(coverage = 1))
    }
    NULL
  }
  abbrev_side(cd, bd) %||% abbrev_side(bd, cd)
}

# ---- cross-column ----------------------------------------------------

ks_detect_patterns_cross <- function(value_diff) {
  empty <- tibble::tibble(
    column = character(),
    pattern = character(),
    coverage = numeric(),
    detail = character()
  )
  n <- nrow(value_diff)
  if (n < 5L) return(empty)
  rows <- list()

  per_col <- sort(table(value_diff$column_base), decreasing = TRUE)
  cum <- cumsum(per_col) / n
  if (length(per_col) >= 2L) {
    k <- which(cum >= 0.95)[[1L]]
    if (!is.na(k) && k <= max(1L, ceiling(length(per_col) * 0.4))) {
      top <- names(per_col)[seq_len(k)]
      rows[[length(rows) + 1L]] <- ks_pat_row(
        NA_character_, "pareto_columns", as.numeric(cum[[k]]),
        sprintf("%d / %d column%s explain %.0f%% of cells: %s",
                k, length(per_col), if (k == 1) "" else "s",
                100 * cum[[k]], paste(top, collapse = ", "))
      )
    }
  }

  per_key <- sort(table(value_diff$key_id), decreasing = TRUE)
  if (length(per_key) >= 2L) {
    top1 <- per_key[[1L]] / n
    if (top1 >= 0.5) {
      rows[[length(rows) + 1L]] <- ks_pat_row(
        NA_character_, "pareto_keys", as.numeric(top1),
        sprintf("key_id %s accounts for %.0f%% of diff cells",
                names(per_key)[[1L]], 100 * top1)
      )
    }
  }

  if (length(per_col) >= 2L) {
    big <- names(per_col)[per_col >= 5L]
    if (length(big) >= 2L) {
      sets <- lapply(big, function(cn) unique(value_diff$key_id[
        value_diff$column_base == cn
      ]))
      names(sets) <- big
      pairs <- list()
      for (i in seq_len(length(big) - 1L)) {
        for (j in seq.int(i + 1L, length(big))) {
          a <- sets[[i]]; bset <- sets[[j]]
          inter <- length(intersect(a, bset))
          uni <- length(union(a, bset))
          jac <- if (uni == 0L) 0 else inter / uni
          if (jac >= 0.9 && inter >= 5L) {
            pairs[[length(pairs) + 1L]] <- ks_pat_row(
              NA_character_, "paired_columns", jac,
              sprintf("%s & %s differ on the same %d row%s (Jaccard %.2f)",
                      big[[i]], big[[j]], inter,
                      if (inter == 1) "" else "s", jac)
            )
          }
        }
      }
      pairs <- utils::head(pairs, 10L)
      rows <- c(rows, pairs)
    }
  }

  if (length(rows) == 0L) return(empty)
  vctrs::vec_rbind(!!!rows)
}
