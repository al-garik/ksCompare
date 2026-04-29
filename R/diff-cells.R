#' Internal: compute element-wise equality for a single column pair
#'
#' Returns a logical vector the same length as the inputs. Both inputs must
#' have already been cast to the chosen common ptype.
#'
#' @keywords internal
#' @noRd
ks_cells_equal <- function(b, c, kind, tol, options) {
  if (length(b) != length(c)) {
    ks_abort("Internal: equality inputs of unequal length.")
  }
  if (length(b) == 0L) {
    return(logical())
  }

  na_eq <- ks_na_equal(b, c, respect_tags = options$sas_special_missing)
  na_either <- is.na(b) | is.na(c)

  eq <- switch(
    kind,
    double = ,
    integer = ks_eq_numeric(b, c, tol),
    logical = b == c,
    character = ks_eq_character(b, c, options),
    factor = ks_eq_factor(b, c),
    date = ,
    datetime = ks_eq_datetime(b, c),
    labelled = ks_eq_character(format(unclass(b)), format(unclass(c)), options),
    list = vapply(seq_along(b), function(i) identical(b[[i]], c[[i]]), logical(1L)),
    # default
    b == c
  )

  # Apply NA semantics
  out <- ifelse(
    na_either,
    if (options$na_equal) na_eq else FALSE,
    eq
  )
  out[is.na(out)] <- FALSE
  as.logical(out)
}

ks_eq_numeric <- function(b, c, tol) {
  ks_eq_numeric_ulp(b, c, tol)
}

ks_eq_character <- function(b, c, options) {
  bb <- enc2utf8(as.character(b))
  cc <- enc2utf8(as.character(c))
  if (options$str_norm == "NFC") {
    bb <- stringi::stri_trans_nfc(bb)
    cc <- stringi::stri_trans_nfc(cc)
  }
  if (options$str_trim) {
    bb <- stringi::stri_trim_both(bb)
    cc <- stringi::stri_trim_both(cc)
  }
  if (options$str_case == "fold") {
    bb <- stringi::stri_trans_tolower(bb)
    cc <- stringi::stri_trans_tolower(cc)
  }
  bb == cc
}

ks_eq_factor <- function(b, c) {
  # Compare by level label, not by integer code.
  as.character(b) == as.character(c)
}

ks_eq_datetime <- function(b, c) {
  # vctrs has already coerced units; rely on equality on the numeric backing.
  unclass(b) == unclass(c)
}

#' Internal: build the long-format value diff for one matched column pair
#'
#' @keywords internal
#' @noRd
ks_value_diff_one <- function(
  base_col,
  comp_col,
  base_name,
  comp_name,
  match,
  tol,
  options
) {
  matched <- match[match$status == "matched", , drop = FALSE]
  if (nrow(matched) == 0L) {
    return(ks_empty_value_diff())
  }

  pt <- ks_common_ptype(base_col, comp_col, mode = "safe")
  if (is.null(pt$ptype)) {
    # Type incompatibility: emit one row per matched row flagging the kind.
    return(tibble::tibble(
      key_id = matched$key_id,
      column_base = base_name,
      column_comp = comp_name,
      kind = "type_mismatch",
      base = format_cell(base_col[matched$base_row]),
      comp = format_cell(comp_col[matched$comp_row]),
      diff = NA_real_,
      note = pt$note
    ))
  }

  b <- ks_cast(base_col[matched$base_row], pt$ptype)
  c <- ks_cast(comp_col[matched$comp_row], pt$ptype)
  kind <- ks_kind(b)
  col_tol <- ks_tol_for(tol, base_name)
  eq <- ks_cells_equal(b, c, kind, col_tol, options)

  unequal <- !eq
  if (!any(unequal)) {
    return(ks_empty_value_diff())
  }

  diff_num <- ks_diff_numeric(b, c, kind, unequal)

  notes <- ks_explain_diffs(
    b[unequal],
    c[unequal],
    kind = kind,
    diff_num = diff_num,
    type_note = if (is.na(pt$note)) NA_character_ else pt$note
  )

  tibble::tibble(
    key_id = matched$key_id[unequal],
    column_base = base_name,
    column_comp = comp_name,
    kind = kind,
    base = format_cell(b[unequal]),
    comp = format_cell(c[unequal]),
    diff = diff_num,
    note = notes
  )
}

#' Internal: numeric diff (base - comp) for the cells flagged as unequal
#'
#' For numeric kinds the diff is plain subtraction. For dates the diff is
#' expressed in days, for datetimes in seconds (matching the natural
#' resolution of `Date` and `POSIXct`). For other kinds we return NA so the
#' Diff column is rendered as `-` rather than as a meaningless number.
#'
#' @keywords internal
#' @noRd
ks_diff_numeric <- function(b, c, kind, unequal) {
  n <- sum(unequal)
  if (n == 0L) return(numeric())
  switch(
    kind,
    integer = ,
    double = suppressWarnings(
      as.numeric(b[unequal]) - as.numeric(c[unequal])
    ),
    date = {
      bb <- as.numeric(unclass(as.Date(b[unequal])))
      cc <- as.numeric(unclass(as.Date(c[unequal])))
      bb - cc
    },
    datetime = {
      bb <- as.numeric(as.POSIXct(b[unequal]))
      cc <- as.numeric(as.POSIXct(c[unequal]))
      bb - cc
    },
    logical = suppressWarnings(
      as.numeric(b[unequal]) - as.numeric(c[unequal])
    ),
    rep(NA_real_, n)
  )
}

#' Internal: render a numeric duration in human-friendly units
#'
#' @keywords internal
#' @noRd
ks_format_duration <- function(seconds) {
  if (!is.finite(seconds) || seconds == 0) return(NA_character_)
  sign_str <- if (seconds < 0) "-" else "+"
  s <- abs(seconds)
  fmt <- function(value, unit) {
    txt <- formatC(value, format = "fg", digits = 3)
    paste0(sign_str, txt, " ", unit)
  }
  if (s < 60)            return(fmt(s, "s"))
  if (s < 3600)          return(fmt(s / 60, "min"))
  if (s < 86400)         return(fmt(s / 3600, "h"))
  if (s < 86400 * 30)    return(fmt(s / 86400, "days"))
  if (s < 86400 * 365.25) return(fmt(s / (86400 * 30.4375), "months"))
  fmt(s / (86400 * 365.25), "years")
}


#' - leading / trailing / internal whitespace
#' - case-only differences
#' - empty-string vs `NA`
#' - non-printable or non-ASCII characters
#' - very small floating-point differences (relative magnitude)
#' - factor/character coercion notes from `ks_common_ptype()`
#'
#' Multiple cues are joined with `"; "`. Returns `NA_character_` when no
#' note applies.
#'
#' @keywords internal
#' @noRd
ks_explain_diffs <- function(b, c, kind, diff_num, type_note = NA_character_) {
  n <- length(b)
  if (n == 0L) return(character())
  notes <- vector("list", n)

  if (kind %in% c("character", "factor", "labelled")) {
    bs <- enc2utf8(as.character(b))
    cs <- enc2utf8(as.character(c))
    for (i in seq_len(n)) {
      bi <- bs[[i]]
      ci <- cs[[i]]
      cues <- character()

      # NA vs empty / non-NA
      if (is.na(bi) && !is.na(ci)) {
        cues <- c(cues, if (identical(ci, "")) "base is NA, compare is empty string" else "base is NA")
      } else if (!is.na(bi) && is.na(ci)) {
        cues <- c(cues, if (identical(bi, "")) "base is empty string, compare is NA" else "compare is NA")
      } else if (!is.na(bi) && !is.na(ci)) {
        if (identical(bi, "") && !identical(ci, "")) {
          cues <- c(cues, "base is empty string")
        } else if (!identical(bi, "") && identical(ci, "")) {
          cues <- c(cues, "compare is empty string")
        }

        b_trim <- stringi::stri_trim_both(bi)
        c_trim <- stringi::stri_trim_both(ci)
        if (!identical(bi, b_trim) || !identical(ci, c_trim)) {
          if (identical(b_trim, c_trim)) {
            sides <- character()
            if (!identical(bi, b_trim)) sides <- c(sides, "base")
            if (!identical(ci, c_trim)) sides <- c(sides, "compare")
            cues <- c(cues, paste0("whitespace padding on ", paste(sides, collapse = " & ")))
          } else {
            cues <- c(cues, "leading/trailing whitespace differs")
          }
        } else {
          # internal whitespace
          if (!identical(bi, ci) &&
              identical(gsub("\\s+", " ", bi), gsub("\\s+", " ", ci))) {
            cues <- c(cues, "internal whitespace differs")
          }
        }

        if (!any(grepl("whitespace", cues, fixed = TRUE)) &&
            !identical(bi, ci) &&
            identical(tolower(bi), tolower(ci))) {
          cues <- c(cues, "letter case differs")
        }

        # Non-printable / non-ASCII
        if (any(grepl("[\u0001-\u0008\u000B\u000C\u000E-\u001F]", c(bi, ci)))) {
          cues <- c(cues, "contains control characters")
        } else {
          b_ascii <- !is.na(iconv(bi, "UTF-8", "ASCII", sub = NA))
          c_ascii <- !is.na(iconv(ci, "UTF-8", "ASCII", sub = NA))
          if (xor(b_ascii, c_ascii)) {
            cues <- c(
              cues,
              if (b_ascii) "compare contains non-ASCII chars" else "base contains non-ASCII chars"
            )
          }
        }

        # Length differences worth pointing out (when neither whitespace nor case)
        if (length(cues) == 0L && !identical(bi, ci)) {
          if (nchar(bi) != nchar(ci)) {
            cues <- c(cues, sprintf("length differs (%d vs %d)", nchar(bi), nchar(ci)))
          }
        }
      }

      notes[[i]] <- cues
    }
  } else if (kind %in% c("integer", "double")) {
    for (i in seq_len(n)) {
      cues <- character()
      bi <- suppressWarnings(as.numeric(b[[i]]))
      ci <- suppressWarnings(as.numeric(c[[i]]))
      if (is.na(bi) && !is.na(ci)) {
        cues <- c(cues, "base is NA")
      } else if (!is.na(bi) && is.na(ci)) {
        cues <- c(cues, "compare is NA")
      } else if (is.finite(bi) && is.finite(ci) && !is.na(diff_num[[i]])) {
        ad <- abs(diff_num[[i]])
        scale <- max(abs(bi), abs(ci))
        if (ad > 0 && scale > 0 && ad / scale < 1e-9) {
          cues <- c(cues, "tiny floating-point difference (< 1e-9 relative)")
        } else if (kind == "double" && ad > 0 && scale > 0 && ad / scale < 1e-4) {
          cues <- c(cues, "small relative difference")
        }
        if (is.finite(bi) && is.finite(ci) &&
            ((bi != 0 && ci == 0) || (bi == 0 && ci != 0))) {
          cues <- c(cues, "one side is zero")
        }
      } else if (is.infinite(bi) || is.infinite(ci)) {
        cues <- c(cues, "infinite value")
      }
      notes[[i]] <- cues
    }
  } else if (kind %in% c("date", "datetime")) {
    seconds_per_day <- 86400
    is_datetime <- kind == "datetime"
    tz_b <- attr(b, "tzone", exact = TRUE)
    tz_c <- attr(c, "tzone", exact = TRUE)
    tz_b <- if (is.null(tz_b) || identical(tz_b, "")) "UTC" else tz_b[[1]]
    tz_c <- if (is.null(tz_c) || identical(tz_c, "")) "UTC" else tz_c[[1]]
    tz_differs <- !identical(tz_b, tz_c)
    for (i in seq_len(n)) {
      cues <- character()
      bi <- b[[i]]
      ci <- c[[i]]
      if (is.na(bi) && !is.na(ci)) {
        cues <- c(cues, "base is NA")
      } else if (!is.na(bi) && is.na(ci)) {
        cues <- c(cues, "compare is NA")
      } else if (!is.na(diff_num[[i]])) {
        if (is_datetime) {
          dur <- ks_format_duration(diff_num[[i]])
          if (!is.na(dur)) cues <- c(cues, paste0("base - compare = ", dur))
          if (abs(diff_num[[i]]) > 0 && abs(diff_num[[i]]) < 1) {
            cues <- c(cues, "sub-second difference")
          } else if (
            abs(diff_num[[i]]) %% seconds_per_day == 0 && abs(diff_num[[i]]) > 0
          ) {
            cues <- c(cues, "whole-day offset")
          }
          if (tz_differs) {
            cues <- c(cues, sprintf("timezone differs (%s vs %s)", tz_b, tz_c))
          }
        } else {
          # date kind: report in days; for >= 365 days also annotate years
          dd <- diff_num[[i]]
          sign_str <- if (dd < 0) "-" else "+"
          d_abs <- abs(dd)
          txt <- paste0(sign_str, formatC(d_abs, format = "fg", digits = 6),
                        if (d_abs == 1) " day" else " days")
          if (d_abs >= 365) {
            yrs <- formatC(d_abs / 365.25, format = "fg", digits = 3)
            txt <- paste0(txt, " (~", sign_str, yrs, " years)")
          }
          cues <- c(cues, paste0("base - compare = ", txt))
        }
      }
      notes[[i]] <- cues
    }
  } else if (kind == "logical") {
    for (i in seq_len(n)) {
      cues <- character()
      if (is.na(b[[i]]) && !is.na(c[[i]])) cues <- c(cues, "base is NA")
      if (!is.na(b[[i]]) && is.na(c[[i]])) cues <- c(cues, "compare is NA")
      notes[[i]] <- cues
    }
  } else {
    for (i in seq_len(n)) notes[[i]] <- character()
  }

  if (!is.na(type_note)) {
    notes <- lapply(notes, function(cues) c(type_note, cues))
  }

  vapply(
    notes,
    function(cues) if (length(cues) == 0L) NA_character_ else paste(cues, collapse = "; "),
    character(1L)
  )
}

format_cell <- function(x) {
  if (is.character(x)) {
    return(x)
  }
  format(x, trim = TRUE, scientific = FALSE)
}

ks_empty_value_diff <- function() {
  tibble::tibble(
    key_id = integer(),
    column_base = character(),
    column_comp = character(),
    kind = character(),
    base = character(),
    comp = character(),
    diff = numeric(),
    note = character()
  )
}
