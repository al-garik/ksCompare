#' Internal: align columns between base and compare frames
#'
#' Strategy:
#' 1. If `mapping` is supplied, those pairs are taken as authoritative.
#' 2. Remaining columns are paired by exact name match.
#' 3. Columns present on only one side become `base_only` / `comp_only`.
#'
#' Fuzzy / content-fingerprint matching is only ever surfaced as
#' *suggestions* (see `ks_suggest_columns()`); it is never applied
#' automatically.
#'
#' @param base,comp Data frames.
#' @param mapping Optional named character vector. Names are column names in
#'   `base`, values are column names in `comp`. May also be supplied as a
#'   two-column data frame with columns `base` and `comp`.
#' @return A list with components:
#'   - `pairs`: tibble with columns `base`, `comp` for matched columns.
#'   - `base_only`: character vector of unmatched `base` columns.
#'   - `comp_only`: character vector of unmatched `comp` columns.
#' @keywords internal
#' @noRd
ks_align_columns <- function(base, comp, mapping = NULL) {
  base_cols <- names(base)
  comp_cols <- names(comp)

  pairs_base <- character()
  pairs_comp <- character()

  if (!is.null(mapping)) {
    map_df <- ks_normalize_mapping(mapping)
    bad_base <- setdiff(map_df$base, base_cols)
    bad_comp <- setdiff(map_df$comp, comp_cols)
    if (length(bad_base) > 0L) {
      ks_abort(
        "Column{?s} {.val {bad_base}} in {.arg mapping} not found in {.field base}."
      )
    }
    if (length(bad_comp) > 0L) {
      ks_abort(
        "Column{?s} {.val {bad_comp}} in {.arg mapping} not found in {.field comp}."
      )
    }
    if (anyDuplicated(map_df$base) || anyDuplicated(map_df$comp)) {
      ks_abort("{.arg mapping} contains duplicated column names.")
    }
    pairs_base <- c(pairs_base, map_df$base)
    pairs_comp <- c(pairs_comp, map_df$comp)
  }

  remaining_base <- setdiff(base_cols, pairs_base)
  remaining_comp <- setdiff(comp_cols, pairs_comp)
  exact <- intersect(remaining_base, remaining_comp)

  pairs_base <- c(pairs_base, exact)
  pairs_comp <- c(pairs_comp, exact)

  base_only <- setdiff(remaining_base, exact)
  comp_only <- setdiff(remaining_comp, exact)

  list(
    pairs = tibble::tibble(base = pairs_base, comp = pairs_comp),
    base_only = base_only,
    comp_only = comp_only
  )
}

#' Internal: normalize a mapping argument into a 2-column tibble
#' @keywords internal
#' @noRd
ks_normalize_mapping <- function(mapping) {
  if (is.data.frame(mapping)) {
    if (!all(c("base", "comp") %in% names(mapping))) {
      ks_abort(
        "{.arg mapping} data frame must have columns {.val base} and {.val comp}."
      )
    }
    return(tibble::tibble(
      base = as.character(mapping$base),
      comp = as.character(mapping$comp)
    ))
  }
  if (is.character(mapping)) {
    if (is.null(names(mapping)) || any(names(mapping) == "")) {
      ks_abort(
        "{.arg mapping} character vector must be fully named (base = comp)."
      )
    }
    return(tibble::tibble(base = names(mapping), comp = unname(mapping)))
  }
  ks_abort(
    "{.arg mapping} must be a named character vector or a 2-column data frame."
  )
}

#' Internal: build a per-column schema diff tibble
#'
#' For each matched pair, records column kind, label, and format on both
#' sides plus a flag of whether they agree. Columns present on only one
#' side are appended with side `"base_only"` / `"comp_only"`.
#'
#' @keywords internal
#' @noRd
ks_schema_diff <- function(base, comp, alignment, options) {
  meta_b <- ks_frame_meta(base)
  meta_c <- ks_frame_meta(comp)

  rows <- list()

  if (nrow(alignment$pairs) > 0L) {
    for (i in seq_len(nrow(alignment$pairs))) {
      bn <- alignment$pairs$base[[i]]
      cn <- alignment$pairs$comp[[i]]
      mb <- meta_b[meta_b$name == bn, , drop = FALSE]
      mc <- meta_c[meta_c$name == cn, , drop = FALSE]
      label_eq <- if (options$compare_labels) {
        ks_attr_equal(mb$label, mc$label)
      } else {
        NA
      }
      format_eq <- if (options$compare_formats) {
        ks_format_equal(mb$format_sas, mc$format_sas)
      } else {
        NA
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        base = bn,
        comp = cn,
        side = "matched",
        kind_base = mb$kind,
        kind_comp = mc$kind,
        kind_match = identical(mb$kind, mc$kind),
        label_base = mb$label,
        label_comp = mc$label,
        label_match = label_eq,
        label_diff = if (isFALSE(label_eq)) {
          ks_attr_diff_note(mb$label, mc$label)
        } else {
          NA_character_
        },
        format_base = mb$format_sas,
        format_comp = mc$format_sas,
        format_match = format_eq,
        format_diff = if (isFALSE(format_eq)) {
          ks_format_diff_note(mb$format_sas, mc$format_sas)
        } else {
          NA_character_
        }
      )
    }
  }

  for (nm in alignment$base_only) {
    rows[[length(rows) + 1L]] <- tibble::tibble(
      base = nm,
      comp = NA_character_,
      side = "base_only",
      kind_base = meta_b$kind[meta_b$name == nm],
      kind_comp = NA_character_,
      kind_match = NA,
      label_base = meta_b$label[meta_b$name == nm],
      label_comp = NA_character_,
      label_match = NA,
      label_diff = NA_character_,
      format_base = meta_b$format_sas[meta_b$name == nm],
      format_comp = NA_character_,
      format_match = NA,
      format_diff = NA_character_
    )
  }
  for (nm in alignment$comp_only) {
    rows[[length(rows) + 1L]] <- tibble::tibble(
      base = NA_character_,
      comp = nm,
      side = "comp_only",
      kind_base = NA_character_,
      kind_comp = meta_c$kind[meta_c$name == nm],
      kind_match = NA,
      label_base = NA_character_,
      label_comp = meta_c$label[meta_c$name == nm],
      label_match = NA,
      label_diff = NA_character_,
      format_base = NA_character_,
      format_comp = meta_c$format_sas[meta_c$name == nm],
      format_match = NA,
      format_diff = NA_character_
    )
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(
      base = character(),
      comp = character(),
      side = character(),
      kind_base = character(),
      kind_comp = character(),
      kind_match = logical(),
      label_base = character(),
      label_comp = character(),
      label_match = logical(),
      label_diff = character(),
      format_base = character(),
      format_comp = character(),
      format_match = logical(),
      format_diff = character()
    ))
  }
  vctrs::vec_rbind(!!!rows)
}

#' Internal: tolerant equality for textual attributes (label, format)
#'
#' `identical()` distinguishes by encoding declaration ("unknown" vs
#' "UTF-8" vs "latin1"), which produces spurious diffs when comparing
#' SAS files written on different platforms even though the bytes
#' (after re-encoding) match. This helper:
#' - treats `NA` and empty string as missing,
#' - normalises encoding to UTF-8 before comparing.
#' Whitespace and case are *not* normalised — those are real diffs and
#' will be flagged in `label_diff` / `format_diff`.
#'
#' @keywords internal
#' @noRd
ks_attr_equal <- function(a, b) {
  na_a <- is.null(a) || length(a) == 0L || is.na(a) || identical(a, "")
  na_b <- is.null(b) || length(b) == 0L || is.na(b) || identical(b, "")
  if (na_a && na_b) return(TRUE)
  if (na_a || na_b) return(FALSE)
  identical(enc2utf8(as.character(a)), enc2utf8(as.character(b)))
}

#' Internal: normalise a SAS format name for comparison
#'
#' SAS format names canonically end in a `.` (e.g. `DATE9.`, `BEST12.`,
#' `$CHAR8.`). Different importers disagree on whether to keep it:
#' - `haven::read_sas()` drops the trailing dot.
#' - `haven::read_xpt()` and several xpt writers keep it.
#' - SAS itself emits it, but is happy to read without it.
#' Format names are also case-insensitive in SAS (`DATE9.` == `date9.`),
#' and surrounding whitespace is never significant.
#'
#' This helper normalises by:
#' - returning `NA_character_` for empty / `NA` / zero-length / `""` input;
#' - trimming surrounding whitespace;
#' - upper-casing;
#' - stripping a single trailing `.` (after width).
#' Width and leading `$` (character formats) are preserved as significant.
#'
#' @keywords internal
#' @noRd
ks_normalize_format <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  s <- enc2utf8(as.character(x))
  s <- trimws(s)
  if (is.na(s) || !nzchar(s)) return(NA_character_)
  s <- toupper(s)
  sub("\\.$", "", s)
}

#' Internal: equality test for SAS format attributes
#'
#' Like [ks_attr_equal()] but tolerant of the trailing-dot and case
#' differences that legitimately arise from different SAS importers.
#'
#' @keywords internal
#' @noRd
ks_format_equal <- function(a, b) {
  na <- ks_normalize_format(a)
  nb <- ks_normalize_format(b)
  if (is.na(na) && is.na(nb)) return(TRUE)
  if (is.na(na) || is.na(nb)) return(FALSE)
  identical(na, nb)
}

#' Internal: explain why two attribute values differ
#' @keywords internal
#' @noRd
ks_attr_diff_note <- function(a, b) {
  na_a <- is.null(a) || length(a) == 0L || is.na(a) || identical(a, "")
  na_b <- is.null(b) || length(b) == 0L || is.na(b) || identical(b, "")
  if (na_a && !na_b) return("only on compare side")
  if (!na_a && na_b) return("only on base side")
  if (na_a && na_b) return(NA_character_)
  ax <- enc2utf8(as.character(a))
  bx <- enc2utf8(as.character(b))
  if (identical(ax, bx)) return(NA_character_)
  if (identical(trimws(ax), trimws(bx))) {
    return("leading/trailing whitespace differs")
  }
  if (identical(gsub("\\s+", " ", ax), gsub("\\s+", " ", bx))) {
    return("internal whitespace differs")
  }
  if (identical(tolower(ax), tolower(bx))) {
    return("letter case differs")
  }
  if (nchar(ax) != nchar(bx)) {
    return(sprintf("length differs (%d vs %d)", nchar(ax), nchar(bx)))
  }
  "text differs"
}

#' Internal: explain why two SAS format attributes differ
#'
#' Recognises the SAS-specific cases (trailing dot, letter case) and
#' otherwise falls through to [ks_attr_diff_note()] for the generic
#' whitespace/length/text reasons.
#'
#' @keywords internal
#' @noRd
ks_format_diff_note <- function(a, b) {
  na_a <- is.null(a) || length(a) == 0L || is.na(a) || identical(a, "")
  na_b <- is.null(b) || length(b) == 0L || is.na(b) || identical(b, "")
  if (na_a && !na_b) return("only on compare side")
  if (!na_a && na_b) return("only on base side")
  if (na_a && na_b) return(NA_character_)

  ax <- trimws(enc2utf8(as.character(a)))
  bx <- trimws(enc2utf8(as.character(b)))
  if (identical(ax, bx)) return(NA_character_)

  # If they only differ by trailing dot / case, that's an importer artefact
  # we already treat as equal; report nothing.
  if (ks_format_equal(a, b)) return(NA_character_)

  # Surface other diffs through the generic helper.
  ks_attr_diff_note(a, b)
}
