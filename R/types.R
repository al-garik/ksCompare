#' Internal: classify a vector into a coarse "kind" used by the diff engine
#'
#' Output values: "logical", "integer", "double", "character", "factor",
#' "date", "datetime", "labelled", "list", "other".
#'
#' This is *not* the same as `vctrs::vec_ptype_full()`; we want a small enum
#' the per-type diff dispatch can switch on cheaply.
#'
#' @keywords internal
#' @noRd
ks_kind <- function(x) {
  if (inherits(x, "haven_labelled")) {
    return("labelled")
  }
  if (inherits(x, c("Date"))) {
    return("date")
  }
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return("datetime")
  }
  if (is.factor(x)) {
    return("factor")
  }
  if (is.character(x)) {
    return("character")
  }
  if (is.logical(x)) {
    return("logical")
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (is.double(x)) {
    return("double")
  }
  if (is.list(x)) {
    return("list")
  }
  "other"
}

#' Internal: column metadata for a single vector
#'
#' @return Named list with `name`, `kind`, `class`, `label`, `format_sas`,
#'   `levels` (factor only), `n`, `n_na`, `n_distinct_sample`.
#' @keywords internal
#' @noRd
ks_col_meta <- function(x, name) {
  list(
    name = name,
    kind = ks_kind(x),
    class = class(x),
    label = attr(x, "label", exact = TRUE) %||% NA_character_,
    format_sas = attr(x, "format.sas", exact = TRUE) %||% NA_character_,
    levels = if (is.factor(x)) levels(x) else NULL,
    n = length(x),
    n_na = sum(is.na(x))
  )
}

#' Internal: column metadata table for an entire data frame
#'
#' @return A tibble with one row per column.
#' @keywords internal
#' @noRd
ks_frame_meta <- function(df) {
  if (ncol(df) == 0L) {
    return(tibble::tibble(
      name = character(),
      kind = character(),
      class = list(),
      label = character(),
      format_sas = character(),
      n = integer(),
      n_na = integer()
    ))
  }
  out <- lapply(seq_len(ncol(df)), function(i) {
    m <- ks_col_meta(df[[i]], names(df)[[i]])
    tibble::tibble(
      name = m$name,
      kind = m$kind,
      class = list(m$class),
      label = m$label,
      format_sas = m$format_sas,
      n = m$n,
      n_na = m$n_na
    )
  })
  vctrs::vec_rbind(!!!out)
}

#' Internal: choose a common ptype for a pair of columns under a coercion mode
#'
#' Modes:
#' - "strict": only allow `vec_ptype2()` to succeed; no implicit lossy casts.
#' - "safe": additionally allow integer<->double, factor<->character.
#' - "lossy": additionally allow numeric<->character (parseable), date<->char
#'   via ISO-8601, and factor<->integer (level codes).
#'
#' Returns a list with components:
#' - `ptype`: a zero-length vector of the chosen common type, or `NULL`.
#' - `note`: a short character explanation if a non-strict cast was used.
#'
#' @keywords internal
#' @noRd
ks_common_ptype <- function(base, comp, mode = c("strict", "safe", "lossy")) {
  mode <- rlang::arg_match(mode)

  # Try the natural vctrs path first.
  pt <- tryCatch(
    vctrs::vec_ptype2(base, comp),
    error = function(e) NULL
  )
  if (!is.null(pt)) {
    return(list(ptype = pt, note = NA_character_))
  }

  if (mode == "strict") {
    return(list(ptype = NULL, note = "incompatible types (strict)"))
  }

  bk <- ks_kind(base)
  ck <- ks_kind(comp)

  # safe rules
  if (bk %in% c("integer", "double") && ck %in% c("integer", "double")) {
    return(list(ptype = double(), note = "int<->double via safe coercion"))
  }
  if (
    (bk == "factor" && ck == "character") ||
      (bk == "character" && ck == "factor")
  ) {
    return(list(
      ptype = character(),
      note = "factor<->character via safe coercion"
    ))
  }
  if (bk == "labelled" || ck == "labelled") {
    # Drop labels for comparison; values are typically integer/double/character
    return(list(
      ptype = character(),
      note = "haven_labelled unwrapped to underlying values"
    ))
  }

  if (mode == "safe") {
    return(list(ptype = NULL, note = "incompatible types (safe)"))
  }

  # lossy rules
  if (
    (bk %in% c("integer", "double") && ck == "character") ||
      (bk == "character" && ck %in% c("integer", "double"))
  ) {
    return(list(
      ptype = character(),
      note = "numeric<->character via lossy coercion"
    ))
  }
  if (
    (bk %in% c("date", "datetime") && ck == "character") ||
      (bk == "character" && ck %in% c("date", "datetime"))
  ) {
    return(list(
      ptype = character(),
      note = "date/datetime<->character via lossy coercion (ISO-8601)"
    ))
  }
  if (
    (bk %in% c("date", "datetime") && ck %in% c("integer", "double")) ||
      (bk %in% c("integer", "double") && ck %in% c("date", "datetime"))
  ) {
    return(list(
      ptype = character(),
      note = "date/datetime<->numeric via lossy coercion"
    ))
  }
  if (
    (bk == "factor" && ck == "integer") ||
      (bk == "integer" && ck == "factor")
  ) {
    return(list(
      ptype = integer(),
      note = "factor<->integer via level codes"
    ))
  }

  list(ptype = NULL, note = "incompatible types (lossy)")
}

#' Internal: cast a column to the chosen common ptype
#'
#' Wraps `vctrs::vec_cast()` with graceful fall-through to character coercion
#' when the chosen ptype is `character()` and `vctrs::vec_cast()` refuses.
#'
#' @keywords internal
#' @noRd
ks_cast <- function(x, ptype) {
  if (is.null(ptype)) {
    return(x)
  }
  out <- tryCatch(
    vctrs::vec_cast(x, ptype),
    error = function(e) NULL
  )
  if (!is.null(out)) {
    return(out)
  }
  # Last-resort: format() to character if target is character.
  if (is.character(ptype)) {
    if (inherits(x, "haven_labelled")) {
      return(format(unclass(x)))
    }
    return(format(x, trim = TRUE))
  }
  ks_abort(
    c(
      "Cannot cast column to chosen common type.",
      "i" = "Source class: {.cls {class(x)}}",
      "i" = "Target class: {.cls {class(ptype)}}"
    ),
    class = "ksCompare_cast_error"
  )
}

#' Internal: null-coalesce
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
