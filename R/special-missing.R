#' Internal: detect SAS special-missing tags on a vector
#'
#' SAS supports special missing values `.A`-`.Z` and `._`. The `haven`
#' package surfaces them via [haven::tagged_na()] / [haven::na_tag()].
#' For non-haven inputs the result is a character vector of `NA`s.
#'
#' @return Character vector the same length as `x`. `""` for ordinary `NA`,
#'   single character (`"a"-"z"` or `"_"`) for tagged NAs, and `NA_character_`
#'   for non-NA elements.
#' @keywords internal
#' @noRd
ks_na_tag <- function(x) {
  out <- rep(NA_character_, length(x))
  na <- is.na(x)
  if (!any(na)) {
    return(out)
  }
  if (rlang::is_installed("haven") && is.double(x)) {
    tag <- haven::na_tag(x)
    out[na] <- ifelse(is.na(tag[na]), "", tag[na])
    return(out)
  }
  out[na] <- ""
  out
}

#' Internal: equality predicate respecting SAS special missings
#'
#' Two NA values are equal if they have the same tag (both ordinary NA, or
#' both `.A`, etc.). When `respect_tags = FALSE`, all NAs are treated as
#' equal regardless of tag.
#'
#' @keywords internal
#' @noRd
ks_na_equal <- function(x, y, respect_tags = TRUE) {
  nx <- is.na(x)
  ny <- is.na(y)
  both_na <- nx & ny
  if (!respect_tags) {
    return(both_na)
  }
  tx <- ks_na_tag(x)
  ty <- ks_na_tag(y)
  both_na & (tx == ty | (is.na(tx) & is.na(ty)))
}
