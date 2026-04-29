#' Internal: propose fuzzy column rename candidates
#'
#' Given the unmatched base- and compare-only columns from
#' `ks_align_columns()`, score each cross-pair on a combination of name
#' similarity (Jaro-Winkler via `stringdist`, Suggests-gated) and
#' compatible "kind" (numeric-ish vs character-ish vs date-ish), then
#' return the top-`n` suggestions sorted by score.
#'
#' This function never modifies the alignment; it returns suggestions only.
#' The user must opt in by passing the chosen pairs to `mapping =`.
#'
#' @return A tibble with columns `base`, `comp`, `score`, `name_similarity`,
#'   `kind_compat`. Empty tibble if `stringdist` is not installed or there
#'   are no unmatched columns.
#' @keywords internal
#' @noRd
ks_suggest_columns <- function(base, comp, alignment, n = 5L) {
  bo <- alignment$base_only
  co <- alignment$comp_only
  empty <- tibble::tibble(
    base = character(),
    comp = character(),
    score = numeric(),
    name_similarity = numeric(),
    kind_compat = logical()
  )
  if (length(bo) == 0L || length(co) == 0L) {
    return(empty)
  }
  if (!rlang::is_installed("stringdist")) {
    return(empty)
  }
  grid <- expand.grid(base = bo, comp = co, stringsAsFactors = FALSE)
  grid$name_similarity <- 1 - stringdist::stringdist(
    tolower(grid$base),
    tolower(grid$comp),
    method = "jw"
  )
  grid$kind_compat <- vapply(
    seq_len(nrow(grid)),
    function(i) {
      ks_kind_compatible(base[[grid$base[[i]]]], comp[[grid$comp[[i]]]])
    },
    logical(1L)
  )
  grid$score <- grid$name_similarity * ifelse(grid$kind_compat, 1, 0.5)
  grid <- grid[order(-grid$score), , drop = FALSE]
  utils::head(tibble::as_tibble(grid), n)
}

ks_kind_compatible <- function(x, y) {
  kx <- ks_kind(x)
  ky <- ks_kind(y)
  if (kx == ky) {
    return(TRUE)
  }
  num <- c("integer", "double")
  txt <- c("character", "factor", "labelled")
  dt <- c("date", "datetime")
  (kx %in% num && ky %in% num) ||
    (kx %in% txt && ky %in% txt) ||
    (kx %in% dt && ky %in% dt)
}
