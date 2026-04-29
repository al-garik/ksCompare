#' Convert an `arsenal::comparedf` result into a `ks_comparison`
#'
#' Lossy interop: takes the two source frames stored on a
#' `arsenal::comparedf` object and re-runs [ks_compare()] using the same
#' BY keys. Useful for teams migrating from `arsenal::comparedf()` to
#' ksCompare without rewriting their inputs.
#'
#' Note that ksCompare's diff is computed from scratch — it does not
#' attempt to translate `comparedf`'s internal diff tables row-by-row.
#'
#' @param x An `arsenal::comparedf` object.
#' @param ... Additional arguments forwarded to [ks_compare()] (e.g.
#'   `tolerance`, `options`).
#'
#' @return A `ks_comparison` object.
#'
#' @export
as_ks_comparison <- function(x, ...) {
  UseMethod("as_ks_comparison")
}

#' @export
as_ks_comparison.ks_comparison <- function(x, ...) {
  x
}

#' @export
as_ks_comparison.comparedf <- function(x, ...) {
  ks_check_installed("arsenal", reason = "for as_ks_comparison.comparedf()")
  base <- x$frame.x
  comp <- x$frame.y
  by <- x$byvars
  if (is.null(by) || length(by) == 0L) {
    by <- NULL
  }
  ks_compare(base, comp, by = by, ...)
}

#' @export
as_ks_comparison.default <- function(x, ...) {
  ks_abort(c(
    "Don't know how to convert object of class {.cls {class(x)[[1]]}} to a {.cls ks_comparison}.",
    i = "Supported inputs: {.cls ks_comparison}, {.cls arsenal::comparedf}."
  ))
}
