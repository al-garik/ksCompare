#' Internal: throw a classed ksCompare error
#'
#' @param message Character vector for [cli::cli_abort()].
#' @param class Optional subclass appended to `ksCompare_error`.
#' @param ... Forwarded to [cli::cli_abort()].
#' @param call Calling environment for the error.
#' @keywords internal
#' @noRd
ks_abort <- function(message, class = NULL, ..., call = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = c(class, "ksCompare_error"),
    ...,
    call = call,
    .envir = call
  )
}

#' Internal: emit a classed ksCompare warning
#' @keywords internal
#' @noRd
ks_warn <- function(message, class = NULL, ..., call = rlang::caller_env()) {
  cli::cli_warn(
    message,
    class = c(class, "ksCompare_warning"),
    ...,
    call = call,
    .envir = call
  )
}

#' Internal: emit a classed ksCompare informational message
#' @keywords internal
#' @noRd
ks_inform <- function(message, class = NULL, ..., call = rlang::caller_env()) {
  cli::cli_inform(
    message,
    class = c(class, "ksCompare_message"),
    ...,
    call = call,
    .envir = call
  )
}

#' Internal: assert a Suggests dependency is installed
#'
#' Used at the boundary where optional features (haven, openxlsx2, ...)
#' are first reached.
#'
#' @keywords internal
#' @noRd
ks_check_installed <- function(pkg, reason = NULL, call = rlang::caller_env()) {
  rlang::check_installed(pkg, reason = reason, call = call)
  invisible(TRUE)
}

#' Internal: stable digest of an R object
#' @keywords internal
#' @noRd
ks_digest <- function(x) {
  digest::digest(x, algo = "xxhash64", serialize = TRUE)
}

#' Internal: deterministic ordering helper
#'
#' Returns a permutation that orders `x` (a data frame) by all of its columns,
#' breaking ties by row index. Used to enforce reproducible report ordering.
#'
#' @keywords internal
#' @noRd
ks_stable_order <- function(x) {
  if (!is.data.frame(x) || ncol(x) == 0L) {
    return(seq_len(NROW(x)))
  }
  do.call(order, c(unname(as.list(x)), list(method = "radix")))
}
