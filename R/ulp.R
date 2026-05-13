#' Internal: ULP (units-in-the-last-place) distance between two doubles
#'
#' Treats IEEE-754 binary64 numbers as ordered when reinterpreted as 64-bit
#' signed integers via `bit64`-style mapping (negative numbers are
#' "twos-complemented" so the ordering is preserved). The distance is the
#' absolute difference of these integer representations.
#'
#' Special cases (per common ULP-distance conventions):
#' - If either value is `NA` or `NaN`, returns `NA_real_`.
#' - If one value is `+Inf` and the other `-Inf`, returns `Inf`.
#' - `+0` and `-0` are considered to have ULP distance 0.
#'
#' We avoid 64-bit integer dependencies by working on the 8 raw bytes of
#' each double, mapping to a sign-magnitude form, and computing the
#' (possibly very large) absolute difference as a `double`. Beyond ~2^53
#' the answer loses integer precision, but that is far past any tolerance
#' anyone would specify in practice.
#'
#' @keywords internal
#' @noRd
ks_ulp_distance <- function(x, y) {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n == 0L) {
    return(out)
  }
  ok <- !is.na(x) & !is.na(y) & !is.nan(x) & !is.nan(y)
  if (!any(ok)) {
    return(out)
  }
  # Treat +0 and -0 as identical (their bit patterns differ).
  eq <- ok & (x == y)
  out[eq] <- 0
  todo <- ok & !eq
  if (any(todo)) {
    bx <- ks_double_as_ordered(x[todo])
    by <- ks_double_as_ordered(y[todo])
    out[todo] <- abs(bx - by)
  }
  out
}

#' Internal: map a double to an ordered real
#'
#' Implementation: for each value `v`, take the raw 8-byte little-endian
#' representation, reconstruct it as a signed 53+11-bit number, and apply
#' the IEEE-754-to-monotone integer transform:
#'
#'   sign bit 0 (positive) -> +mag
#'   sign bit 1 (negative) -> -(mag + 1)  (so that -0 maps just below +0)
#'
#' We compute `mag` as the 63-bit unsigned integer encoded by the lower
#' bits, expressed as a `double`. This loses precision above 2^53 but is
#' fine for ULP-tolerance comparisons.
#'
#' @keywords internal
#' @noRd
ks_double_as_ordered <- function(v) {
  n <- length(v)
  if (n == 0L) return(numeric(0L))
  # One bulk writeBin then reshape to an 8-row byte matrix (column = element).
  raw_bytes <- writeBin(as.double(v), raw(), endian = "little")
  m <- matrix(as.integer(raw_bytes), nrow = 8L, ncol = n)
  # Little-endian: row 8 carries sign bit (top bit) + top 7 exponent bits.
  sign_bit <- bitwShiftR(m[8L, ], 7L)
  m[8L, ] <- m[8L, ] - sign_bit * 128L
  # Build magnitude in double space: mag = sum(bytes[i] * 256^(i-1)).
  weights <- 256 ^ (0:7)
  mag <- as.numeric(weights %*% m)
  ifelse(sign_bit == 0L, mag, -(mag + 1))
}

#' Internal: numeric equality including ULP tolerance
#' @keywords internal
#' @noRd
ks_eq_numeric_ulp <- function(b, c, tol) {
  d <- abs(b - c)
  pass_abs <- if (tol$abs > 0) d <= tol$abs else d == 0
  pass_rel <- if (tol$rel > 0) {
    scale <- pmax(abs(b), abs(c))
    is.finite(d) & is.finite(scale) & d <= tol$rel * scale
  } else {
    rep(FALSE, length(d))
  }
  pass_ulp <- if (tol$ulp > 0) {
    ud <- ks_ulp_distance(as.double(b), as.double(c))
    !is.na(ud) & ud <= tol$ulp
  } else {
    rep(FALSE, length(d))
  }
  pass_abs | pass_rel | pass_ulp
}
