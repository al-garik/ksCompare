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
  vapply(
    v,
    function(z) {
      raw <- writeBin(as.double(z), raw(), endian = "little")
      bytes <- as.integer(raw)
      # Little-endian: byte 8 (index 8) holds sign + top 7 bits of exponent.
      sign_bit <- bitwShiftR(bytes[8], 7L)
      # Magnitude bytes: clear sign bit on highest byte.
      bytes[8] <- bytes[8] - sign_bit * 128L
      mag <- 0
      for (i in 8:1) {
        mag <- mag * 256 + bytes[i]
      }
      if (sign_bit == 0L) mag else -(mag + 1)
    },
    numeric(1L)
  )
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
