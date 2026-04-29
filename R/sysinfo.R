#' SAS SYSINFO-compatible bitmask for a comparison
#'
#' SAS's `&SYSINFO` macro variable encodes the result of `PROC COMPARE` as a
#' bitmask. `ks_sysinfo()` returns a compatible-style integer summarizing
#' the same kinds of facts, plus a named integer vector showing which bits
#' are set. Bit positions follow SAS documentation:
#'
#' \tabular{rll}{
#'  Bit \tab Mask     \tab Meaning                                          \cr
#'  1   \tab 1        \tab Data set labels differ                          \cr
#'  2   \tab 2        \tab Data set types differ                           \cr
#'  3   \tab 4        \tab Variable has different informat                 \cr
#'  4   \tab 8        \tab Variable has different format                   \cr
#'  5   \tab 16       \tab Variable has different length                   \cr
#'  6   \tab 32       \tab Variable has different label                    \cr
#'  7   \tab 64       \tab Base data set has observation not in compare    \cr
#'  8   \tab 128      \tab Compare data set has observation not in base    \cr
#'  9   \tab 256      \tab Base data set has BY group not in compare       \cr
#'  10  \tab 512      \tab Compare data set has BY group not in base       \cr
#'  11  \tab 1024     \tab Base data set has variable not in compare       \cr
#'  12  \tab 2048     \tab Compare data set has variable not in base       \cr
#'  13  \tab 4096     \tab A value comparison was unequal                  \cr
#'  14  \tab 8192     \tab Conflicting variable types                      \cr
#'  15  \tab 16384    \tab BY variables do not match                       \cr
#'  16  \tab 32768    \tab Fatal error - comparison not done               \cr
#' }
#'
#' Length and informat are not currently tracked; their bits are always
#' clear. `haven` does not surface SAS informats, and column "length"
#' is not a meaningful R-side concept for numerics.
#'
#' @param x A `ks_comparison` object.
#' @return An integer with class `ks_sysinfo`. Use [as.integer()] for the
#'   raw value or print to see the named bits.
#' @export
#' @examples
#' a <- data.frame(id = 1:3, x = c(1, 2, 3))
#' b <- data.frame(id = 1:3, x = c(1, 2, 4), z = 1:3)
#' cmp <- ks_compare(a, b, by = "id")
#' ks_sysinfo(cmp)
#' as.integer(ks_sysinfo(cmp))
ks_sysinfo <- function(x) {
  ks_assert_comparison(x)
  bits <- ks_sysinfo_bits(x)
  value <- sum(bits[bits > 0L])
  structure(
    as.integer(value),
    bits = bits,
    class = "ks_sysinfo"
  )
}

#' @export
print.ks_sysinfo <- function(x, ...) {
  cli::cli_h2("SYSINFO = {.val {as.integer(x)}}")
  bits <- attr(x, "bits")
  set <- bits[bits > 0L]
  if (length(set) == 0L) {
    cli::cli_alert_success("No conditions set.")
    return(invisible(x))
  }
  for (nm in names(set)) {
    cli::cli_bullets(c("*" = "{.field {nm}} ({.val {set[[nm]]}})"))
  }
  invisible(x)
}

#' @export
as.integer.ks_sysinfo <- function(x, ...) {
  unclass(x)
}

ks_sysinfo_bits <- function(x) {
  schema <- x$schema_diff
  matched <- schema[schema$side == "matched", , drop = FALSE]

  bit <- function(cond, value) if (isTRUE(cond)) value else 0L

  label_diff <- any(matched$label_match == FALSE, na.rm = TRUE)
  format_diff <- any(matched$format_match == FALSE, na.rm = TRUE)
  type_conflict <- any(matched$kind_match == FALSE, na.rm = TRUE)

  base_only_obs <- any(x$row_diff$status == "base_only")
  comp_only_obs <- any(x$row_diff$status == "comp_only")

  base_only_var <- any(schema$side == "base_only")
  comp_only_var <- any(schema$side == "comp_only")

  values_unequal <- nrow(x$value_diff) > 0L

  c(
    "data set labels differ" = bit(FALSE, 1L),
    "data set types differ" = bit(FALSE, 2L),
    "variable informat differs" = bit(FALSE, 4L),
    "variable format differs" = bit(format_diff, 8L),
    "variable length differs" = bit(FALSE, 16L),
    "variable label differs" = bit(label_diff, 32L),
    "base has observation not in compare" = bit(base_only_obs, 64L),
    "compare has observation not in base" = bit(comp_only_obs, 128L),
    "base has BY group not in compare" = bit(FALSE, 256L),
    "compare has BY group not in base" = bit(FALSE, 512L),
    "base has variable not in compare" = bit(base_only_var, 1024L),
    "compare has variable not in base" = bit(comp_only_var, 2048L),
    "value comparison unequal" = bit(values_unequal, 4096L),
    "conflicting variable types" = bit(type_conflict, 8192L),
    "BY variables do not match" = bit(FALSE, 16384L),
    "fatal error" = bit(FALSE, 32768L)
  )
}
