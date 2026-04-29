# ---------------------------------------------------------------------------
# 03 - Tolerances and options
#
# Demonstrates all knobs of ks_tol() and ks_options() side by side.
# ---------------------------------------------------------------------------

library(ksCompare)

base <- data.frame(
  id     = 1:4,
  weight = c(70.00,  80.00, 65.00, 90.00),
  height = c( 1.75,   1.80,  1.65,  1.90),
  amount = c(10.00,  20.00, 30.00, 40.00)
)

comp <- data.frame(
  id     = 1:4,
  weight = c(70.005, 80.02, 65.00, 90.00),  # rounding noise
  height = c( 1.751, 1.799, 1.650, 1.900),  # within 0.1%
  amount = c(10.001, 20.00, 29.99, 40.00)   # >= 0.005
)

# 1) Strict (default): every cell that differs is flagged.
ks_compare(base, comp, by = "id")

# 2) Absolute tolerance: ignore <= 0.01 differences everywhere.
ks_compare(base, comp, by = "id", tolerance = ks_tol(abs = 0.01))

# 3) Relative tolerance: ignore <= 0.1% relative differences.
ks_compare(base, comp, by = "id", tolerance = ks_tol(rel = 1e-3))

# 4) Per-column overrides: weight loose, amount strict, height relative.
ks_compare(
  base, comp, by = "id",
  tolerance = ks_tol(
    abs = 0,
    per_column = list(
      weight = ks_tol(abs = 0.05),
      amount = ks_tol(abs = 0.005),
      height = ks_tol(rel = 1e-3)
    )
  )
)

# 5) ULP tolerance: useful for floating-point round-trip noise.
x <- 1.0
y <- x + 4 * .Machine$double.eps
ks_compare(
  data.frame(id = 1, x = x),
  data.frame(id = 1, x = y),
  by = "id",
  tolerance = ks_tol(ulp = 8)
)

# ---- ks_options() --------------------------------------------------------

a <- data.frame(
  id  = 1:3,
  s   = c("foo  ", "Bar", NA),  # trailing spaces, mixed case, NA
  n   = c(1, NA, 3),
  stringsAsFactors = FALSE
)
b <- data.frame(
  id  = 1:3,
  s   = c("foo", "BAR", NA),
  n   = c(1, NA, 3),
  stringsAsFactors = FALSE
)

# Default: NA == NA, strict strings -> two value diffs (whitespace + case)
ks_compare(a, b, by = "id")

# Trim + fold case -> no string diffs
ks_compare(
  a, b, by = "id",
  options = ks_options(str_trim = TRUE, str_case = "fold")
)

# Treat NA != NA (PROC COMPARE without NOMISS) -> NA at id=2 surfaces
ks_compare(a, b, by = "id", options = ks_options(na_equal = FALSE))

# Suppress label/format drift on schema
attr(a$n, "label")      <- "Numeric value"
attr(b$n, "label")      <- "Number"
attr(a$n, "format.sas") <- "BEST12."
attr(b$n, "format.sas") <- "BEST."

ks_compare(a, b, by = "id")$schema_diff

ks_compare(
  a, b, by = "id",
  options = ks_options(compare_labels = FALSE, compare_formats = FALSE)
)$schema_diff
