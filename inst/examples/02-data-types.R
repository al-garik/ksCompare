# ---------------------------------------------------------------------------
# 02 - Data types
#
# Demonstrates how ks_compare() handles each major R column type, including:
#   - numeric / integer (with type coercion)
#   - character
#   - factor (and factor <-> character)
#   - logical
#   - Date / POSIXct (with timezones)
#   - haven_labelled (when haven is installed)
# ---------------------------------------------------------------------------

library(ksCompare)

# ---- numeric, integer, logical, character --------------------------------

base <- data.frame(
  id   = 1:4,
  num  = c(1.0, 2.5, 3.1415, 4.0),
  int  = 1:4,
  lgl  = c(TRUE, FALSE, TRUE, NA),
  chr  = c("alpha", "beta", "gamma", "delta"),
  stringsAsFactors = FALSE
)

comp <- data.frame(
  id   = 1:4,
  # numeric differs at row 2 only; int is now stored as double on this side
  num  = c(1.0, 2.6, 3.1415, 4.0),
  int  = c(1, 2, 3, 4),
  lgl  = c(TRUE, FALSE, FALSE, NA),
  chr  = c("alpha", "BETA", "gamma", "delta"),  # case difference
  stringsAsFactors = FALSE
)

# Default coerce = "safe" reconciles integer <-> double silently.
ks_compare(base, comp, by = "id")

# Strict typing surfaces the int/double mismatch as a type_mismatch row.
ks_compare(base, comp, by = "id", coerce = "strict")$value_diff

# ---- factor vs character -------------------------------------------------

a <- data.frame(id = 1:3, x = factor(c("a", "b", "c")))
b <- data.frame(id = 1:3, x = c("a", "b", "C"), stringsAsFactors = FALSE)

# safe coerce: factor <-> character, only "C" vs "c" is a real diff
ks_compare(a, b, by = "id")

# Treat case-only diffs as equal
ks_compare(
  a, b, by = "id",
  options = ks_options(str_case = "fold")
)

# ---- Dates and POSIXct ---------------------------------------------------

a <- data.frame(
  id    = 1:3,
  d     = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")),
  ts_ny = as.POSIXct(c("2025-01-01 12:00", "2025-02-01 12:00", "2025-03-01 12:00"),
                     tz = "America/New_York")
)
b <- a
b$d[2]     <- a$d[2] + 1
b$ts_ny    <- as.POSIXct(format(a$ts_ny, tz = "UTC", usetz = FALSE), tz = "UTC")

# tz = "preserve" (default) -> the tz mismatch above changes the instant only
# in display; underlying numeric is identical, so no diff for ts_ny.
ks_compare(a, b, by = "id")

# tz = "UTC" -> compare both sides converted to UTC
ks_compare(a, b, by = "id", options = ks_options(tz = "UTC"))

# tz = "strip" -> drop tzone before comparing
ks_compare(a, b, by = "id", options = ks_options(tz = "strip"))

# ---- haven_labelled (SAS-style numerics) ---------------------------------

if (requireNamespace("haven", quietly = TRUE)) {
  a <- tibble::tibble(
    id  = 1:4,
    sex = haven::labelled(
      c(1, 2, 1, 2),
      labels = c(Male = 1, Female = 2),
      label  = "Sex"
    )
  )
  b <- tibble::tibble(
    id  = 1:4,
    sex = haven::labelled(
      c(1, 2, 2, 2),
      labels = c(Male = 1, Female = 2),
      label  = "Gender"  # different variable label
    )
  )

  # Default options compare both labels and (where present) format.sas.
  cmp <- ks_compare(a, b, by = "id")
  cmp$schema_diff   # label_match == FALSE
  cmp$value_diff    # one row at id == 3

  # Suppress label-only schema diffs
  ks_compare(
    a, b, by = "id",
    options = ks_options(compare_labels = FALSE)
  )$schema_diff

  # SAS special missing tags (.A-.Z, ._)
  a$sex[1] <- haven::tagged_na("a")
  b$sex[1] <- haven::tagged_na("b")
  ks_compare(a, b, by = "id")$value_diff
}

# ---- format normalisation (DATE9. == DATE9 == date9.) --------------------

a <- data.frame(d = as.Date("2024-01-01"))
attr(a$d, "format.sas") <- "DATE9."
b <- a
attr(b$d, "format.sas") <- "date9"

ks_compare(a, b)$schema_diff   # format_match == TRUE
