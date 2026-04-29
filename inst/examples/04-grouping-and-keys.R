# ---------------------------------------------------------------------------
# 04 - Grouping and keys
#
# Demonstrates row matching strategies: explicit keys, renamed keys,
# auto-key inference, duplicate-key handling, and the group_by_key HTML
# report mode.
# ---------------------------------------------------------------------------

library(ksCompare)

# ---- explicit single-column key ------------------------------------------

a <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
b <- data.frame(id = 1:5, x = c(1, 2, 9, 4, 5))

ks_compare(a, b, by = "id")

# ---- multi-column key ----------------------------------------------------

a <- data.frame(
  study = rep("S1", 6),
  arm   = rep(c("A", "B"), each = 3),
  visit = rep(1:3, 2),
  val   = c(10, 20, 30, 40, 50, 60)
)
b <- a
b$val[c(2, 5)] <- c(99, 51)

ks_compare(a, b, by = c("study", "arm", "visit"))

# ---- renamed key column --------------------------------------------------

prod <- data.frame(USUBJID = c("S001", "S002", "S003"), AGE = c(34, 41, 28))
qc   <- data.frame(SUBJID  = c("S001", "S002", "S003"), AGE = c(34, 42, 28))

ks_compare(prod, qc, by = c(USUBJID = "SUBJID"))

# ---- auto key inference --------------------------------------------------

# ks_compare looks for the smallest combination of shared columns that is
# unique on both sides (capped at 4 columns).
a <- data.frame(study = "S1", id = 1:3, x = 1:3)
b <- data.frame(study = "S1", id = 1:3, x = c(1, 9, 3))

suppressMessages(ks_compare(a, b, by = "auto"))

# ---- duplicate-key strategies --------------------------------------------

a <- data.frame(id = c(1, 1, 2, 3), x = c(10, 99, 20, 30))
b <- data.frame(id = c(1, 2, 3),    x = c(10, 20, 31))

# "first" (default) keeps first occurrence per side
suppressMessages(ks_compare(a, b, by = "id", dup_keys = "first"))

# "last" keeps last occurrence per side
suppressMessages(ks_compare(a, b, by = "id", dup_keys = "last"))

# "keep_all" pairs duplicates positionally within each key (1<->1, 2<->2,...)
ks_compare(a, b, by = "id", dup_keys = "keep_all")

# "all_pairs" cartesian-pairs every base row with every comp row sharing a key
suppressWarnings(ks_compare(a, b, by = "id", dup_keys = "all_pairs"))

# "error" raises on any duplicate
try(ks_compare(a, b, by = "id", dup_keys = "error"))

# ---- no key: row-position matching ---------------------------------------

a <- data.frame(x = c(1, 2, 3))
b <- data.frame(x = c(1, 2, 4))
ks_compare(a, b)

# ---- group_by_key HTML report --------------------------------------------

# Renders one collapsed <details> block per key value, sorted by number
# of diffs. With dup_keys = "keep_all" or "all_pairs" each block also
# carries a `Pair` column.
if (interactive() &&
    requireNamespace("htmltools", quietly = TRUE) &&
    requireNamespace("reactable", quietly = TRUE)) {
  a <- data.frame(
    USUBJID = rep(c("S001", "S002", "S003"), each = 3),
    PARAM   = rep(c("ALT", "AST", "BUN"), 3),
    AVAL    = c(10, 20, 30, 11, 22, 33, 12, 24, 36)
  )
  b <- a
  b$AVAL[c(2, 5, 8)] <- b$AVAL[c(2, 5, 8)] + 1

  cmp <- ks_compare(a, b, by = c("USUBJID", "PARAM"))
  ks_report_html(
    cmp,
    title        = "Lab QC -- by subject",
    group_by_key = TRUE,
    max_groups   = 50
  )
}
