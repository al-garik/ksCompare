# ---------------------------------------------------------------------------
# 01 - Basic comparison
#
# Goal: introduce ks_compare() with the smallest possible inputs and show
# how to read the result.
# ---------------------------------------------------------------------------

library(ksCompare)

base <- data.frame(
  id  = 1:5,
  age = c(34, 41, 28, 55, 19),
  arm = c("A", "A", "B", "B", "A"),
  stringsAsFactors = FALSE
)

comp <- data.frame(
  id  = 1:5,
  age = c(34, 41, 27, 55, 19),  # one numeric difference
  arm = c("A", "A", "b", "B", "A"),  # one case difference
  stringsAsFactors = FALSE
)

cmp <- ks_compare(base, comp, by = "id")

# Console summary
print(cmp)

# Structured summary
summary(cmp)

# One-row glance for QC dashboards
ks_glance(cmp)

# Long-format diff table (one row per differing cell)
tibble::as_tibble(cmp)

# Inspect specific slots directly
cmp$schema_diff
cmp$row_diff
cmp$value_diff
cmp$pattern_summary
cmp$manifest
