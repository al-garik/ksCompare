# ---------------------------------------------------------------------------
# 06 - pointblank recipes
#
# Six concrete patterns for plugging ksCompare into a pointblank pipeline.
# Each recipe is self-contained and can be run independently.
#
# Background:
#   - ks_assert_clean(cmp) raises a classed condition
#     (`ksCompare_assertion_failed`) when expectations are not met.
#   - ks_pointblank_step(cmp, ...) wraps that gate as a pointblank step
#     suitable for create_agent() workflows.
#
# All recipes use only exported ksCompare functions plus pointblank itself.
# ---------------------------------------------------------------------------

library(ksCompare)

if (!requireNamespace("pointblank", quietly = TRUE)) {
  message("Install 'pointblank' to run the recipes in this script.")
  return(invisible(NULL))
}

library(pointblank)

# ---------------------------------------------------------------------------
# Recipe 1 -- minimal pass/fail gate at the end of an ETL step
#
# Use ks_assert_clean() as a typed assertion in a pipeline. On success it
# returns the comparison invisibly so the chain continues; on failure it
# raises a classed error that CI can catch precisely.
# ---------------------------------------------------------------------------

prod_adsl <- data.frame(USUBJID = c("S001", "S002", "S003"),
                        AGE     = c(34, 41, 28))
qc_adsl   <- prod_adsl

cmp <- ks_compare(prod_adsl, qc_adsl, by = "USUBJID")
result <- tryCatch(
  ks_assert_clean(cmp),
  ksCompare_assertion_failed = function(e) {
    message("QC failed: ", conditionMessage(e))
    NULL
  }
)
stopifnot(!is.null(result))   # passed

# ---------------------------------------------------------------------------
# Recipe 2 -- budget for known acceptable diffs
#
# Real-world QC almost always allows a small known-diff budget (e.g. derived
# variables that round in different orders). All three knobs are independent.
# ---------------------------------------------------------------------------

a <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
b <- data.frame(id = 1:5, x = c(1, 2, 9, 4, 5))   # 1 cell diff

cmp <- ks_compare(a, b, by = "id")

# Strict -> fails
try(ks_assert_clean(cmp))

# Allow a budget of 1 cell diff -> passes
invisible(ks_assert_clean(cmp, max_value_diffs = 1L))

# ---------------------------------------------------------------------------
# Recipe 3 -- pointblank agent gating a whole pipeline
#
# ks_pointblank_step() exposes the comparison gate as a regular pointblank
# step so it participates in pass / warn / fail action levels alongside
# other validations.
# ---------------------------------------------------------------------------

cmp_clean <- ks_compare(prod_adsl, qc_adsl, by = "USUBJID")

agent <- create_agent(
  tbl    = qc_adsl,
  label  = "ADSL release gate",
  actions = action_levels(warn_at = 1L, stop_at = 1L)
) |>
  col_vals_not_null(USUBJID) |>
  col_vals_between(AGE, 18, 99) |>
  ks_pointblank_step(cmp_clean,
                     max_value_diffs = 0L,
                     label = "ksCompare clean vs production")

agent_int <- interrogate(agent)
get_agent_x_list(agent_int)$pass

# ---------------------------------------------------------------------------
# Recipe 4 -- multiple comparisons, one report
#
# Compare several ADaM-style domains and aggregate the gates. Each domain
# becomes its own pointblank step.
# ---------------------------------------------------------------------------

prod_adlb <- data.frame(USUBJID = c("S001", "S001", "S002"),
                        PARAMCD = c("ALT", "AST", "ALT"),
                        AVAL    = c(20.1, 22.3, 18.0))
qc_adlb   <- prod_adlb
qc_adlb$AVAL[1] <- 20.2  # tiny rounding

cmp_adsl <- ks_compare(prod_adsl, qc_adsl, by = "USUBJID")
cmp_adlb <- ks_compare(prod_adlb, qc_adlb,
                       by        = c("USUBJID", "PARAMCD"),
                       tolerance = ks_tol(abs = 0.5))   # absorb rounding

agent <- create_agent(tbl = qc_adsl, label = "Multi-domain QC") |>
  ks_pointblank_step(cmp_adsl, label = "ADSL clean") |>
  ks_pointblank_step(cmp_adlb, label = "ADLB clean (abs <= 0.5)")

interrogate(agent)

# ---------------------------------------------------------------------------
# Recipe 5 -- emit an HTML report whether or not the gate passes
#
# In CI you usually want a debuggable artefact even when the build fails,
# so write the report first and then run the gate.
# ---------------------------------------------------------------------------

cmp <- ks_compare(prod_adsl, qc_adsl, by = "USUBJID")

if (requireNamespace("htmltools", quietly = TRUE) &&
    requireNamespace("reactable", quietly = TRUE)) {
  out <- ks_report_html(cmp, path = tempfile(fileext = ".html"),
                        title = "Release QC", group_by_key = TRUE)
  message("Report: ", out)
}

tryCatch(
  ks_assert_clean(cmp, max_value_diffs = 0L),
  ksCompare_assertion_failed = function(e) {
    message("Gate failed: ", conditionMessage(e))
    # In a CI script you would call quit(status = 1) here.
  }
)

# ---------------------------------------------------------------------------
# Recipe 6 -- testthat integration
#
# The classed error is precisely catchable in unit tests; combine with
# expect_error(class = ...) for tight assertions.
# ---------------------------------------------------------------------------

if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_that("ADSL matches reference", {
    cmp <- ks_compare(prod_adsl, qc_adsl, by = "USUBJID")
    testthat::expect_silent(ks_assert_clean(cmp))
  })

  testthat::test_that("Drift is detected", {
    drifted <- qc_adsl
    drifted$AGE[1] <- drifted$AGE[1] + 1
    cmp <- ks_compare(prod_adsl, drifted, by = "USUBJID")
    testthat::expect_error(
      ks_assert_clean(cmp),
      class = "ksCompare_assertion_failed"
    )
  })
}

# ===========================================================================
# REAL-WORLD WORKFLOW EXAMPLES
#
# The recipes below show how ksCompare and pointblank combine into a
# release-bundle that you can attach to a study lock e-mail or a CI build
# artefact. Each one writes a real HTML report to a temp directory; open
# the printed file paths in a browser to see what an analyst would receive.
# ===========================================================================

# ---------------------------------------------------------------------------
# Recipe 7 -- ADaM release gate with rich pointblank report
#
# Scenario: production statistical programmer ships ADSL / ADAE / ADLB; QC
# programmer independently re-derives the same domains. Before locking the
# study we want a single pointblank report showing:
#   * structural QC of each ADaM domain (column presence, value ranges,
#     uniqueness of USUBJID, etc.)
#   * row-by-row equivalence vs the QC programmer's outputs, with a
#     defensible per-column tolerance for derived numeric variables
#
# pointblank's HTML report shows pass/warn/fail for every step; ksCompare
# emits a per-domain HTML drill-down for any failed step.
# ---------------------------------------------------------------------------

set.seed(2026)
n <- 120L
adsl_prod <- data.frame(
  STUDYID  = "ABC-001",
  USUBJID  = sprintf("ABC-001-%03d", seq_len(n)),
  AGE      = round(rnorm(n, 55, 12)),
  SEX      = sample(c("M", "F"), n, TRUE),
  ARM      = sample(c("Placebo", "Active 50mg", "Active 100mg"), n, TRUE),
  RFSTDTC  = as.Date("2026-01-01") + sample.int(60, n, TRUE),
  SAFFL    = "Y",
  ITTFL    = sample(c("Y", "N"), n, TRUE, prob = c(0.95, 0.05)),
  stringsAsFactors = FALSE
)

adsl_qc <- adsl_prod
# Two realistic QC differences: one rounding inconsistency on AGE for a
# single subject, and a casing drift on ARM for a handful of subjects.
adsl_qc$AGE[3] <- adsl_qc$AGE[3] + 1L
adsl_qc$ARM[c(7, 22, 41)] <- toupper(adsl_qc$ARM[c(7, 22, 41)])

adae_prod <- data.frame(
  USUBJID = sample(adsl_prod$USUBJID, 200, TRUE),
  AETERM  = sample(c("Headache", "Nausea", "Fatigue", "Insomnia",
                     "Dizziness", "Rash"), 200, TRUE),
  AESEV   = sample(c("MILD", "MODERATE", "SEVERE"), 200, TRUE,
                   prob = c(0.6, 0.3, 0.1)),
  AESER   = sample(c("Y", "N"), 200, TRUE, prob = c(0.05, 0.95)),
  stringsAsFactors = FALSE
)
# AESEQ is the within-subject sequence; build it from the (USUBJID-ordered)
# data frame so it is stable.
adae_prod <- adae_prod[order(adae_prod$USUBJID), , drop = FALSE]
adae_prod$AESEQ <- ave(seq_len(nrow(adae_prod)), adae_prod$USUBJID,
                       FUN = seq_along)
adae_qc <- adae_prod
adae_qc$AETERM <- trimws(paste0(adae_qc$AETERM, " "))   # whitespace drift

adlb_prod <- expand.grid(
  USUBJID = adsl_prod$USUBJID[1:30],
  PARAMCD = c("ALT", "AST", "BUN", "CRE", "GLUC"),
  VISIT   = c("BASELINE", "WEEK 4", "WEEK 8", "WEEK 12"),
  stringsAsFactors = FALSE
)
adlb_prod$AVAL  <- round(rlnorm(nrow(adlb_prod), log(25), 0.4), 2)
adlb_prod$CHG   <- round(rnorm(nrow(adlb_prod), 0, 2), 2)
adlb_prod$ANRLO <- 5
adlb_prod$ANRHI <- 50

adlb_qc <- adlb_prod
# Sub-percent rounding noise on derived CHG; absorbed by a small tolerance.
adlb_qc$CHG <- adlb_qc$CHG + rnorm(nrow(adlb_qc), 0, 0.001)
# A handful of true diffs on AVAL that the gate should catch.
adlb_qc$AVAL[c(11, 47, 88)] <- adlb_qc$AVAL[c(11, 47, 88)] * 1.05

cmp_adsl_full <- ks_compare(
  adsl_prod, adsl_qc,
  by      = "USUBJID",
  options = ks_options(str_case = "fold")        # tolerate ARM casing drift
)
cmp_adae_full <- ks_compare(
  adae_prod, adae_qc,
  by      = c("USUBJID", "AESEQ"),
  options = ks_options(str_trim = TRUE)          # tolerate whitespace drift
)
cmp_adlb_full <- ks_compare(
  adlb_prod, adlb_qc,
  by        = c("USUBJID", "PARAMCD", "VISIT"),
  tolerance = ks_tol(
    abs = 0,
    per_column = list(CHG = ks_tol(abs = 0.01))
  )
)

bundle_dir <- file.path(tempdir(), "release-bundle")
dir.create(bundle_dir, showWarnings = FALSE)

# Per-domain ksCompare drill-downs (one HTML per ADaM domain).
if (requireNamespace("htmltools", quietly = TRUE) &&
    requireNamespace("reactable", quietly = TRUE)) {
  ks_report_html(cmp_adsl_full,
                 path  = file.path(bundle_dir, "adsl-vs-qc.html"),
                 title = "ADSL: production vs QC",
                 group_by_key = TRUE)
  ks_report_html(cmp_adae_full,
                 path  = file.path(bundle_dir, "adae-vs-qc.html"),
                 title = "ADAE: production vs QC",
                 group_by_key = TRUE)
  ks_report_html(cmp_adlb_full,
                 path  = file.path(bundle_dir, "adlb-vs-qc.html"),
                 title = "ADLB: production vs QC",
                 group_by_key = TRUE)
}

# Single pointblank agent that combines structural QC and equivalence QC.
release_agent <- create_agent(
  tbl     = adsl_prod,
  label   = "Study ABC-001 -- release gate",
  actions = action_levels(warn_at = 1L, stop_at = 1L)
) |>
  # ----- structural QC of ADSL ---------------------------------------
  rows_distinct(columns = USUBJID,
                label   = "ADSL: USUBJID is unique") |>
  col_vals_not_null(columns = c(USUBJID, ARM, SAFFL),
                    label = "ADSL: required columns are populated") |>
  col_vals_in_set(columns = ARM,
                  set = c("Placebo", "Active 50mg", "Active 100mg"),
                  label = "ADSL: ARM is in the protocol set") |>
  col_vals_between(columns = AGE, left = 18L, right = 99L,
                   label = "ADSL: AGE within 18-99") |>
  # ----- production-vs-QC equivalence on each domain -----------------
  ks_pointblank_step(cmp_adsl_full, max_value_diffs = 0L,
                     label = "ADSL == QC (case-fold tolerated)") |>
  ks_pointblank_step(cmp_adae_full, max_value_diffs = 0L,
                     label = "ADAE == QC (whitespace tolerated)") |>
  ks_pointblank_step(cmp_adlb_full, max_value_diffs = 0L,
                     label = "ADLB == QC (CHG abs <= 0.01)")

release_agent <- interrogate(release_agent)

# Save the pointblank report next to the ksCompare drill-downs. Open
# index.html / report-of-validations.html for the executive view, and the
# per-domain ksCompare HTML for the row-level drill-down.
if (requireNamespace("htmltools", quietly = TRUE)) {
  pb_report <- get_agent_report(release_agent, display_table = FALSE)
  htmltools::save_html(
    pb_report,
    file = file.path(bundle_dir, "release-pointblank-report.html")
  )
}

message("Release bundle written to: ", bundle_dir)
list.files(bundle_dir)

# ---------------------------------------------------------------------------
# Recipe 8 -- multi-environment drift monitor (dev vs UAT vs prod)
#
# Scenario: a nightly job re-runs the same derivation in three environments
# and compares each to a frozen reference snapshot. Acceptable diffs grow
# the further you are from prod (e.g. dev may have intentional changes).
# Each environment becomes its own pointblank step with its own budget;
# the agent's pass / warn / fail summary tells the data-engineering team
# which environments to investigate.
# ---------------------------------------------------------------------------

set.seed(7L)
ref <- data.frame(
  USUBJID = sprintf("S%03d", 1:50),
  AVAL    = round(runif(50, 10, 40), 2),
  AVALC   = sample(c("LOW", "NORMAL", "HIGH"), 50, TRUE),
  stringsAsFactors = FALSE
)

# Each env intentionally drifts a little from the reference.
prod <- ref
prod$AVAL[1] <- prod$AVAL[1] + 0.001                # numeric noise

uat <- ref
uat$AVAL[c(3, 7)] <- uat$AVAL[c(3, 7)] + 0.5        # known feature flag
uat$AVALC[10]    <- "low"                           # casing drift

dev <- ref
dev$AVAL <- dev$AVAL + rnorm(nrow(dev), 0, 0.05)    # work-in-progress
dev$AVALC[c(2, 5, 9, 14)] <- tolower(dev$AVALC[c(2, 5, 9, 14)])

cmp_prod <- ks_compare(ref, prod, by = "USUBJID")
cmp_uat  <- ks_compare(ref, uat,  by = "USUBJID",
                       options = ks_options(str_case = "fold"))
cmp_dev  <- ks_compare(ref, dev,  by = "USUBJID",
                       options = ks_options(str_case = "fold"))

drift_agent <- create_agent(
  tbl     = ref,
  label   = "Nightly drift -- ref vs {prod,uat,dev}",
  actions = action_levels(warn_at = 1L, stop_at = 1L)
) |>
  ks_pointblank_step(cmp_prod,
                     max_value_diffs = 0L,
                     label = "PROD: must be byte-identical to reference") |>
  ks_pointblank_step(cmp_uat,
                     max_value_diffs = 5L,
                     label = "UAT: <=5 cell diffs allowed (feature flag)") |>
  ks_pointblank_step(cmp_dev,
                     max_value_diffs = 50L,
                     max_unmatched_rows = 5L,
                     label = "DEV: large budget (work in progress)")

drift_agent <- interrogate(drift_agent)
drift_agent

# ---------------------------------------------------------------------------
# Recipe 9 -- dynamic per-domain budgets and CI exit code
#
# Scenario: a CI job comparing many domains where the acceptable diff
# budget scales with the size of each domain (e.g. allow up to 0.1% of
# cells to differ). Build the steps programmatically and emit both the
# pointblank report (for humans) and a non-zero exit status (for CI).
# ---------------------------------------------------------------------------

domains <- list(
  ADSL = list(prod = adsl_prod, qc = adsl_qc,
              by   = "USUBJID",
              opts = ks_options(str_case = "fold")),
  ADAE = list(prod = adae_prod, qc = adae_qc,
              by   = c("USUBJID", "AESEQ"),
              opts = ks_options(str_trim = TRUE)),
  ADLB = list(prod = adlb_prod, qc = adlb_qc,
              by   = c("USUBJID", "PARAMCD", "VISIT"),
              opts = ks_options(),
              tol  = ks_tol(abs = 0, per_column = list(CHG = ks_tol(abs = 0.01))))
)

dynamic_agent <- create_agent(
  tbl     = adsl_prod,
  label   = "CI gate with size-scaled budgets",
  actions = action_levels(warn_at = 1L, stop_at = 1L)
)

for (nm in names(domains)) {
  d   <- domains[[nm]]
  cmp <- ks_compare(
    d$prod, d$qc, by = d$by,
    options   = if (is.null(d$opts)) ks_options() else d$opts,
    tolerance = if (is.null(d$tol))  ks_tol()     else d$tol
  )
  budget <- max(1L, ceiling(0.001 * nrow(d$prod) * ncol(d$prod)))
  dynamic_agent <- ks_pointblank_step(
    dynamic_agent,
    cmp,
    max_value_diffs = budget,
    label = sprintf("%s: <= %d cell diffs (0.1%% of %d cells)",
                    nm, budget, nrow(d$prod) * ncol(d$prod))
  )
}

dynamic_agent <- interrogate(dynamic_agent)

# In a CI runner the next two lines decide the exit code. The HTML report
# should still be uploaded as a build artefact regardless of pass/fail.
xl <- get_agent_x_list(dynamic_agent)
if (any(xl$f_failed > 0)) {
  message("CI gate failed -- see release bundle: ", bundle_dir)
  # quit(status = 1)
} else {
  message("CI gate passed.")
}
