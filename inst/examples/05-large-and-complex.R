# ---------------------------------------------------------------------------
# 05 - Large and complex datasets
#
# A stress-test scenario: ~50,000 rows, multi-column composite key, ~25
# columns of mixed types, and several injected diff patterns. Useful for
# exercising the pattern detector and for benchmarking the HTML / Excel
# reports on realistic volumes.
#
# Run interactively; the example sets a small random seed for repeatability.
# ---------------------------------------------------------------------------

library(ksCompare)

set.seed(42)

n_subj <- 1000L
n_visit <- 5L
n <- n_subj * n_visit          # 5,000 rows -- bump up for a real stress test

make_frame <- function(n_subj, n_visit) {
  subjects <- sprintf("S%05d", seq_len(n_subj))
  visits   <- seq_len(n_visit)

  base <- expand.grid(USUBJID = subjects, VISIT = visits, stringsAsFactors = FALSE)
  base$AGE      <- rep(round(rnorm(n_subj, 55, 12), 1), times = n_visit)
  base$SEX      <- rep(sample(c("M", "F"), n_subj, TRUE), times = n_visit)
  base$ARM      <- rep(sample(c("Placebo", "Active"), n_subj, TRUE), times = n_visit)
  base$RACE     <- rep(sample(c("WHITE", "BLACK", "ASIAN", "OTHER"), n_subj, TRUE),
                       times = n_visit)
  base$WEIGHT   <- round(rnorm(nrow(base), 75, 15), 2)
  base$HEIGHT   <- round(rnorm(nrow(base), 1.70, 0.10), 3)
  base$BMI      <- round(base$WEIGHT / base$HEIGHT^2, 2)
  base$ALT      <- round(rlnorm(nrow(base), log(25), 0.4), 1)
  base$AST      <- round(rlnorm(nrow(base), log(22), 0.4), 1)
  base$HBA1C    <- round(rnorm(nrow(base), 5.6, 0.5), 2)
  base$DBP      <- round(rnorm(nrow(base), 80, 10), 0)
  base$SBP      <- round(rnorm(nrow(base), 120, 15), 0)
  base$DTC      <- as.Date("2024-01-01") +
    sample.int(365, nrow(base), TRUE)
  base$RFSTDTC  <- as.POSIXct("2024-01-01 08:00", tz = "UTC") +
    sample.int(86400 * 365, nrow(base), TRUE)
  base$AESEV    <- factor(
    sample(c("MILD", "MODERATE", "SEVERE", NA),
           nrow(base), TRUE, prob = c(0.5, 0.3, 0.1, 0.1)),
    levels = c("MILD", "MODERATE", "SEVERE")
  )
  base$AEDECOD  <- sample(c("HEADACHE", "NAUSEA", "FATIGUE",
                            "INSOMNIA", "DIZZINESS"),
                          nrow(base), TRUE)
  base$AEREL    <- sample(c(TRUE, FALSE), nrow(base), TRUE)
  base$DOSE_MG  <- sample(c(50, 100, 200, 400), nrow(base), TRUE)
  base$COMMENT  <- ""
  tibble::as_tibble(base)
}

base <- make_frame(n_subj, n_visit)
comp <- base

# Inject several diff patterns -------------------------------------------

# 1) constant offset on WEIGHT for all comp rows (kg -> kg + 0.5)
comp$WEIGHT <- comp$WEIGHT + 0.5

# 2) sub-percent rounding noise on HBA1C
comp$HBA1C <- comp$HBA1C + rnorm(nrow(comp), 0, 0.001)

# 3) sign flip on DBP for 2% of rows (data-entry error)
flip <- sample.int(nrow(comp), size = floor(0.02 * nrow(comp)))
comp$DBP[flip] <- -comp$DBP[flip]

# 4) trailing-whitespace drift on AEDECOD for 5% of rows
ws <- sample.int(nrow(comp), size = floor(0.05 * nrow(comp)))
comp$AEDECOD[ws] <- paste0(comp$AEDECOD[ws], " ")

# 5) case fold on RACE for 10% of rows
case <- sample.int(nrow(comp), size = floor(0.10 * nrow(comp)))
comp$RACE[case] <- tolower(comp$RACE[case])

# 6) drop 5 rows on the comp side and add 3 net-new keys
comp <- comp[-sample.int(nrow(comp), 5), , drop = FALSE]
extra <- comp[1:3, , drop = FALSE]
extra$USUBJID <- sprintf("Z%05d", 1:3)
comp <- rbind(comp, extra)

# 7) one schema diff: drop COMMENT on the comp side
comp$COMMENT <- NULL

# ---- the comparison ------------------------------------------------------

system.time({
  cmp <- ks_compare(
    base, comp,
    by        = c("USUBJID", "VISIT"),
    tolerance = ks_tol(
      abs = 0,
      per_column = list(
        WEIGHT = ks_tol(abs = 1),       # absorb the +0.5 offset
        HBA1C  = ks_tol(abs = 0.01),    # absorb rounding noise
        BMI    = ks_tol(rel = 1e-3)
      )
    ),
    options   = ks_comp_options(str_trim = TRUE, str_case = "fold")
  )
})

print(cmp)
ks_glance(cmp)

# Pattern detector picks up the constant offset, sign flip, trim-only,
# case-only patterns automatically.
cmp$pattern_summary

# Reports -- write to tempdir to avoid polluting the working directory.
tmp_html <- tempfile(fileext = ".html")
tmp_xlsx <- tempfile(fileext = ".xlsx")

if (requireNamespace("htmltools", quietly = TRUE) &&
    requireNamespace("reactable", quietly = TRUE)) {
  ks_report_html(
    cmp,
    path         = tmp_html,
    title        = "Stress-test QC",
    subtitle     = sprintf("%d rows / %d columns", nrow(base), ncol(base)),
    group_by_key = TRUE,
    max_groups   = 200L
  )
  message("HTML: ", tmp_html)
}

if (requireNamespace("openxlsx2", quietly = TRUE)) {
  ks_report_xlsx(cmp, path = tmp_xlsx, threshold = 0.5)
  message("XLSX: ", tmp_xlsx)
}

# SAS-style outputs
as_outdif(cmp)
as_outnoequal(cmp)
ks_sysinfo(cmp)
