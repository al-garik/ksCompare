#' ksCompare: smart data frame comparison
#'
#' A tidyverse-native engine for comparing two data frames in the
#' spirit of SAS `PROC COMPARE`, with extensions for clinical / pharma
#' QC workflows.
#'
#' # Getting started
#'
#' The single entry point is [ks_compare()]:
#'
#' ```r
#' library(ksCompare)
#'
#' cmp <- ks_compare(adsl_prod, adsl_qc, by = "USUBJID")
#' cmp                       # console summary
#' tibble::as_tibble(cmp)    # long cell-level diff table
#' ks_report_html(cmp, "qc.html")
#' ```
#'
#' # Building blocks
#'
#' - [ks_compare()] -- main entry point.
#' - [ks_tol()] -- numeric tolerance (abs / rel / ULP, per-column).
#' - [ks_options()] -- NA, SAS special-missing, label / format,
#'   string normalisation, time-zone handling.
#' - [ks_assert_clean()] / [ks_pointblank_step()] -- pipeline gates.
#' - [ks_report_html()] / [ks_report_xlsx()] -- shareable reports.
#' - [as_outbase()] / [as_outcomp()] / [as_outdif()] /
#'   [as_outnoequal()] -- PROC COMPARE-style output datasets.
#' - [ks_sysinfo()] -- SAS `&SYSINFO`-compatible bitmask for CI.
#'
#' [ks_compare()] also accepts file paths directly (`.sas7bdat`,
#' `.xpt`, `.parquet`, `.feather`/`.arrow`, `.rds`, `.rdata`/`.rda`,
#' `.csv`, `.tsv`) and chooses a reader based on the file extension.
#'
#' # Vignettes
#'
#' - `vignette("getting-started", package = "ksCompare")`
#' - `vignette("from-proc-compare", package = "ksCompare")`
#' - `vignette("smart-features", package = "ksCompare")`
#' - `vignette("reports", package = "ksCompare")`
#' - `vignette("pipeline-gates", package = "ksCompare")`
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom rlang :=
## usethis namespace: end
NULL
