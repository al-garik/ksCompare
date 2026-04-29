#' Use a comparison as a pipeline gate
#'
#' Treats a `ks_comparison` as a pass/fail check. By default a
#' "passing" comparison has zero schema, key, and value differences;
#' callers can relax those expectations to allow a budget of known
#' acceptable diffs.
#'
#' Two flavours are provided:
#'
#' - [ks_assert_clean()] throws a classed error
#'   (`ksCompare_assertion_failed`) and halts a pipeline when
#'   expectations are unmet. Returns the comparison invisibly on
#'   success, so it can be dropped into a `|>` chain.
#' - [ks_pointblank_step()] returns a `pointblank` step that wraps
#'   [ks_assert_clean()] for use inside [pointblank::create_agent()] /
#'   [pointblank::action_levels()] flows.
#'
#' @param x A `ks_comparison` object returned by [ks_compare()].
#' @param max_value_diffs Integer. Maximum number of differing cells
#'   allowed in matched rows. Default `0L` (strict).
#' @param max_schema_diffs Integer. Maximum schema differences allowed,
#'   counted as the sum of base-only columns, comp-only columns, and
#'   matched columns whose `kind` differs. Default `0L` (strict).
#' @param max_unmatched_rows Integer. Maximum number of unmatched rows
#'   allowed (`n_base_only_rows + n_comp_only_rows`). Default `0L`
#'   (strict).
#'
#' @return [ks_assert_clean()] returns `x` invisibly when expectations
#'   are met. On failure, raises a condition of class
#'   `ksCompare_assertion_failed` (catchable in CI with
#'   `tryCatch(..., ksCompare_assertion_failed = function(e) ...)`).
#'   [ks_pointblank_step()] returns a `pointblank` step.
#'
#' @examples
#' a <- data.frame(id = 1:3, x = 1:3)
#' ks_compare(a, a, by = "id") |> ks_assert_clean()
#'
#' # Allow a small budget of known diffs
#' \dontrun{
#'   ks_compare(a, b, by = "id") |>
#'     ks_assert_clean(max_value_diffs = 5)
#' }
#'
#' @export
ks_assert_clean <- function(
  x,
  max_value_diffs = 0L,
  max_schema_diffs = 0L,
  max_unmatched_rows = 0L
) {
  ks_assert_comparison(x)
  s <- summary(x)
  schema_diffs <- s$n_base_only_columns +
    s$n_comp_only_columns +
    sum(x$schema_diff$kind_base != x$schema_diff$kind_comp, na.rm = TRUE)
  unmatched <- s$n_base_only_rows + s$n_comp_only_rows
  problems <- character()
  if (s$n_value_diffs > max_value_diffs) {
    problems <- c(
      problems,
      sprintf(
        "%d value differences (max %d allowed)",
        s$n_value_diffs,
        max_value_diffs
      )
    )
  }
  if (schema_diffs > max_schema_diffs) {
    problems <- c(
      problems,
      sprintf(
        "%d schema differences (max %d allowed)",
        schema_diffs,
        max_schema_diffs
      )
    )
  }
  if (unmatched > max_unmatched_rows) {
    problems <- c(
      problems,
      sprintf(
        "%d unmatched rows (max %d allowed)",
        unmatched,
        max_unmatched_rows
      )
    )
  }
  if (length(problems) > 0L) {
    bullets <- stats::setNames(problems, rep("x", length(problems)))
    ks_abort(
      c("Comparison did not meet expectations:", bullets),
      class = "ksCompare_assertion_failed"
    )
  }
  invisible(x)
}

#' @rdname ks_assert_clean
#' @param agent A `ptblank_agent` (typically piped in from
#'   [pointblank::create_agent()]) or a data frame / tibble. The
#'   comparison gate is added as a step on this object and `agent` is
#'   returned, so it composes with the rest of a pointblank pipeline.
#' @param comparison A `ks_comparison` produced by [ks_compare()] that
#'   the step will gate on.
#' @param label Optional label for the pointblank step.
#' @export
ks_pointblank_step <- function(
  agent,
  comparison,
  max_value_diffs = 0L,
  max_schema_diffs = 0L,
  max_unmatched_rows = 0L,
  label = "ksCompare clean"
) {
  ks_check_installed("pointblank", reason = "for ks_pointblank_step()")
  if (missing(comparison)) {
    ks_abort(c(
      "{.arg comparison} is required.",
      "i" = "Pass the {.cls ks_comparison} as the second argument, e.g. \\
             {.code agent |> ks_pointblank_step(cmp)}."
    ))
  }
  ks_assert_comparison(comparison)
  pointblank::specially(
    agent,
    fn = function(value) {
      tryCatch(
        {
          ks_assert_clean(
            comparison,
            max_value_diffs = max_value_diffs,
            max_schema_diffs = max_schema_diffs,
            max_unmatched_rows = max_unmatched_rows
          )
          TRUE
        },
        ksCompare_assertion_failed = function(e) FALSE
      )
    },
    label = label
  )
}
