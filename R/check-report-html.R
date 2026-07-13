#' Summarise check results from \code{ks_check_rules()}
#'
#' Computes row-level validation totals and a per-message breakdown from a
#' data frame containing a \code{check_msgs} column.
#'
#' @param data A data frame containing \code{check_msgs}.
#' @param col \code{<tidy-select>} Bare name of the check-message column.
#'   Default \code{check_msgs}.
#'
#' @return A list with two tibbles:
#' \describe{
#'   \item{\code{overview}}{Single-row summary with total, passing,
#'   failing, and pass-rate columns.}
#'   \item{\code{violations}}{Per-message counts and percentages,
#'   sorted by count descending.}
#' }
#'
#' @examples
#' \dontrun{
#' summary <- ks_check_summary(results)
#' summary$overview
#' summary$violations
#' }
#' @importFrom dplyr mutate
#' @importFrom purrr map_lgl
#' @importFrom tibble tibble
#' @export
ks_check_summary <- function(data, col = check_msgs) {
  check_col <- dplyr::mutate(data, .ks_msgs = {{ col }})
  rows_total <- nrow(check_col)

  has_ok <- purrr::map_lgl(check_col$.ks_msgs, ~ "OK" %in% .x)
  n_pass <- sum(has_ok)
  n_fail <- rows_total - n_pass
  pass_rate <- if (rows_total == 0L) 1 else n_pass / rows_total

  overview <- tibble::tibble(
    rows_total = rows_total,
    n_pass = n_pass,
    n_fail = n_fail,
    pass_rate = pass_rate
  )

  if (rows_total == 0L || n_fail == 0L) {
    violations <- tibble::tibble(
      message = character(),
      n_rows = integer(),
      pct_rows = numeric()
    )
    return(list(overview = overview, violations = violations))
  }

  msg_lists <- check_col$.ks_msgs[!has_ok]
  flat <- unlist(lapply(msg_lists, unique), use.names = FALSE)

  if (length(flat) == 0L) {
    violations <- tibble::tibble(
      message = character(),
      n_rows = integer(),
      pct_rows = numeric()
    )
    return(list(overview = overview, violations = violations))
  }

  counts <- sort(table(flat), decreasing = TRUE)
  violations <- tibble::tibble(
    message = names(counts),
    n_rows = as.integer(counts),
    pct_rows = as.integer(counts) / rows_total
  )

  list(overview = overview, violations = violations)
}

#' Render an HTML check report
#'
#' Creates a self-contained HTML report for validation results produced by
#' \code{\link{ks_check_rules}}. The report includes overview KPI cards,
#' a violation summary table, and a row-level detail table.
#'
#' @param data A data frame containing a check-message column.
#' @param path Output file path. Three behaviours:
#'   - \code{NULL} (default): auto-generates a file name in the current
#'     working directory.
#'   - \code{NA}: returns the assembled \code{htmltools::tagList()} without
#'     writing a file.
#'   - character path: writes report to that path (\code{.html} appended
#'     if missing).
#' @param title Report title.
#' @param subtitle Optional subtitle.
#' @param col \code{<tidy-select>} Bare name of the check-message column.
#'   Default \code{check_msgs}.
#' @param max_rows Maximum number of rows shown in the detail table.
#' @param theme One of \code{"default"} or \code{"slate"}.
#'
#' @return Invisibly returns \code{path} (or the assembled tag list when
#'   \code{path = NA}).
#'
#' @examples
#' \dontrun{
#' checked <- ks_check_rules(adsl, rules, mode = "all")
#'
#' # Write to disk
#' ks_check_report_html(checked, "check-report.html")
#'
#' # In-memory tagList
#' tags <- ks_check_report_html(checked, path = NA)
#' }
#' @importFrom dplyr mutate select
#' @importFrom purrr map_lgl map_chr
#' @export
ks_check_report_html <- function(
  data,
  path = NULL,
  title = "ksCompare check report",
  subtitle = NULL,
  col = check_msgs,
  max_rows = 500L,
  theme = c("default", "slate")
) {
  ks_check_installed("htmltools", reason = "for ks_check_report_html()")
  ks_check_installed("reactable", reason = "for ks_check_report_html()")

  theme <- match.arg(theme)
  page <- ks_check_html_page(
    data = data,
    col = {{ col }},
    title = title,
    subtitle = subtitle,
    max_rows = as.integer(max_rows),
    theme = theme
  )

  if (length(path) == 1L && is.na(path)) {
    return(page)
  }

  if (is.null(path)) {
    path <- file.path(getwd(), paste0("ksCompare_check_report_", format(Sys.Date(), "%Y%m%d"), ".html"))
  } else {
    if (!is.character(path) || length(path) != 1L) {
      ks_abort("{.arg path} must be {.code NULL}, {.code NA}, or a single file path.")
    }
    if (!nzchar(tools::file_ext(path))) {
      path <- paste0(path, ".html")
    }
  }

  ks_ensure_dir(dirname(path))
  ks_save_html_self_contained(page, path)
  cli::cli_alert_success("HTML check report written to {.file {path}}")
  invisible(path)
}

ks_check_html_page <- function(data, col, title, subtitle, max_rows, theme) {
  check_df <- dplyr::mutate(data, .ks_msgs = {{ col }})
  summary <- ks_check_summary(check_df, col = .ks_msgs)

  overview <- summary$overview
  violations <- summary$violations

  rows_total <- overview$rows_total[[1L]]
  n_pass <- overview$n_pass[[1L]]
  n_fail <- overview$n_fail[[1L]]
  pass_rate <- overview$pass_rate[[1L]]

  detail_df <- check_df %>%
    dplyr::mutate(
      .ks_status = ifelse(purrr::map_lgl(.ks_msgs, ~ "OK" %in% .x), "OK", "Issue"),
      .ks_msgs_chr = purrr::map_chr(.ks_msgs, ~ paste(.x, collapse = "; "))
    ) %>%
    dplyr::select(-.ks_msgs)

  detail_cap <- max(0L, as.integer(max_rows))
  detail_shown <- min(nrow(detail_df), detail_cap)
  detail_view <- utils::head(detail_df, detail_cap)

  kpi <- function(label, value, kind = "neutral", hint = NULL) {
    htmltools::tags$div(
      class = paste0("ks-kpi ks-kpi-", kind),
      htmltools::tags$div(
        class = "ks-kpi-value",
        if (is.character(value)) value else format(value, big.mark = ",")
      ),
      htmltools::tags$div(class = "ks-kpi-label", label),
      if (!is.null(hint)) htmltools::tags$div(class = "ks-kpi-hint", hint)
    )
  }

  status_badge <- if (n_fail == 0L) {
    htmltools::tags$div(
      class = "ks-status ks-status-pass",
      htmltools::tags$span(class = "ks-status-dot"),
      "All checks passed"
    )
  } else {
    htmltools::tags$div(
      class = "ks-status ks-status-diff",
      htmltools::tags$span(class = "ks-status-dot"),
      "Violations detected"
    )
  }

  violation_table <- if (nrow(violations) == 0L) {
    htmltools::tags$p(class = "ks-muted", "No violations found.")
  } else {
    reactable::reactable(
      violations,
      columns = list(
        message = reactable::colDef(name = "Message", minWidth = 280),
        n_rows = reactable::colDef(name = "Rows", align = "right"),
        pct_rows = reactable::colDef(
          name = "% of rows",
          align = "right",
          format = reactable::colFormat(percent = TRUE, digits = 1)
        )
      ),
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      defaultPageSize = min(10L, max(1L, nrow(violations))),
      pageSizeOptions = c(10L, 25L, 50L),
      searchable = TRUE
    )
  }

  detail_table <- reactable::reactable(
    detail_view,
    columns = list(
      .ks_status = reactable::colDef(
        name = "Status",
        cell = function(value) {
          cls <- if (identical(value, "OK")) "ks-chip ks-chip-ok" else "ks-chip ks-chip-bad"
          htmltools::tags$span(class = cls, value)
        }
      ),
      .ks_msgs_chr = reactable::colDef(name = "check_msgs", minWidth = 320)
    ),
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    defaultPageSize = min(25L, max(1L, detail_shown)),
    pageSizeOptions = c(25L, 50L, 100L),
    searchable = TRUE
  )

  htmltools::tags$html(
    lang = "en",
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(ks_html_css(theme))),
      htmltools::tags$style(htmltools::HTML(ks_check_html_css()))
    ),
    htmltools::tags$body(
      class = paste0("ks-theme-", theme),
      htmltools::tags$main(
        class = "ks-main ks-check-main",
        htmltools::tags$header(
          class = "ks-header",
          htmltools::tags$div(
            class = "ks-header-titles",
            htmltools::tags$h1(title),
            if (!is.null(subtitle)) htmltools::tags$p(class = "ks-subtitle", subtitle)
          ),
          status_badge
        ),
        htmltools::tags$div(
          class = "ks-kpis",
          kpi("Rows checked", rows_total, "ok"),
          kpi("Passing rows", n_pass, if (n_fail == 0L) "ok" else "neutral"),
          kpi("Failing rows", n_fail, if (n_fail == 0L) "ok" else "warn"),
          kpi("Pass rate", sprintf("%.1f%%", 100 * pass_rate), if (n_fail == 0L) "ok" else "warn")
        ),
        htmltools::tags$section(
          class = "ks-section",
          htmltools::tags$div(
            class = "ks-section-head",
            htmltools::tags$h2("Violation summary"),
            htmltools::tags$span(class = "ks-pill ks-pill-warn", format(nrow(violations), big.mark = ","), " messages")
          ),
          violation_table
        ),
        htmltools::tags$section(
          class = "ks-section",
          htmltools::tags$div(
            class = "ks-section-head",
            htmltools::tags$h2("Row detail"),
            htmltools::tags$span(
              class = "ks-pill ks-pill-ok",
              sprintf("showing %s of %s rows", format(detail_shown, big.mark = ","), format(nrow(detail_df), big.mark = ","))
            )
          ),
          detail_table
        ),
        htmltools::tags$footer(
          class = "ks-footer",
          sprintf("Generated %s by ksCompare %s.", format(Sys.time()), utils::packageVersion("ksCompare"))
        )
      )
    )
  )
}

ks_check_html_css <- function() {
  paste(
    ".ks-check-main { max-width: 1280px; margin-left: auto; margin-right: auto; }",
    ".ks-chip { display: inline-block; padding: 2px 8px; border-radius: 999px; font-weight: 600; font-size: 12px; }",
    ".ks-chip-ok { background: #e9f8ee; color: #1f7a3f; border: 1px solid #bce7cb; }",
    ".ks-chip-bad { background: #fff1f0; color: #b42318; border: 1px solid #f5c2be; }",
    ".ks-muted { color: #6b7280; margin: 4px 0 0; }",
    sep = "\n"
  )
}