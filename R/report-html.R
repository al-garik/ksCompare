#' Render a self-contained HTML report
#'
#' Produces a polished, single-file HTML report summarising a
#' `ks_comparison`. The output uses `htmltools` for layout and `reactable`
#' for interactive tables. No internet access, no Quarto, and no Pandoc
#' are required.
#'
#' Layout: a left-hand sticky table of contents, a header with dataset
#' names, a status pill, KPI cards, then sections for schema, row diff,
#' value diff, patterns, and the run manifest. Tables use
#' friendly column labels, column groups, and `tick`/`cross` markers
#' instead of raw `TRUE`/`FALSE`.
#'
#' @param x A `ks_comparison` object.
#' @param path File path for the report. Three behaviours:
#'   - missing / `NULL` (default): a filename is auto-generated from the
#'     comparison's `base_name` (and, when distinct, `comp_name`)
#'     written into `options$path` (if set on
#'     [ks_comp_options()]) or the current working directory, e.g.
#'     `ksCompare_adsl_vs_adsl_qc.html`.
#'   - `NA`: nothing is written; the assembled `htmltools::tagList()` is
#'     returned for embedding in another document.
#'   - a character path: written to that path. A `.html` extension is
#'     appended if missing. A bare filename (no directory component) is
#'     resolved relative to `options$path` when that has been set on
#'     [ks_comp_options()], otherwise relative to the working directory.
#' @param title Title shown at the top of the report.
#' @param subtitle Optional subtitle (e.g. study identifier or run id).
#' @param max_rows Per-table row cap (default `100`). When the value-diff
#'   table exceeds this cap, the report shows a *smart stratified sample*
#'   covering each column and each distinct diff-cause (`note`), prioritising
#'   the largest numeric magnitudes (at least one example per column and per
#'   cause is always retained). A notice indicates the sample size and
#'   recommends `as_tibble(cmp)` for the full set. `as_tibble(x)` is never
#'   truncated.
#' @param theme One of `"default"` (steel blue) or `"slate"` (neutral
#'   dark headers on light background).
#' @param group_by_key Logical (default `FALSE`). When `TRUE` and a `by =`
#'   key was used in [ks_compare()], the value-diff section is rendered as
#'   one collapsed `<details>` block per key value, sorted by number of
#'   diffs (most-affected first). For `dup_keys = "keep_all"` /
#'   `"all_pairs"`, each block also shows a *Pair* column so you can tell
#'   pairs apart. Silently falls back to the flat table when no `by =` was
#'   supplied (position match).
#' @param max_groups Maximum number of key-value blocks to render when
#'   `group_by_key = TRUE` (default `200`). Excess groups are summarised in
#'   a closing notice.
#'
#' @return Invisibly returns `path` (or the assembled tag list when
#'   `path = NULL`).
#'
#' @examples
#' \dontrun{
#'   cmp <- ks_compare(iris, iris, by = "Species")
#'
#'   # Explicit path
#'   ks_report_html(cmp, "report.html", title = "ADSL QC")
#'
#'   # Auto-generated filename in working directory
#'   ks_report_html(cmp)
#'
#'   # In-memory tagList for embedding
#'   tags <- ks_report_html(cmp, path = NA)
#' }
#' @export
ks_report_html <- function(
  x,
  path = NULL,
  title = "ksCompare report",
  subtitle = NULL,
  max_rows = 100L,
  theme = c("default", "slate"),
  group_by_key = FALSE,
  max_groups = 200L
) {
  ks_assert_comparison(x)
  ks_check_installed("htmltools", reason = "for ks_report_html()")
  ks_check_installed("reactable", reason = "for ks_report_html()")
  theme <- match.arg(theme)

  page <- ks_html_page(
    x,
    title = title,
    subtitle = subtitle,
    max_rows = max_rows,
    theme = theme,
    group_by_key = isTRUE(group_by_key),
    max_groups = as.integer(max_groups)
  )

  if (length(path) == 1L && is.na(path)) {
    return(page)
  }
  if (is.null(path)) {
    path <- ks_default_report_path(x, ext = "html")
  } else {
    if (!is.character(path) || length(path) != 1L) {
      ks_abort("{.arg path} must be {.code NULL}, {.code NA}, or a single file path.")
    }
    if (!nzchar(tools::file_ext(path))) {
      path <- paste0(path, ".html")
    }
    path <- ks_resolve_output_path(path, x)
  }
  ks_ensure_dir(dirname(path))
  ks_save_html_self_contained(page, path)
  cli::cli_alert_success("HTML report written to {.file {path}}")
  invisible(path)
}

#' Internal: write an `htmltools` tag list to a single self-contained HTML file
#'
#' Unlike `htmltools::save_html()`, which copies each `htmlDependency`'s
#' files into a sibling `lib/` directory, this helper inlines every CSS
#' and JS dependency directly into the document via `<style>` and
#' `<script>` blocks. The result is a single portable file with no
#' adjacent assets, which keeps `ks_report_html()` outputs tidy when many
#' reports share an output folder.
#'
#' Image/font assets referenced from inlined CSS via `url(...)` are
#' converted to `data:` URIs when the referenced file exists alongside
#' the stylesheet; otherwise the URL is left as-is.
#'
#' @keywords internal
#' @noRd
ks_save_html_self_contained <- function(tags, path) {
  rendered <- htmltools::renderTags(tags)
  deps <- rendered$dependencies %||% list()

  inlined <- vapply(
    deps,
    function(dep) ks_inline_dependency(dep),
    character(1L)
  )
  head_blob <- paste(c(rendered$head, inlined), collapse = "\n")

  doc <- as.character(rendered$html)

  # Splice head_blob into <head> using literal string ops. We avoid
  # sub()/gsub() here because the replacement string interprets
  # backreferences (`\1`, `\\`, ...), and inlined JS/CSS bundles routinely
  # contain those sequences, which would corrupt the output and prematurely
  # close script tags.
  doc <- ks_inject_into_head(doc, head_blob)

  out <- paste0("<!DOCTYPE html>\n", doc)
  writeLines(enc2utf8(out), path, useBytes = TRUE)
  invisible(path)
}

# Insert `extra` immediately before </head>. If the document has <head>
# but no closing tag, append after <head>. If it has neither, wrap the
# whole document in a minimal <html><head>...</head>...</html> shell.
ks_inject_into_head <- function(doc, extra) {
  close_pos <- regexpr("</head\\s*>", doc, ignore.case = TRUE, perl = TRUE)
  if (close_pos > 0L) {
    return(paste0(
      substr(doc, 1L, close_pos - 1L),
      extra, "\n",
      substr(doc, close_pos, nchar(doc))
    ))
  }
  open_match <- regexpr("<head\\b[^>]*>", doc, ignore.case = TRUE, perl = TRUE)
  if (open_match > 0L) {
    open_end <- open_match + attr(open_match, "match.length") - 1L
    return(paste0(
      substr(doc, 1L, open_end),
      "\n", extra, "\n</head>",
      substr(doc, open_end + 1L, nchar(doc))
    ))
  }
  paste0("<html>\n<head>\n", extra, "\n</head>\n<body>\n", doc, "\n</body>\n</html>")
}

ks_inline_dependency <- function(dep) {
  src <- dep$src$file %||% dep$src
  if (is.null(src) || !is.character(src) || !nzchar(src)) return("")

  out <- character()

  # Stylesheets ----------------------------------------------------------
  for (css in as.character(dep$stylesheet %||% character())) {
    f <- file.path(src, css)
    if (!file.exists(f)) next
    txt <- ks_read_text(f)
    txt <- ks_inline_css_urls(txt, dirname(f))
    txt <- gsub("</style", "<\\\\/style", txt, ignore.case = TRUE)
    out <- c(out, paste0("<style>\n", txt, "\n</style>"))
  }

  # Scripts --------------------------------------------------------------
  for (js in as.character(dep$script %||% character())) {
    f <- file.path(src, js)
    if (!file.exists(f)) next
    txt <- ks_read_text(f)
    # Prevent premature script termination. HTML5 ends a <script> on
    # </script> case-insensitively, and "<!--" can switch the parser
    # into script-data-escaped state, so neutralise both.
    txt <- gsub("</script", "<\\\\/script", txt, ignore.case = TRUE)
    txt <- gsub("<!--", "<\\\\!--", txt, fixed = TRUE)
    out <- c(out, paste0("<script>\n", txt, "\n</script>"))
  }

  # Free-form <head> markup attached to the dependency -------------------
  if (!is.null(dep$head) && nzchar(dep$head)) {
    out <- c(out, as.character(dep$head))
  }

  paste(out, collapse = "\n")
}

ks_read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Replace url(...) refs in CSS with data: URIs so we don't need any
# sibling files. Leaves remote URLs and unresolved paths untouched.
ks_inline_css_urls <- function(css, base_dir) {
  re <- "url\\(\\s*(['\"]?)([^'\")]+)\\1\\s*\\)"
  m <- gregexpr(re, css, perl = TRUE)
  hits <- regmatches(css, m)[[1L]]
  if (length(hits) == 0L) return(css)
  replaced <- vapply(hits, function(token) {
    url <- sub(re, "\\2", token, perl = TRUE)
    if (grepl("^(data:|https?:|//)", url)) return(token)
    f <- file.path(base_dir, url)
    if (!file.exists(f)) return(token)
    mime <- ks_guess_mime(f)
    enc <- base64enc::base64encode(f)
    sprintf("url(\"data:%s;base64,%s\")", mime, enc)
  }, character(1L))
  regmatches(css, m) <- list(replaced)
  css
}

ks_guess_mime <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    woff  = "font/woff",
    woff2 = "font/woff2",
    ttf   = "font/ttf",
    otf   = "font/otf",
    eot   = "application/vnd.ms-fontobject",
    svg   = "image/svg+xml",
    png   = "image/png",
    jpg   = "image/jpeg",
    jpeg  = "image/jpeg",
    gif   = "image/gif",
    webp  = "image/webp",
    "application/octet-stream"
  )
}

#' Internal: generate a default report file name from a `ks_comparison`
#'
#' Filename is `ksCompare_<base>_vs_<comp>.<ext>` (or
#' `ksCompare_<base>.<ext>` when names match), placed in the
#' comparison's `options$path` folder when set, otherwise the current
#' working directory. Filenames are sanitized for safe filesystem use.
#'
#' @keywords internal
#' @noRd
ks_default_report_path <- function(x, ext = "html") {
  bn <- ks_sanitize_filename(x$meta$base_name %||% "base")
  cn <- ks_sanitize_filename(x$meta$comp_name %||% "comp")
  stem <- if (identical(bn, cn) || !nzchar(cn)) {
    paste0("ksCompare_", bn)
  } else {
    paste0("ksCompare_", bn, "_vs_", cn)
  }
  dir <- ks_options_path(x) %||% getwd()
  file.path(dir, paste0(stem, ".", ext))
}

#' Internal: resolve a user-supplied report path against `options$path`
#'
#' If `path` is absolute (or has an explicit directory component) it is
#' returned unchanged. Otherwise it is resolved relative to the
#' comparison's `options$path`, falling back to the current working
#' directory when that option was not set.
#'
#' @keywords internal
#' @noRd
ks_resolve_output_path <- function(path, x) {
  dir <- ks_options_path(x)
  if (is.null(dir)) return(path)
  # leave caller-supplied directory components alone
  has_dir <- grepl("[\\\\/]", path)
  if (has_dir) return(path)
  file.path(dir, path)
}

#' Internal: pull the output folder set on `ks_comp_options()`
#' @keywords internal
#' @noRd
ks_options_path <- function(x) {
  opts <- x$options
  if (is.null(opts)) return(NULL)
  p <- opts$path
  if (is.null(p) || !nzchar(p)) NULL else p
}

#' Internal: ensure a directory exists (create recursively if needed)
#' @keywords internal
#' @noRd
ks_ensure_dir <- function(dir) {
  if (is.null(dir) || identical(dir, "") || identical(dir, ".")) {
    return(invisible(NULL))
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(NULL)
}

ks_sanitize_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("[\\\\/]+", "_", x)            # path separators
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)    # other unsafe chars
  x <- gsub("_+", "_", x)
  x <- gsub("^[._-]+|[._-]+$", "", x)
  if (!nzchar(x)) x <- "report"
  x
}

# ---- top-level layout -------------------------------------------------------

ks_html_page <- function(x, title, subtitle, max_rows, theme,
                         group_by_key = FALSE, max_groups = 200L) {
  s <- summary(x)
  bn <- x$meta$base_name %||% "base"
  cn <- x$meta$comp_name %||% "comp"

  status <- ks_html_status(s, x)
  sections <- ks_html_sections(
    x, s, max_rows, bn, cn,
    group_by_key = group_by_key,
    max_groups = max_groups
  )

  toc_items <- lapply(sections, function(sec) {
    htmltools::tags$li(
      htmltools::tags$a(
        href = paste0("#", sec$id),
        htmltools::tags$span(class = "ks-toc-label", sec$title),
        if (!is.null(sec$count)) {
          htmltools::tags$span(
            class = paste0(
              "ks-toc-count",
              if (isTRUE(sec$count > 0L)) " ks-toc-count-warn" else ""
            ),
            format(sec$count, big.mark = ",")
          )
        }
      )
    )
  })

  body_sections <- lapply(sections, function(sec) {
    htmltools::tags$section(
      id = sec$id,
      class = "ks-section",
      htmltools::tags$div(
        class = "ks-section-head",
        htmltools::tags$h2(sec$title),
        if (!is.null(sec$count)) {
          htmltools::tags$span(
            class = paste0(
              "ks-pill ",
              if (isTRUE(sec$count > 0L)) "ks-pill-warn" else "ks-pill-ok"
            ),
            sprintf(
              "%s %s",
              format(sec$count, big.mark = ","),
              sec$count_label %||% "rows"
            )
          )
        },
        if (!is.null(sec$blurb)) {
          htmltools::tags$p(
            class = "ks-section-blurb",
            htmltools::HTML(ks_md_inline_code(sec$blurb))
          )
        }
      ),
      sec$body
    )
  })

  htmltools::tags$html(
    lang = "en",
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(ks_html_css(theme)))
    ),
    htmltools::tags$body(
      class = paste0("ks-theme-", theme),
      htmltools::tags$nav(
        class = "ks-toc",
        htmltools::tags$div(class = "ks-toc-brand", "ksCompare"),
        htmltools::tags$ul(toc_items)
      ),
      htmltools::tags$main(
        class = "ks-main",
        htmltools::tags$header(
          class = "ks-header",
          htmltools::tags$div(
            class = "ks-header-titles",
            htmltools::tags$h1(title),
            if (!is.null(subtitle)) {
              htmltools::tags$p(class = "ks-subtitle", subtitle)
            }
          ),
          status
        ),
        ks_html_dataset_block(x, bn, cn),
        ks_html_verdict_block(x),
        ks_html_recommendations_block(x),
        ks_html_kpi_block(s, x),
        body_sections,
        htmltools::tags$footer(
          class = "ks-footer",
          sprintf(
            "Generated %s by ksCompare %s.",
            x$manifest$created_at %||% format(Sys.time()),
            x$manifest$package_version %||%
              utils::packageVersion("ksCompare")
          )
        )
      )
    )
  )
}

# ---- header / KPI blocks ----------------------------------------------------

ks_html_status <- function(s, x) {
  schema_diffs <- ks_count_schema_diffs(x, s)
  unmatched <- s$n_base_only_rows + s$n_comp_only_rows
  clean <- s$n_value_diffs == 0L &&
    schema_diffs == 0L &&
    unmatched == 0L
  if (clean) {
    htmltools::tags$div(
      class = "ks-status ks-status-pass",
      htmltools::tags$span(class = "ks-status-dot"),
      "Clean"
    )
  } else {
    htmltools::tags$div(
      class = "ks-status ks-status-diff",
      htmltools::tags$span(class = "ks-status-dot"),
      "Differences detected"
    )
  }
}

ks_count_schema_diffs <- function(x, s) {
  sd <- x$schema_diff
  type_mismatch <- if (is.null(sd) || nrow(sd) == 0L) {
    0L
  } else {
    sum(
      !is.na(sd$kind_base) &
        !is.na(sd$kind_comp) &
        sd$kind_base != sd$kind_comp,
      na.rm = TRUE
    )
  }
  s$n_base_only_columns + s$n_comp_only_columns + type_mismatch
}

ks_html_dataset_block <- function(x, bn, cn) {
  card <- function(label, name, dim, kind) {
    htmltools::tags$div(
      class = paste0("ks-dscard ks-dscard-", kind),
      htmltools::tags$div(class = "ks-dscard-label", label),
      htmltools::tags$div(class = "ks-dscard-name", name),
      htmltools::tags$div(
        class = "ks-dscard-dim",
        sprintf(
          "%s rows \u00d7 %s columns",
          format(dim[[1]] %||% NA, big.mark = ","),
          dim[[2]] %||% NA
        )
      )
    )
  }
  htmltools::tags$div(
    class = "ks-datasets",
    card("Base", bn, c(x$meta$n_base_rows, x$meta$n_base_cols), "base"),
    htmltools::tags$div(class = "ks-arrow", htmltools::HTML("&harr;")),
    card("Compare", cn, c(x$meta$n_comp_rows, x$meta$n_comp_cols), "comp")
  )
}

ks_html_kpi_block <- function(s, x = NULL) {
  kpi <- function(label, value, kind = "neutral", hint = NULL) {
    htmltools::tags$div(
      class = paste0("ks-kpi ks-kpi-", kind),
      htmltools::tags$div(
        class = "ks-kpi-value",
        if (is.character(value)) value else format(value, big.mark = ",")
      ),
      htmltools::tags$div(class = "ks-kpi-label", label),
      if (!is.null(hint)) {
        htmltools::tags$div(class = "ks-kpi-hint", hint)
      }
    )
  }
  v_kind <- if (s$n_value_diffs == 0L) "ok" else "warn"
  rb_kind <- if (s$n_base_only_rows == 0L) "ok" else "warn"
  rc_kind <- if (s$n_comp_only_rows == 0L) "ok" else "warn"
  schema_n <- s$n_base_only_columns + s$n_comp_only_columns
  schema_kind <- if (schema_n == 0L) "ok" else "warn"
  health <- if (!is.null(x)) tryCatch(ks_match_health(x), error = function(e) NULL) else NULL
  density_kpi <- if (!is.null(health)) {
    dd <- health$diff_density
    pct <- if (is.na(dd) || dd == 0) "0%" else if (dd < 0.0001) "<0.01%" else sprintf("%.2f%%", 100 * dd)
    sev <- if (is.na(dd) || dd == 0) "ok" else if (dd > 0.10) "warn" else "neutral"
    kpi("Diff density", pct, sev, "diff cells / matched cells")
  } else NULL
  htmltools::tags$div(
    class = "ks-kpis",
    kpi("Matched rows", s$n_matched_rows, "ok"),
    kpi("Base-only rows", s$n_base_only_rows, rb_kind, "in base, not in compare"),
    kpi("Comp-only rows", s$n_comp_only_rows, rc_kind, "in compare, not in base"),
    kpi("Matched columns", s$n_matched_columns, "ok"),
    kpi("Schema diffs", schema_n, schema_kind, "unmatched columns"),
    kpi("Value diffs", s$n_value_diffs, v_kind, "cells that differ"),
    kpi(
      "Columns w/ diffs",
      s$n_columns_with_diffs,
      v_kind,
      "distinct columns affected"
    ),
    density_kpi
  )
}

# ---- sections ---------------------------------------------------------------

ks_html_sections <- function(x, s, max_rows, bn, cn,
                              group_by_key = FALSE, max_groups = 200L) {
  schema_n     <- ks_count_schema_diffs(x, s)
  row_hotspots <- ks_row_diff_summary(x)      # computed once; reused for count and body
  diff_causes  <- ks_cause_summary(x)         # computed once; reused for count and body
  list(
    list(
      id = "schema",
      title = "Schema",
      count = schema_n,
      count_label = "diffs",
      blurb = "Per-column type, label and format on each side. Mismatches are flagged.",
      body = ks_html_table_schema(x$schema_diff, max_rows, bn, cn)
    ),
    list(
      id = "row-matching",
      title = "Row matching",
      count = NULL,
      blurb = "How rows are paired between the two datasets: key columns, duplicate handling, and a sample of duplicated keys (when present).",
      body = ks_html_row_matching(x$meta$matching, x)
    ),
    list(
      id = "unmatched-rows",
      title = "Unmatched rows",
      count = if (is.null(x$unmatched_rows)) 0L else nrow(x$unmatched_rows),
      count_label = "rows",
      blurb = "Full base-only / comp-only rows (capped by `max_unmatched_rows` on `ks_compare()`).",
      body = ks_html_table_unmatched(x$unmatched_rows, max_rows, bn, cn)
    ),
    list(
      id = "columns-with-diffs",
      title = "Columns with differences",
      count = s$n_columns_with_diffs,
      count_label = "columns",
      blurb = "Per-column summary: counts, the dominant cause of difference, and (for numeric columns) magnitude statistics. Click a column name to filter the value-diff table below.",
      body = ks_html_table_column_summary(x$value_diff, s)
    ),
    list(
      id = "diff-causes",
      title = "Diff causes",
      count = nrow(diff_causes),
      count_label = "causes",
      blurb = "Taxonomy of *why* cells differ (case-fold, whitespace, NA flow, magnitude, ...). One row per recognised cause across all matched columns.",
      body = ks_html_table_causes(diff_causes, max_rows)
    ),
    list(
      id = "row-hotspots",
      title = "Most-affected rows",
      count = nrow(row_hotspots),
      count_label = "rows",
      blurb = "Observations with the highest number of cell differences. Often pinpoints wrong row matches or systemic shifts on a single record. `base_row` / `comp_row` reference the original 1-based row index in `base` / `comp`.",
      body = ks_html_table_row_hotspots(utils::head(row_hotspots, max_rows), max_rows)
    ),
    list(
      id = "values",
      title = "Value differences",
      count = s$n_value_diffs,
      count_label = "diffs",
      blurb = ks_values_blurb(x, group_by_key),
      body = ks_html_values_section(x, max_rows, group_by_key, max_groups)
    ),
    list(
      id = "patterns",
      title = "Patterns",
      count = if (is.null(x$pattern_summary)) 0L else nrow(x$pattern_summary),
      count_label = "found",
      blurb = "Recurring shapes detected across diffs (e.g. case fold, trailing whitespace, constant offset).",
      body = ks_html_table_patterns(x$pattern_summary, max_rows)
    ),
    list(
      id = "manifest",
      title = "Run manifest",
      count = NULL,
      blurb = "Reproducibility metadata for this comparison.",
      body = ks_html_kv(x$manifest)
    )
  )
}

# ---- per-section renderers --------------------------------------------------

# Row-matching diagnostics: strategy banner, key card, dup table.
ks_html_row_matching <- function(matching, x) {
  if (is.null(matching)) {
    # Backwards-compat fallback for results saved before the matching block
    # was added.
    return(htmltools::tags$p(
      class = "ks-empty",
      htmltools::tags$em("(no matching metadata)")
    ))
  }
  strategy <- matching$strategy %||% "unknown"
  keys <- matching$keys
  has_keys <- !is.null(keys) && nrow(keys) > 0L

  # ---- Strategy banner ----
  banner <- switch(
    strategy,
    position = htmltools::tags$div(
      class = "ks-banner ks-banner-warn",
      htmltools::tags$div(class = "ks-banner-title",
        htmltools::HTML("&#9888;&#65039; Row-position matching")),
      htmltools::tags$div(class = "ks-banner-body", htmltools::HTML(
        "No <code>by =</code> key was provided, so rows were paired by their ",
        "physical position (row 1 \u2194 row 1, row 2 \u2194 row 2, \u2026). ",
        "If the two datasets are not in the same order this can produce ",
        "misleading differences. Pass <code>by = \"&lt;key column&gt;\"</code> ",
        "to align rows by identifier instead."
      ))
    ),
    keyed_unique = htmltools::tags$div(
      class = "ks-banner ks-banner-ok",
      htmltools::tags$div(class = "ks-banner-title",
        htmltools::HTML("&#10003; Keyed match (unique on both sides)")),
      htmltools::tags$div(class = "ks-banner-body", htmltools::HTML(
        "Rows are paired by an exact match on the key column(s). ",
        "Both datasets are unique on the key, so there is a 1:1 outer join."
      ))
    ),
    keyed_dup_first = ,
    keyed_dup_last = ,
    keyed_dup_keep_all = ,
    keyed_dup_all_pairs = htmltools::tags$div(
      class = "ks-banner ks-banner-warn",
      htmltools::tags$div(class = "ks-banner-title",
        htmltools::HTML(sprintf(
          "&#9888;&#65039; Keyed match with duplicate keys (strategy: <code>%s</code>)",
          htmltools::htmlEscape(matching$dup_strategy %||% "unknown")
        ))),
      htmltools::tags$div(class = "ks-banner-body",
        ks_dup_strategy_explainer(matching))
    ),
    htmltools::tags$div(
      class = "ks-banner",
      htmltools::tags$div(class = "ks-banner-title", "Row matching strategy"),
      htmltools::tags$div(class = "ks-banner-body",
        htmltools::HTML(htmltools::htmlEscape(strategy)))
    )
  )

  # ---- Key card ----
  key_card <- if (has_keys) {
    rows <- mapply(function(b, c) {
      label <- if (identical(b, c)) {
        htmltools::HTML(sprintf("<code>%s</code>",
          htmltools::htmlEscape(b)))
      } else {
        htmltools::HTML(sprintf(
          "<code>%s</code> \u2194 <code>%s</code>",
          htmltools::htmlEscape(b),
          htmltools::htmlEscape(c)
        ))
      }
      htmltools::tags$li(label)
    }, keys$base, keys$comp, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    htmltools::tags$div(
      class = "ks-keycard",
      htmltools::tags$div(class = "ks-keycard-label", "Key columns"),
      htmltools::tags$ul(class = "ks-keycard-list", rows)
    )
  } else {
    htmltools::tags$div(
      class = "ks-keycard",
      htmltools::tags$div(class = "ks-keycard-label", "Key columns"),
      htmltools::tags$div(class = "ks-keycard-empty",
        htmltools::tags$em("(none \u2014 row-position match)"))
    )
  }

  # ---- Counts strip ----
  bn <- x$meta$base_name %||% "base"
  cn <- x$meta$comp_name %||% "comp"
  counts_card <- function(label, value, kind = "neutral") {
    htmltools::tags$div(
      class = paste0("ks-rm-stat ks-rm-stat-", kind),
      htmltools::tags$div(class = "ks-rm-stat-value",
        format(value %||% 0L, big.mark = ",")),
      htmltools::tags$div(class = "ks-rm-stat-label", label)
    )
  }
  counts <- htmltools::tags$div(
    class = "ks-rm-stats",
    counts_card(sprintf("Rows in %s", bn), x$meta$n_base_rows, "neutral"),
    counts_card(sprintf("Rows in %s", cn), x$meta$n_comp_rows, "neutral"),
    counts_card("Duplicated keys (base)", matching$n_base_dup_keys,
      if ((matching$n_base_dup_keys %||% 0L) > 0L) "warn" else "ok"),
    counts_card("Duplicated keys (comp)", matching$n_comp_dup_keys,
      if ((matching$n_comp_dup_keys %||% 0L) > 0L) "warn" else "ok"),
    if ((matching$n_dropped_base %||% 0L) + (matching$n_dropped_comp %||% 0L) > 0L) {
      counts_card(
        sprintf("Rows dropped (%s/%s)", bn, cn),
        sprintf("%s / %s",
          format(matching$n_dropped_base, big.mark = ","),
          format(matching$n_dropped_comp, big.mark = ",")
        ),
        "warn"
      )
    },
    if ((matching$n_pairs_created %||% 0L) > 0L) {
      counts_card("Pairs created", matching$n_pairs_created, "warn")
    }
  )

  # ---- Dup-keys table ----
  dup_tbl <- if (!is.null(matching$dup_summary) &&
                 nrow(matching$dup_summary) > 0L) {
    ks_html_table_dup_keys(matching$dup_summary, matching$dup_strategy, bn, cn)
  } else {
    NULL
  }

  htmltools::tagList(banner, counts, key_card, dup_tbl)
}

ks_dup_strategy_explainer <- function(matching) {
  ds <- matching$dup_strategy %||% ""
  text <- switch(
    ds,
    first = paste0(
      "On each side, only the <b>first</b> row per key was kept; later ",
      "rows were dropped. Counts of dropped rows appear below."
    ),
    last = paste0(
      "On each side, only the <b>last</b> row per key was kept; earlier ",
      "rows were dropped. Counts of dropped rows appear below."
    ),
    keep_all = paste0(
      "Within each key group, rows were paired <b>positionally</b> ",
      "(1\u21941, 2\u21942, \u2026). Leftover rows on the longer side ",
      "appear as base-only / compare-only."
    ),
    all_pairs = paste0(
      "Every base row was paired with every compare row sharing the same ",
      "key (<b>cartesian product</b> per key). This can inflate the ",
      "matched-row count when cardinalities differ."
    ),
    "Duplicate keys were resolved using a custom strategy."
  )
  htmltools::HTML(text)
}

ks_html_table_dup_keys <- function(dup_summary, strategy, bn, cn) {
  df <- dup_summary
  # Annotate each row with the action taken.
  df$action <- vapply(seq_len(nrow(df)), function(i) {
    nb <- df$n_base[[i]]; nc <- df$n_comp[[i]]
    switch(
      strategy %||% "",
      first = if (nb > 1L && nc > 1L) {
        sprintf("kept first of %d (base) & %d (comp)", nb, nc)
      } else if (nb > 1L) {
        sprintf("kept first of %d (base)", nb)
      } else {
        sprintf("kept first of %d (comp)", nc)
      },
      last = if (nb > 1L && nc > 1L) {
        sprintf("kept last of %d (base) & %d (comp)", nb, nc)
      } else if (nb > 1L) {
        sprintf("kept last of %d (base)", nb)
      } else {
        sprintf("kept last of %d (comp)", nc)
      },
      keep_all = sprintf("paired positionally: %d matched, %d leftover",
        min(nb, nc), abs(nb - nc)),
      all_pairs = sprintf("expanded to %d cartesian pair(s)", nb * nc),
      sprintf("base: %d, comp: %d", nb, nc)
    )
  }, character(1L))

  cols <- list(
    key = reactable::colDef(name = "Key value", minWidth = 160),
    n_base = reactable::colDef(
      name = sprintf("# in %s", bn),
      minWidth = 90,
      align = "right"
    ),
    n_comp = reactable::colDef(
      name = sprintf("# in %s", cn),
      minWidth = 90,
      align = "right"
    ),
    action = reactable::colDef(name = "Action taken", minWidth = 240)
  )
  htmltools::tagList(
    htmltools::tags$div(
      class = "ks-section-blurb",
      htmltools::HTML(sprintf(
        "<b>%s</b> distinct key value(s) were duplicated on at least one side.",
        format(nrow(df), big.mark = ",")
      ))
    ),
    ks_reactable(
      df,
      max_rows = 200L,
      columns = cols,
      default_sorted = list(n_base = "desc")
    )
  )
}

ks_html_table_schema <- function(df, max_rows, bn, cn) {
  if (is.null(df) || nrow(df) == 0L) {
    return(ks_html_empty())
  }
  bn <- bn %||% "base"
  cn <- cn %||% "comp"

  # Make invisible whitespace visible in label / format displays.
  if (!is.null(df$label_base)) df$label_base <- ks_mark_whitespace(df$label_base)
  if (!is.null(df$label_comp)) df$label_comp <- ks_mark_whitespace(df$label_comp)
  if (!is.null(df$format_base)) df$format_base <- ks_mark_whitespace(df$format_base)
  if (!is.null(df$format_comp)) df$format_comp <- ks_mark_whitespace(df$format_comp)

  has_label <- any(nzchar(df$label_base %||% "") | nzchar(df$label_comp %||% ""))
  has_format <- any(nzchar(df$format_base %||% "") | nzchar(df$format_comp %||% ""))

  cols <- list(
    base = reactable::colDef(name = bn, minWidth = 110),
    comp = reactable::colDef(name = cn, minWidth = 110),
    side = reactable::colDef(
      name = "Match",
      minWidth = 100,
      html = TRUE,
      cell = ks_js_side_pill()
    ),
    kind_base = reactable::colDef(name = bn, minWidth = 90),
    kind_comp = reactable::colDef(name = cn, minWidth = 90),
    kind_match = reactable::colDef(
      name = "=",
      minWidth = 50,
      align = "center",
      html = TRUE,
      cell = ks_js_check()
    )
  )
  groups <- list(
    reactable::colGroup(name = "Column", columns = c("base", "comp", "side")),
    reactable::colGroup(
      name = "Type",
      columns = c("kind_base", "kind_comp", "kind_match")
    )
  )
  if (has_label && all(c("label_base", "label_comp", "label_match") %in% names(df))) {
    cols$label_base <- reactable::colDef(name = bn, minWidth = 130)
    cols$label_comp <- reactable::colDef(name = cn, minWidth = 130)
    cols$label_match <- reactable::colDef(
      name = "=",
      minWidth = 60,
      align = "center",
      html = TRUE,
      cell = ks_js_check_with_note("label_diff")
    )
    groups[[length(groups) + 1L]] <- reactable::colGroup(
      name = "Label",
      columns = c("label_base", "label_comp", "label_match")
    )
  } else {
    df$label_base <- NULL
    df$label_comp <- NULL
    df$label_match <- NULL
  }
  if (has_format && all(c("format_base", "format_comp", "format_match") %in% names(df))) {
    cols$format_base <- reactable::colDef(name = bn, minWidth = 110)
    cols$format_comp <- reactable::colDef(name = cn, minWidth = 110)
    cols$format_match <- reactable::colDef(
      name = "=",
      minWidth = 60,
      align = "center",
      html = TRUE,
      cell = ks_js_check_with_note("format_diff")
    )
    groups[[length(groups) + 1L]] <- reactable::colGroup(
      name = "Format",
      columns = c("format_base", "format_comp", "format_match")
    )
  } else {
    df$format_base <- NULL
    df$format_comp <- NULL
    df$format_match <- NULL
  }
  # Hide the diff-note columns from the rendered grid (they are still
  # accessible to the cell renderer via cellInfo.row).
  if ("label_diff" %in% names(df)) {
    cols$label_diff <- reactable::colDef(show = FALSE)
  }
  if ("format_diff" %in% names(df)) {
    cols$format_diff <- reactable::colDef(show = FALSE)
  }

  ks_reactable(
    df,
    max_rows = max_rows,
    columns = cols,
    column_groups = groups,
    default_sorted = list(side = "asc")
  )
}

ks_values_blurb <- function(x, group_by_key) {
  has_keys <- !is.null(x$meta$keys) && nrow(x$meta$keys) > 0L
  if (group_by_key && has_keys) {
    paste0(
      "Cells that differ between matched rows, **grouped by key**. ",
      "Each block below collapses one key value (most-affected first); ",
      "expand to see the columns that changed for that subject. ",
      "`diff` is `base - comp` for numeric columns; the Note column ",
      "highlights subtle causes (whitespace, case, NA vs empty, tiny floats)."
    )
  } else if (group_by_key && !has_keys) {
    paste0(
      "Cells that differ between matched rows. ",
      "(`group_by_key = TRUE` was requested but no `by =` key is set, so ",
      "rows are shown flat.) Use the column selector to focus on one variable; ",
      "the Note column highlights subtle causes."
    )
  } else {
    paste0(
      "Cells that differ between matched rows. `diff` is `base - comp` for ",
      "numeric columns. Use the column selector to focus on one variable; ",
      "the Note column highlights subtle causes (whitespace, case, NA vs empty, ",
      "tiny floats); a middle-dot \u00b7 marks invisible leading/trailing ",
      "whitespace in the values."
    )
  }
}

ks_html_values_section <- function(x, max_rows, group_by_key, max_groups) {
  vd <- x$value_diff
  if (is.null(vd) || nrow(vd) == 0L) {
    return(ks_html_empty("No value differences."))
  }
  has_keys <- !is.null(x$meta$keys) && nrow(x$meta$keys) > 0L
  if (!group_by_key || !has_keys) {
    return(ks_html_table_values(vd, max_rows))
  }
  ks_html_values_grouped(x, max_rows, max_groups)
}

# Grouped value-diff renderer: one collapsed <details> block per key value,
# sorted by # diffs descending. Honours max_groups budget; remainder is
# rolled into a closing notice.
ks_html_values_grouped <- function(x, max_rows, max_groups) {
  vd <- x$value_diff
  rk <- x$meta$row_keys
  if (is.null(rk) || nrow(rk) == 0L) {
    # Saved before row_keys was added \u2014 fall back.
    return(ks_html_table_values(vd, max_rows))
  }
  # Join key_label onto vd by key_id.
  m <- match(vd$key_id, rk$key_id)
  vd$key_label <- rk$key_label[m]
  vd$pair_rank <- rk$pair_rank[m]
  vd$pair_total <- rk$pair_total[m]

  # Drop rows where key_label is NA (defensive; shouldn't happen with keys).
  vd <- vd[!is.na(vd$key_label), , drop = FALSE]
  if (nrow(vd) == 0L) return(ks_html_empty("No value differences."))

  # Per-group counts.
  by_grp <- split(seq_len(nrow(vd)), vd$key_label)
  counts <- vapply(by_grp, length, integer(1L))
  n_cols <- vapply(by_grp, function(idx) length(unique(vd$column_base[idx])),
                   integer(1L))
  ord <- order(-counts, names(by_grp))
  by_grp <- by_grp[ord]
  counts <- counts[ord]
  n_cols <- n_cols[ord]

  total_groups <- length(by_grp)
  shown_groups <- min(total_groups, max_groups)

  # Determine if pair info is meaningful for any shown group.
  any_pairs <- any(vd$pair_total[unlist(by_grp[seq_len(shown_groups)])] > 1L)

  blocks <- lapply(seq_len(shown_groups), function(i) {
    idx <- by_grp[[i]]
    sub <- vd[idx, , drop = FALSE]
    label <- names(by_grp)[[i]]
    n <- counts[[i]]
    nc <- n_cols[[i]]
    ks_html_value_group_block(label, sub, n, nc, max_rows, any_pairs)
  })

  trailer <- if (total_groups > shown_groups) {
    n_hidden <- total_groups - shown_groups
    rows_hidden <- sum(counts[(shown_groups + 1L):total_groups])
    htmltools::tags$div(
      class = "ks-sample-notice",
      htmltools::HTML(sprintf(
        paste0(
          "Showing the top <b>%s</b> most-affected key value(s); ",
          "<b>%s</b> further key(s) covering <b>%s</b> diff row(s) were ",
          "omitted. Increase <code>max_groups</code> or use the Excel ",
          "report for full detail."
        ),
        format(shown_groups, big.mark = ","),
        format(n_hidden, big.mark = ","),
        format(rows_hidden, big.mark = ",")
      ))
    )
  } else {
    NULL
  }

  intro <- htmltools::tags$div(
    class = "ks-section-blurb",
    htmltools::HTML(sprintf(
      "<b>%s</b> key value(s) have at least one difference.",
      format(total_groups, big.mark = ",")
    ))
  )
  htmltools::tagList(intro, blocks, trailer)
}

ks_html_value_group_block <- function(label, sub, n_diffs, n_cols, max_rows,
                                       any_pairs) {
  # Build the small inner table for a single key group.
  cols <- list(
    column_base = reactable::colDef(name = "Column", minWidth = 130),
    base = reactable::colDef(name = "Base", minWidth = 110),
    comp = reactable::colDef(name = "Compare", minWidth = 110),
    diff = reactable::colDef(
      name = "Diff",
      minWidth = 90,
      align = "right",
      html = TRUE,
      cell = ks_js_diff_cell()
    ),
    note = reactable::colDef(
      name = "Note",
      minWidth = 220,
      html = TRUE,
      cell = ks_js_note_cell()
    ),
    # Hide bookkeeping / non-display columns.
    key_id = reactable::colDef(show = FALSE),
    column_comp = reactable::colDef(show = FALSE),
    kind = reactable::colDef(show = FALSE),
    key_label = reactable::colDef(show = FALSE),
    na_flow = reactable::colDef(show = FALSE),
    pair_rank = reactable::colDef(show = FALSE),
    pair_total = reactable::colDef(show = FALSE)
  )
  if (any_pairs) {
    cols$pair_rank <- reactable::colDef(
      name = "Pair",
      minWidth = 70,
      align = "right",
      cell = reactable::JS(
        "function(cellInfo) {
          var t = cellInfo.row['pair_total'];
          if (!t || t <= 1) return '';
          return cellInfo.value + ' / ' + t;
        }"
      ),
      html = TRUE
    )
  }

  # Make whitespace visible at render time.
  sub$base <- ks_mark_whitespace(sub$base)
  sub$comp <- ks_mark_whitespace(sub$comp)

  # Apply the smart sampler so every column with a diff in this group
  # gets at least one example row, even when truncating to max_rows.
  sampled <- ks_smart_sample_value_diff(sub, max_rows)
  sub <- sampled$df
  notice <- if (!sampled$full) {
    htmltools::tags$p(
      class = "ks-warning",
      sprintf(
        "Showing %s of %s rows (\u2265 1 row per column with a difference).",
        format(sampled$sampled, big.mark = ","),
        format(sampled$total, big.mark = ",")
      )
    )
  }

  tbl <- ks_reactable(
    sub,
    max_rows = nrow(sub),
    columns = cols,
    searchable = FALSE,
    default_sorted = list(column_base = "asc")
  )

  pair_chip <- if (any_pairs) {
    npairs <- sub$pair_total[[1L]]
    if (!is.na(npairs) && npairs > 1L) {
      htmltools::tags$span(
        class = "ks-group-chip ks-group-chip-warn",
        sprintf("%d pairs", npairs)
      )
    }
  }

  htmltools::tags$details(
    class = "ks-group",
    htmltools::tags$summary(
      class = "ks-group-summary",
      htmltools::tags$span(class = "ks-group-label", label),
      htmltools::tags$span(
        class = "ks-group-chip",
        sprintf("%s diff%s", format(n_diffs, big.mark = ","),
                if (n_diffs == 1L) "" else "s")
      ),
      htmltools::tags$span(
        class = "ks-group-chip ks-group-chip-neutral",
        sprintf("%d column%s", n_cols, if (n_cols == 1L) "" else "s")
      ),
      pair_chip
    ),
    htmltools::tags$div(class = "ks-group-body", notice, tbl)
  )
}

ks_html_table_values <- function(df, max_rows) {
  if (is.null(df) || nrow(df) == 0L) {
    return(ks_html_empty("No value differences."))
  }
  sample <- ks_smart_sample_value_diff(df, max_rows)
  df <- sample$df

  # Make leading/trailing whitespace visible at render time only.
  df$base <- ks_mark_whitespace(df$base)
  df$comp <- ks_mark_whitespace(df$comp)

  # Stable element id so the external column selector can drive the filter.
  table_id <- paste0("ks-values-", as.integer(stats::runif(1L) * 1e9))
  col_counts <- table(df$column_base, useNA = "no")
  col_options <- names(sort(col_counts, decreasing = TRUE))

  cols <- list(
    key_id = reactable::colDef(name = "Key id", minWidth = 70, align = "right"),
    base_row = reactable::colDef(
      name = "Base row", minWidth = 80, align = "right", na = "\u2013"
    ),
    comp_row = reactable::colDef(
      name = "Comp row", minWidth = 80, align = "right", na = "\u2013"
    ),
    column_base = reactable::colDef(
      name = "Base column",
      minWidth = 150,
      filterable = TRUE,
      filterMethod = reactable::JS(
        "function(rows, columnId, filterValue) {
          if (!filterValue) return rows;
          return rows.filter(function(r) { return r.values[columnId] === filterValue; });
        }"
      ),
      # Hide the default text input вЂ” we drive this filter from the external
      # select above the table.
      filterInput = reactable::JS(
        "function() { return null; }"
      )
    ),
    column_comp = reactable::colDef(name = "Compare column", minWidth = 130),
    kind = reactable::colDef(
      name = "Type",
      minWidth = 100,
      html = TRUE,
      cell = ks_js_kind_pill()
    ),
    base = reactable::colDef(
      name = "Base value",
      minWidth = 130,
      style = list(background = "#fff7e6")
    ),
    comp = reactable::colDef(
      name = "Compare value",
      minWidth = 130,
      style = list(background = "#e6f4ea")
    ),
    diff = reactable::colDef(
      name = "Diff (base \u2212 compare)",
      minWidth = 150,
      align = "right",
      html = TRUE,
      cell = ks_js_diff_cell()
    ),
    na_flow = reactable::colDef(show = FALSE),
    note = reactable::colDef(
      name = "Note",
      minWidth = 200,
      html = TRUE,
      cell = ks_js_note_cell()
    )
  )
  groups <- list(
    reactable::colGroup(name = "Location", columns = c("key_id", "base_row", "comp_row", "column_base", "column_comp")),
    reactable::colGroup(name = "Values", columns = c("base", "comp", "diff"))
  )
  tbl <- ks_reactable(
    df,
    max_rows = nrow(df) + 1L,
    columns = cols,
    column_groups = groups,
    searchable = FALSE,
    element_id = table_id
  )

  selector <- ks_html_column_selector(table_id, col_options, col_counts, nrow(df))

  if (sample$full) {
    return(htmltools::tagList(selector, tbl))
  }
  notice <- htmltools::tags$div(
    class = "ks-sample-notice",
    htmltools::HTML(sprintf(
      paste0(
        "Showing a smart sample of <b>%s</b> rows out of <b>%s</b> total ",
        "value differences (covering <b>%s</b> column%s and <b>%s</b> distinct ",
        "cause%s). The sample picks at least one example per column and per ",
        "diff cause, prioritising the largest numeric magnitudes. Use ",
        "<code>as_tibble(cmp)</code> for the full set."
      ),
      format(sample$sampled, big.mark = ","),
      format(sample$total, big.mark = ","),
      format(sample$n_columns, big.mark = ","),
      if (sample$n_columns == 1L) "" else "s",
      format(sample$n_buckets, big.mark = ","),
      if (sample$n_buckets == 1L) "" else "s"
    ))
  )
  htmltools::tagList(notice, selector, tbl)
}

# Build a `<select>` that drives the value-diff table's column filter via
# Reactable.setFilter().
ks_html_column_selector <- function(table_id, values, counts, total) {
  if (length(values) == 0L) return(NULL)
  options <- c(
    list(htmltools::tags$option(
      value = "",
      sprintf("All columns (%s rows)", format(total, big.mark = ","))
    )),
    lapply(values, function(v) {
      n <- as.integer(counts[[v]] %||% 0L)
      htmltools::tags$option(
        value = v,
        sprintf("%s (%s)", v, format(n, big.mark = ","))
      )
    })
  )
  htmltools::tags$div(
    class = "ks-col-selector",
    htmltools::tags$label(
      `for` = paste0(table_id, "-sel"),
      "Show column:"
    ),
    htmltools::tags$select(
      id = paste0(table_id, "-sel"),
      onchange = sprintf(
        "Reactable.setFilter('%s', 'column_base', this.value || undefined);",
        table_id
      ),
      options
    )
  )
}

#' Internal: pick a stratified sample of value-diff rows when there are
#' more diffs than the report budget.
#'
#' Strategy (tiered):
#'   1. The highest-`|diff|` row per `column_base`. Always kept, even
#'      if `n_columns > max_rows` -- every column with a diff appears
#'      at least once.
#'   2. The highest-`|diff|` row per `(column_base, note)` bucket --
#'      one example of every distinct diff cause within each column.
#'   3. If the budget still has room, fill with the remaining rows in
#'      `|diff|`-desc order.
#'
#' @return A list with `df`, `full` (logical), `total`, `sampled`,
#'   `n_columns`, `n_buckets`.
#' @keywords internal
#' @noRd
ks_smart_sample_value_diff <- function(df, max_rows) {
  n <- nrow(df)
  cols <- df$column_base
  notes <- ifelse(is.na(df$note), "(no-note)", df$note)
  bucket <- paste0(cols, "\u0001", notes)
  n_columns <- length(unique(cols))
  n_buckets <- length(unique(bucket))

  if (n <= max_rows) {
    return(list(
      df = df,
      full = TRUE,
      total = n,
      sampled = n,
      n_columns = n_columns,
      n_buckets = n_buckets
    ))
  }

  absdiff <- suppressWarnings(abs(as.numeric(df$diff)))
  absdiff[is.na(absdiff)] <- 0
  # Global priority: largest |diff| first, then earliest key_id.
  global_ord <- order(-absdiff, df$key_id)

  # Tier 1 (always honoured, even if it exceeds max_rows): the
  # highest-|diff| row per column. Guarantees every column appears
  # at least once in the rendered sample.
  tier1 <- global_ord[!duplicated(cols[global_ord])]
  # Tier 2: the highest-|diff| row per (column, note) bucket -- one
  # example of every distinct diff cause within each column.
  tier2 <- global_ord[!duplicated(bucket[global_ord])]

  picked <- unique(c(tier1, tier2))
  if (length(picked) < max_rows) {
    remainder <- setdiff(global_ord, picked)
    picked <- c(picked, utils::head(remainder, max_rows - length(picked)))
  }
  # If n_columns > max_rows, tier1 alone exceeds the cap; keep it
  # anyway -- coverage trumps the cap.

  out <- df[sort(picked), , drop = FALSE]
  list(
    df = out,
    full = FALSE,
    total = n,
    sampled = nrow(out),
    n_columns = n_columns,
    n_buckets = n_buckets
  )
}

ks_html_table_patterns <- function(df, max_rows) {
  if (is.null(df) || nrow(df) == 0L) {
    return(ks_html_empty("No recurring patterns detected."))
  }
  cols <- list(
    column = reactable::colDef(name = "Column", minWidth = 130),
    pattern = reactable::colDef(
      name = "Pattern",
      minWidth = 140,
      html = TRUE,
      cell = ks_js_pattern_pill()
    ),
    coverage = reactable::colDef(
      name = "Coverage",
      minWidth = 110,
      align = "right",
      format = reactable::colFormat(percent = TRUE, digits = 1)
    ),
    detail = reactable::colDef(name = "Detail", minWidth = 200)
  )
  tbl <- ks_reactable(df, max_rows = max_rows, columns = cols)
  htmltools::tagList(tbl, ks_html_pattern_legend(unique(df$pattern)))
}

ks_pattern_glossary <- function() {
  list(
    constant_offset = "Every diff is the same additive shift (`base - comp` is constant). Detail shows the offset.",
    constant_scale = "Every diff is the same multiplicative factor (`base / comp` is constant). Detail shows the ratio.",
    percentage_scale = "Constant scale of 100 (or 1/100) -- one side stores a fraction, the other a percent.",
    unit_scale = "Constant scale matching a known unit conversion (kg/lb, m/ft, mi/km, etc.). Detail names the conversion.",
    sign_flip = "`base` and `comp` are negatives of each other (`base == -comp`).",
    signed_vs_abs = "One side stores the absolute value of the other.",
    integer_round = "One side equals the rounded value of the other (e.g. `1.7` vs `2`).",
    precision_truncated = "One side is the rounding of the other to a fixed number of decimal places. Detail shows the precision.",
    null_as_zero = "On rows where one side is NA, the other side is 0 -- 'NA filled with zero' upstream.",
    null_as_sentinel = "On rows where one side is NA, the other is a sentinel value such as -999 / 99999998.",
    monotone_drift = "Diff trends with row order (Spearman rho >= 0.9) -- systematic drift, not noise.",
    flag_polarity_swapped = "Logical column with TRUE/FALSE inverted between sides.",
    true_to_na = "TRUE on one side, NA on the other (filter or coercion dropped the flag).",
    false_to_na = "FALSE on one side, NA on the other.",
    epoch_swap = "Date / datetime offset matches a known epoch delta (SAS 1960 vs Unix 1970, Excel 1900 / 1904).",
    tz_hour_offset = "Constant whole-hour offset on a datetime column -- likely DST or timezone.",
    year_offset = "Constant whole-year offset on a date column.",
    midnight_truncation = "Datetime collapsed to date (time component zeroed) on one side.",
    trim_only = "Strings differ only by leading or trailing whitespace.",
    whitespace_only = "Strings differ only in internal whitespace (after collapsing spaces).",
    case_only = "Strings differ only in letter case (e.g. `\"Yes\"` vs `\"yes\"`).",
    unicode_normalization_only = "Strings differ only in Unicode composition (e.g. composed `e\u0301` vs decomposed).",
    punctuation_only = "Strings differ only in punctuation / spacing.",
    prefix_added = "comp side gained a constant prefix not present on base.",
    prefix_removed = "base side has an extra constant prefix not on comp.",
    suffix_added = "comp side gained a constant suffix.",
    suffix_removed = "base side has an extra constant suffix.",
    zero_padded = "One side stores integers as fixed-width zero-padded strings (e.g. `\"007\"` vs `\"7\"`).",
    truncated_to_width = "One side truncated to a fixed character width (e.g. SAS `$8.` format).",
    abbreviation = "One side stores a varying-length prefix of the other (e.g. `\"Female\"` -> `\"F\"`).",
    coded_decode = "Each base value maps consistently to one comp value (code/decode lookup).",
    factor_recoded = "Factor levels appear consistently re-mapped between sides; detail shows `old -> new` pairs.",
    near_match = "String pairs are close edit-distance matches (likely typos).",
    pareto_columns = "A small number of columns explain the bulk of all diff cells.",
    pareto_keys = "A single key_id accounts for a large share of diff cells.",
    paired_columns = "Two columns differ on almost the same rows -- likely linked by a derivation."
  )
}

ks_html_pattern_legend <- function(present) {
  glossary <- ks_pattern_glossary()
  hits <- intersect(names(glossary), present)
  if (length(hits) == 0L) {
    return(NULL)
  }
  items <- lapply(hits, function(p) {
    htmltools::tags$li(
      htmltools::tags$span(class = "ks-pill ks-pill-info", p),
      htmltools::tags$span(
        class = "ks-legend-text",
        htmltools::HTML(ks_md_inline_code(glossary[[p]]))
      )
    )
  })
  htmltools::tags$div(
    class = "ks-legend",
    htmltools::tags$div(class = "ks-legend-title", "Pattern legend"),
    htmltools::tags$ul(class = "ks-legend-list", items)
  )
}

# Convert minimal `code` markdown spans into <code>...</code>; leaves other
# text untouched. Escapes HTML entities first to keep this safe.
ks_md_inline_code <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  gsub("`([^`]+)`", "<code>\\1</code>", s)
}

ks_html_table_unmatched <- function(df, max_rows, bn = "base", cn = "comp") {
  if (is.null(df) || nrow(df) == 0L) {
    return(ks_html_empty("No unmatched rows."))
  }
  truncated <- attr(df, "truncated")
  n_total <- attr(df, "n_total")
  bn <- bn %||% "base"
  cn <- cn %||% "comp"
  side_pill <- reactable::JS(sprintf(
    "function(cell) {
       var v = cell.value;
       if (v === 'base_only') return '<span class=\"ks-pill ks-pill-warn\">%s only</span>';
       if (v === 'comp_only') return '<span class=\"ks-pill ks-pill-warn\">%s only</span>';
       return v == null ? '' : String(v);
     }",
    gsub("'", "\\\\'", bn), gsub("'", "\\\\'", cn)
  ))
  cols <- list(
    side = reactable::colDef(
      name = "Side",
      minWidth = 110,
      html = TRUE,
      cell = side_pill
    ),
    key_id = reactable::colDef(name = "Key id", minWidth = 80, align = "right"),
    key_label = reactable::colDef(name = "Key", minWidth = 180),
    base_row = reactable::colDef(
      name = "Base row", minWidth = 90, align = "right",
      na = "\u2013"
    ),
    comp_row = reactable::colDef(
      name = "Comp row", minWidth = 90, align = "right",
      na = "\u2013"
    )
  )
  tbl <- ks_reactable(df, max_rows = max_rows, columns = cols)
  notice <- NULL
  if (!is.null(truncated) && any(truncated, na.rm = TRUE)) {
    bo_n <- if (is.null(n_total)) NA else n_total[["base"]]
    co_n <- if (is.null(n_total)) NA else n_total[["comp"]]
    notice <- htmltools::tags$p(
      class = "ks-empty",
      sprintf(
        "Showing a capped sample of unmatched rows (%d base-only / %d comp-only in total). Raise `max_unmatched_rows` on `ks_compare()` to retain more.",
        as.integer(bo_n %||% 0L), as.integer(co_n %||% 0L)
      )
    )
  }
  htmltools::tagList(notice, tbl)
}

# Executive verdict card shown above the KPI strip.
ks_html_verdict_block <- function(x) {
  v <- tryCatch(ks_executive_verdict(x), error = function(e) NULL)
  if (is.null(v)) return(NULL)
  pct <- v$pct_match
  sev <- v$severity %||% "ok"
  bar <- if (!is.na(pct)) {
    htmltools::tags$div(
      class = "ks-verdict-bar",
      htmltools::tags$div(
        class = "ks-verdict-bar-fill",
        style = sprintf("width: %.2f%%;", max(0, min(100, pct)))
      )
    )
  } else {
    NULL
  }
  htmltools::tags$div(
    class = paste0("ks-verdict ks-verdict-", sev),
    htmltools::tags$div(class = "ks-verdict-label", "Verdict"),
    htmltools::tags$div(class = "ks-verdict-text", v$headline),
    bar
  )
}

# Recommendations card: "what to look at first" actionable findings.
ks_html_recommendations_block <- function(x) {
  recs <- tryCatch(ks_recommendations(x), error = function(e) NULL)
  if (is.null(recs) || nrow(recs) == 0L) return(NULL)
  items <- lapply(seq_len(nrow(recs)), function(i) {
    sev <- recs$severity[[i]]
    htmltools::tags$li(
      class = paste0("ks-rec-item ks-rec-", sev),
      htmltools::tags$span(class = "ks-rec-badge", toupper(sev)),
      htmltools::tags$div(
        class = "ks-rec-body",
        htmltools::tags$div(class = "ks-rec-title", recs$title[[i]]),
        htmltools::tags$div(class = "ks-rec-msg", recs$message[[i]]),
        if (!is.na(recs$action[[i]])) {
          htmltools::tags$div(class = "ks-rec-action",
                              htmltools::tags$strong("Next: "),
                              recs$action[[i]])
        }
      )
    )
  })
  htmltools::tags$div(
    class = "ks-recs",
    htmltools::tags$div(class = "ks-recs-label", "Recommendations"),
    htmltools::tags$ul(class = "ks-recs-list", items)
  )
}

# Diff causes table.
ks_html_table_causes <- function(df, max_rows) {
  if (is.null(df) || nrow(df) == 0L) {
    return(ks_html_empty("No diff causes (no value differences)."))
  }
  cols <- list(
    cause = reactable::colDef(name = "Cause", minWidth = 220),
    n_cells = reactable::colDef(name = "Cells", minWidth = 80, align = "right"),
    n_columns = reactable::colDef(name = "Columns", minWidth = 90, align = "right"),
    columns = reactable::colDef(name = "Affected columns", minWidth = 240)
  )
  ks_reactable(
    df, max_rows = max_rows, columns = cols,
    default_sorted = list(n_cells = "desc")
  )
}

# Per-row hotspots.
ks_html_table_row_hotspots <- function(df, max_rows) {
  if (is.null(df) || nrow(df) == 0L) {
    return(ks_html_empty("No matched rows with cell differences."))
  }
  cols <- list(
    key_id = reactable::colDef(name = "Key id", minWidth = 80, align = "right"),
    base_row = reactable::colDef(name = "Base row", minWidth = 90, align = "right", na = "\u2013"),
    comp_row = reactable::colDef(name = "Comp row", minWidth = 90, align = "right", na = "\u2013"),
    key_label = reactable::colDef(name = "Key", minWidth = 180, na = "\u2013"),
    n_diffs = reactable::colDef(name = "Diffs", minWidth = 80, align = "right"),
    columns = reactable::colDef(name = "Columns affected", minWidth = 240)
  )
  ks_reactable(
    df, max_rows = max_rows, columns = cols,
    default_sorted = list(n_diffs = "desc")
  )
}

# Per-column summary: counts, dominant cause, magnitude statistics.
ks_html_table_column_summary <- function(value_diff, s) {
  if (is.null(value_diff) || nrow(value_diff) == 0L) {
    return(ks_html_empty("No columns with value differences."))
  }
  vd <- value_diff
  # Split once; derive all per-column aggregates without creating N tiny tibbles.
  col_groups <- split(seq_len(nrow(vd)), vd$column_base)
  cols_u     <- names(col_groups)
  ng         <- length(cols_u)

  kind_v     <- vapply(cols_u, function(col) vd$kind[col_groups[[col]][1L]], character(1L))
  col_comp_v <- vapply(cols_u, function(col) vd$column_comp[col_groups[[col]][1L]], character(1L))
  n_diffs_v  <- vapply(col_groups, length, integer(1L))
  n_rows_v   <- vapply(col_groups, function(idx) length(unique(vd$key_id[idx])), integer(1L))

  is_num_v   <- kind_v %in% c("integer", "double")
  d_all      <- suppressWarnings(as.numeric(vd$diff))
  max_abs_v  <- vapply(seq_len(ng), function(i) {
    if (!is_num_v[i]) return(NA_real_)
    d <- d_all[col_groups[[i]]]
    d <- d[is.finite(d)]
    if (length(d) == 0L) NA_real_ else max(abs(d))
  }, numeric(1L))
  mean_abs_v <- vapply(seq_len(ng), function(i) {
    if (!is_num_v[i]) return(NA_real_)
    d <- d_all[col_groups[[i]]]
    d <- d[is.finite(d)]
    if (length(d) == 0L) NA_real_ else mean(abs(d))
  }, numeric(1L))
  top_cause_v <- vapply(cols_u, function(col) {
    notes <- vd$note[col_groups[[col]]]
    notes <- notes[!is.na(notes)]
    if (length(notes) == 0L) return(NA_character_)
    parts <- trimws(unlist(strsplit(notes, ";\\s*"), use.names = FALSE))
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0L) return(NA_character_)
    tt <- sort(table(parts), decreasing = TRUE)
    sprintf("%s (\u00d7%d)", names(tt)[[1L]], tt[[1L]])
  }, character(1L))

  # Show mapped comp name inline when base and compare column names differ.
  col_display <- ifelse(
    !is.na(col_comp_v) & col_comp_v != cols_u,
    paste0(cols_u, " \u2192 ", col_comp_v),
    cols_u
  )

  out <- tibble::tibble(
    column        = col_display,
    kind          = kind_v,
    n_diffs       = as.integer(n_diffs_v),
    n_rows        = as.integer(n_rows_v),
    max_abs_diff  = max_abs_v,
    mean_abs_diff = mean_abs_v,
    top_cause     = top_cause_v
  )
  out <- out[order(-out$n_diffs), , drop = FALSE]

  cols_def <- list(
    column = reactable::colDef(name = "Column", minWidth = 140),
    kind = reactable::colDef(
      name = "Type",
      minWidth = 100,
      html = TRUE,
      cell = ks_js_kind_pill()
    ),
    n_diffs = reactable::colDef(
      name = "Diffs",
      minWidth = 80,
      align = "right",
      format = reactable::colFormat(separators = TRUE)
    ),
    n_rows = reactable::colDef(
      name = "Rows affected",
      minWidth = 110,
      align = "right",
      format = reactable::colFormat(separators = TRUE)
    ),
    max_abs_diff = reactable::colDef(
      name = "Max |diff|",
      minWidth = 110,
      align = "right",
      format = reactable::colFormat(digits = 6, separators = TRUE)
    ),
    mean_abs_diff = reactable::colDef(
      name = "Mean |diff|",
      minWidth = 110,
      align = "right",
      format = reactable::colFormat(digits = 6, separators = TRUE)
    ),
    top_cause = reactable::colDef(
      name = "Top cause",
      minWidth = 220,
      html = TRUE,
      cell = ks_js_note_cell()
    )
  )
  ks_reactable(
    out,
    max_rows = nrow(out) + 1L,
    columns = cols_def,
    default_sorted = list(n_diffs = "desc")
  )
}

# ---- shared reactable wrapper ----------------------------------------------

ks_reactable <- function(
  df,
  max_rows,
  columns = NULL,
  column_groups = NULL,
  default_sorted = NULL,
  searchable = TRUE,
  filterable = TRUE,
  element_id = NULL
) {
  full_rows <- nrow(df)
  truncated <- full_rows > max_rows
  if (truncated) df <- df[seq_len(max_rows), , drop = FALSE]

  # When per-column filters are enabled we need a stable element id so
  # the select-dropdown filters can call Reactable.setFilter().
  if (isTRUE(filterable) && is.null(element_id)) {
    element_id <- paste0("ks-tbl-", as.integer(stats::runif(1L) * 1e9))
  }

  rt_args <- list(
    data = df,
    bordered = FALSE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    searchable = searchable,
    filterable = filterable,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100),
    theme = reactable::reactableTheme(
      style = list(fontSize = "10.5px"),
      tableStyle = list(border = "none"),
      headerStyle = list(
        background = "#eef2f7",
        color = "#1f2933",
        fontWeight = 700,
        borderBottom = "1px solid #cdd6e0",
        fontSize = "9.5px",
        textTransform = "none"
      ),
      groupHeaderStyle = list(
        background = "#dde4ee",
        color = "#1f2933",
        fontWeight = 700,
        fontSize = "11px",
        textTransform = "uppercase",
        letterSpacing = "0.04em",
        borderBottom = "1px solid #c6cfdb"
      ),
      cellStyle = list(
        fontVariantNumeric = "tabular-nums",
        padding = "2px 5px"
      ),
      searchInputStyle = list(width = "240px")
    )
  )
  if (isTRUE(filterable)) {
    rt_args$defaultColDef <- reactable::colDef(
      filterInput = ks_select_filter(element_id),
      # Custom filter: select-dropdown options arrive with a leading "="
      # sentinel and require an exact match against the cell value;
      # free-text inputs fall back to case-insensitive substring matching.
      filterMethod = reactable::JS(
        "function(rows, columnId, filterValue) {
          if (filterValue == null || filterValue === '') return rows;
          var s = String(filterValue);
          if (s.charAt(0) === '=') {
            var v = s.substring(1);
            return rows.filter(function(r) {
              var cv = r.values[columnId];
              return cv != null && String(cv) === v;
            });
          }
          var needle = s.toLowerCase();
          return rows.filter(function(r) {
            var cv = r.values[columnId];
            return cv != null && String(cv).toLowerCase().indexOf(needle) !== -1;
          });
        }"
      )
    )
  }
  if (!is.null(columns)) rt_args$columns <- columns
  if (!is.null(column_groups)) rt_args$columnGroups <- column_groups
  if (!is.null(default_sorted)) rt_args$defaultSorted <- default_sorted
  if (!is.null(element_id)) rt_args$elementId <- element_id

  rt <- do.call(reactable::reactable, rt_args)

  if (truncated) {
    htmltools::tagList(
      htmltools::tags$p(
        class = "ks-warning",
        sprintf(
          "Truncated to first %s of %s rows.",
          format(max_rows, big.mark = ","),
          format(full_rows, big.mark = ",")
        )
      ),
      rt
    )
  } else {
    rt
  }
}

# ---- JS cell renderers ------------------------------------------------------

# Per-column header filter input. Returns a <select> dropdown of distinct
# values for low-cardinality columns, falling back to a free-text input
# when there are too many distinct values (or when values aren't atomic).
# Uses `Reactable.setFilter()` to drive the table's column filter.
ks_select_filter <- function(table_id) {
  force(table_id)
  function(values, name) {
    set_call <- sprintf(
      "Reactable.setFilter('%s', '%s', event.target.value || undefined)",
      table_id, name
    )
    uv <- tryCatch(unique(values), error = function(e) NULL)
    # Free-text fallback for non-atomic / high-cardinality columns.
    # Uses substring matching via the default filterMethod (see ks_reactable).
    if (is.null(uv) || !is.atomic(uv) || length(uv) == 0L || length(uv) > 50L) {
      return(htmltools::tags$input(
        type = "search",
        class = "ks-col-filter",
        oninput = set_call,
        placeholder = "filter\u2026",
        `aria-label` = sprintf("Filter %s", name)
      ))
    }
    uv <- uv[!is.na(uv)]
    uv_str <- sort(unique(as.character(uv)))
    # Dropdown options carry a leading "=" sentinel so the default
    # filterMethod in ks_reactable() treats the selection as an EXACT
    # match (otherwise "AVAL" would also match "AVALC", "AVAL2", ...).
    htmltools::tags$select(
      class = "ks-col-filter",
      onchange = set_call,
      `aria-label` = sprintf("Filter %s", name),
      htmltools::tags$option(value = "", "All"),
      lapply(uv_str, function(v) {
        htmltools::tags$option(value = paste0("=", v), v)
      })
    )
  }
}

ks_js_check <- function() {
  reactable::JS(
    "function(cellInfo) {
      if (cellInfo.value === true || cellInfo.value === 'TRUE' || cellInfo.value === 'true') {
        return '<span class=\"ks-tick\" title=\"match\">\u2713</span>';
      }
      if (cellInfo.value === false || cellInfo.value === 'FALSE' || cellInfo.value === 'false') {
        return '<span class=\"ks-cross\" title=\"differs\">\u2717</span>';
      }
      return '<span class=\"ks-dim\">-</span>';
    }"
  )
}

# Like ks_js_check, but if the underlying value is FALSE the tooltip shows
# the diff explanation pulled from another column in the same row
# (e.g. "label_diff").
ks_js_check_with_note <- function(note_col) {
  reactable::JS(sprintf(
    "function(cellInfo) {
      var v = cellInfo.value;
      if (v === true || v === 'TRUE' || v === 'true') {
        return '<span class=\"ks-tick\" title=\"match\">\u2713</span>';
      }
      if (v === false || v === 'FALSE' || v === 'false') {
        var note = (cellInfo.row && cellInfo.row['%s']) ? cellInfo.row['%s'] : 'differs';
        var safe = String(note).replace(/\"/g, '&quot;');
        return '<span class=\"ks-cross\" title=\"' + safe + '\">\u2717</span>'
          + ' <span class=\"ks-cross-note\">' + safe + '</span>';
      }
      return '<span class=\"ks-dim\">-</span>';
    }",
    note_col,
    note_col
  ))
}

ks_js_side_pill <- function() {
  reactable::JS(
    "function(cellInfo) {
      var v = cellInfo.value;
      if (v === 'matched') return '<span class=\"ks-pill ks-pill-ok\">matched</span>';
      if (v === 'base_only') return '<span class=\"ks-pill ks-pill-warn\">base only</span>';
      if (v === 'comp_only') return '<span class=\"ks-pill ks-pill-warn\">compare only</span>';
      return v ? v : '';
    }"
  )
}

ks_js_kind_pill <- function() {
  reactable::JS(
    "function(cellInfo) {
      var v = cellInfo.value;
      if (!v) return '';
      var cls = v === 'type_mismatch' ? 'ks-pill ks-pill-warn' : 'ks-pill ks-pill-neutral';
      return '<span class=\"' + cls + '\">' + v + '</span>';
    }"
  )
}

ks_js_pattern_pill <- function() {
  reactable::JS(
    "function(cellInfo) {
      var v = cellInfo.value;
      if (!v) return '';
      return '<span class=\"ks-pill ks-pill-info\">' + v + '</span>';
    }"
  )
}

ks_js_diff_cell <- function() {
  reactable::JS(
    "function(cellInfo) {
      var v = cellInfo.value;
      if (v === null || v === undefined || v === '') {
        return '<span class=\"ks-dim\" title=\"not applicable\">-</span>';
      }
      var num = typeof v === 'number' ? v : Number(v);
      if (!isFinite(num)) {
        return '<span class=\"ks-dim\" title=\"not applicable\">-</span>';
      }
      var kind = cellInfo.row && cellInfo.row['kind'];
      var unit = '';
      var display = num;
      if (kind === 'date') {
        unit = ' d';
      } else if (kind === 'datetime') {
        var abs = Math.abs(num);
        if (abs >= 86400) { display = num / 86400; unit = ' d'; }
        else if (abs >= 3600) { display = num / 3600; unit = ' h'; }
        else if (abs >= 60) { display = num / 60; unit = ' min'; }
        else { unit = ' s'; }
      }
      var cls = num > 0 ? 'ks-diff-pos' : (num < 0 ? 'ks-diff-neg' : 'ks-dim');
      var absDisp = Math.abs(display);
      var formatted;
      if (absDisp === 0) {
        formatted = '0';
      } else if (absDisp >= 0.001) {
        formatted = display.toLocaleString(undefined, { maximumFractionDigits: 4 });
      } else {
        formatted = display.toExponential(3);
      }
      var sign = num > 0 ? '+' : '';
      var title = (kind ? kind + ': ' : '') + num + (kind === 'date' ? ' days' : (kind === 'datetime' ? ' seconds' : ''));
      return '<span class=\"' + cls + '\" title=\"' + title + '\">' + sign + formatted + unit + '</span>';
    }"
  )
}

ks_js_note_cell <- function() {
  reactable::JS(
    "function(cellInfo) {
      var v = cellInfo.value;
      var row = cellInfo.row || {};
      var na = row['na_flow'];
      var chips = [];
      if (na === 'value_to_na') {
        chips.push('<span class=\"ks-note-chip ks-note-chip-na\" title=\"base had a value, compare is NA\">\u2192NA</span>');
      } else if (na === 'na_to_value') {
        chips.push('<span class=\"ks-note-chip ks-note-chip-na\" title=\"base was NA, compare has a value\">\u2190NA</span>');
      } else if (na === 'both_na_differ') {
        chips.push('<span class=\"ks-note-chip ks-note-chip-na\" title=\"both sides NA, but different NA flavours\">\u2260NA</span>');
      }
      if (v !== null && v !== undefined && v !== '') {
        var parts = String(v).split(/;\\s*/);
        parts.forEach(function(p) {
          chips.push('<span class=\"ks-note-chip\">' + p + '</span>');
        });
      }
      if (chips.length === 0) {
        return '<span class=\"ks-dim\">-</span>';
      }
      return chips.join(' ');
    }"
  )
}

# ---- misc helpers -----------------------------------------------------------

ks_html_kv <- function(lst) {
  if (length(lst) == 0L) {
    return(htmltools::tags$p(htmltools::tags$em("(empty)")))
  }
  htmltools::tags$table(
    class = "ks-kv",
    htmltools::tags$tbody(
      lapply(names(lst), function(k) {
        v <- lst[[k]]
        htmltools::tags$tr(
          htmltools::tags$th(k),
          htmltools::tags$td(htmltools::tags$code(format(v)[[1]]))
        )
      })
    )
  )
}

ks_html_empty <- function(msg = "(none)") {
  htmltools::tags$p(class = "ks-empty", htmltools::tags$em(msg))
}

# Render a character vector with leading/trailing whitespace made
# visible via a middle-dot.
ks_mark_whitespace <- function(x) {
  if (is.null(x)) return(x)
  s <- as.character(x)
  out <- s
  has_lead <- !is.na(s) & grepl("^\\s", s)
  has_trail <- !is.na(s) & grepl("\\s$", s)
  out <- ifelse(has_lead & !is.na(out), paste0("\u00b7", out), out)
  out <- ifelse(has_trail & !is.na(out), paste0(out, "\u00b7"), out)
  out
}

# ---- CSS --------------------------------------------------------------------

ks_html_css <- function(theme) {
  # Theme accent (kept for backwards compat with the `theme` arg).
  # The new design is deliberately neutral; the accent is reserved for
  # status-warning hairlines so the two themes look very similar.
  accent <- if (identical(theme, "slate")) "#0f172a" else "#1f2937"
  accent_dark <- if (identical(theme, "slate")) "#020617" else "#0b1220"
  paste(
    # ---- Design tokens --------------------------------------------------
    # Calm neutral palette. One single chromatic accent (vermillion) is
    # reserved for "differences detected" markers and warning rules; an
    # emerald is used only for the "pass" status pill. Everything else is
    # ink-on-paper. Four-step type scale: 11px (meta/mono), 13px (body),
    # 18px (section title), 26px (h1) plus 22px for KPI values.
    ":root {",
    "  --ks-paper: #f7f5f0;",
    "  --ks-paper-soft: #fbfaf6;",
    "  --ks-card: #ffffff;",
    "  --ks-ink: #1a1a1a;",
    "  --ks-ink-soft: #4a4a4a;",
    "  --ks-muted: #767676;",
    "  --ks-rule: #d6d2c7;",
    "  --ks-rule-soft: #e8e4d8;",
    sprintf("  --ks-accent: %s;", accent),
    sprintf("  --ks-accent-dark: %s;", accent_dark),
    "  --ks-vermillion: #b94a2c;",
    "  --ks-vermillion-soft: #fbe9e1;",
    "  --ks-emerald: #2f7d5b;",
    "  --ks-emerald-soft: #e2efe8;",
    "  --ks-amber: #8a5a17;",
    "  --ks-amber-soft: #f3ead4;",
    "  --ks-indigo: #2f3f7d;",
    "  --ks-indigo-soft: #e6e8f1;",
    "  --ks-crimson: #8a2542;",
    "  --ks-crimson-soft: #f3dde2;",
    "  --ks-font-body: 'IBM Plex Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;",
    "  --ks-font-mono: 'IBM Plex Mono', ui-monospace, SFMono-Regular, 'Cascadia Mono', Menlo, Consolas, monospace;",
    "  --ks-font-display: var(--ks-font-body);",
    "  --ks-fs-micro: 0.66rem;",
    "  --ks-fs-meta: 0.7rem;",
    "  --ks-fs-body: 0.8rem;",
    "  --ks-fs-table: 0.74rem;",
    "  --ks-fs-h2: 1.05rem;",
    "  --ks-fs-h1: 1.5rem;",
    "  --ks-fs-kpi: 1.35rem;",
    "}",
    # ---- Reset / base ---------------------------------------------------
    "* { box-sizing: border-box; }",
    "html, body { margin: 0; padding: 0; }",
    paste0(
      "body { ",
      "font-family: var(--ks-font-body); ",
      "color: var(--ks-ink); ",
      "background: var(--ks-paper); ",
      "line-height: 1.5; font-size: 13.5px; ",
      "-webkit-font-smoothing: antialiased; ",
      "-moz-osx-font-smoothing: grayscale; ",
      "font-feature-settings: 'tnum' 1, 'cv11' 1; ",
      "}"
    ),
    "code, kbd, samp { font-family: var(--ks-font-mono); font-size: 0.92em; }",
    "a { color: var(--ks-ink); text-decoration: underline; text-decoration-color: var(--ks-rule); text-underline-offset: 3px; }",
    "a:hover { text-decoration-color: var(--ks-vermillion); }",
    # ---- TOC : minimal side index ---------------------------------------
    ".ks-toc { position: fixed; top: 0; left: 0; bottom: 0; width: 212px; padding: 1.5rem 1.1rem 1rem; background: transparent; border-right: 1px solid var(--ks-rule); overflow-y: auto; }",
    ".ks-toc-brand { font-family: var(--ks-font-body); font-weight: 600; font-size: 0.88rem; line-height: 1; color: var(--ks-ink); padding: 0 0 0.5rem; margin-bottom: 0.85rem; border-bottom: 1px solid var(--ks-ink); letter-spacing: 0; }",
    ".ks-toc-brand::after { content: ''; }",
    ".ks-toc ul { list-style: none; margin: 0; padding: 0; counter-reset: ks-toc; }",
    ".ks-toc li { margin: 0; counter-increment: ks-toc; }",
    ".ks-toc a { display: flex; align-items: center; gap: 0.5rem; padding: 0.3rem 0; color: var(--ks-ink-soft); text-decoration: none; font-size: var(--ks-fs-table); line-height: 1.3; border-bottom: 1px solid var(--ks-rule-soft); }",
    ".ks-toc a::before { content: counter(ks-toc, decimal-leading-zero); font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); color: var(--ks-muted); flex: 0 0 auto; }",
    ".ks-toc a:hover { color: var(--ks-ink); }",
    ".ks-toc a:hover::before { color: var(--ks-ink); }",
    ".ks-toc-label { flex: 1; }",
    ".ks-toc-count { font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); color: var(--ks-muted); font-variant-numeric: tabular-nums; padding: 0 0.3rem; border: 1px solid var(--ks-rule); border-radius: 2px; background: var(--ks-card); }",
    ".ks-toc-count-warn { color: var(--ks-vermillion); border-color: var(--ks-vermillion); }",
    # ---- Main canvas ----------------------------------------------------
    ".ks-main { margin-left: 232px; padding: 1.75rem 2.25rem 3rem; max-width: 1360px; }",
    # ---- Hero header ----------------------------------------------------
    ".ks-header { display: grid; grid-template-columns: 1fr auto; align-items: end; gap: 1.5rem; padding-bottom: 1rem; margin-bottom: 1.5rem; border-bottom: 1px solid var(--ks-ink); }",
    ".ks-header::after { content: none; }",
    ".ks-header h1 { margin: 0; font-family: var(--ks-font-body); font-weight: 600; font-size: var(--ks-fs-h1); line-height: 1.15; letter-spacing: -0.01em; color: var(--ks-ink); }",
    ".ks-subtitle { margin: 0.4rem 0 0; color: var(--ks-ink-soft); font-size: var(--ks-fs-body); font-family: var(--ks-font-body); }",
    # ---- Status pill ----------------------------------------------------
    ".ks-status { display: inline-flex; align-items: center; gap: 0.4rem; padding: 0.3rem 0.6rem; border-radius: 2px; border: 1px solid var(--ks-ink); font-size: var(--ks-fs-micro); font-weight: 600; text-transform: uppercase; letter-spacing: 0.05em; white-space: nowrap; font-family: var(--ks-font-mono); background: var(--ks-card); color: var(--ks-ink); }",
    ".ks-status-dot { width: 0.45rem; height: 0.45rem; border-radius: 50%; background: var(--ks-ink); }",
    ".ks-status-pass { color: var(--ks-emerald); border-color: var(--ks-emerald); }",
    ".ks-status-pass .ks-status-dot { background: var(--ks-emerald); }",
    ".ks-status-diff { color: var(--ks-vermillion); border-color: var(--ks-vermillion); }",
    ".ks-status-diff .ks-status-dot { background: var(--ks-vermillion); }",
    # ---- Datasets : citation-style --------------------------------------
    ".ks-datasets { display: grid; grid-template-columns: 1fr auto 1fr; gap: 1.5rem; align-items: stretch; margin-bottom: 1.5rem; }",
    ".ks-dscard { background: transparent; border: 0; border-top: 1px solid var(--ks-rule); padding: 0.75rem 0 0; position: relative; }",
    ".ks-dscard::before { content: ''; position: absolute; top: -1px; left: 0; width: 32px; height: 2px; background: var(--ks-ink); }",
    ".ks-dscard-comp::before { background: var(--ks-ink); }",
    ".ks-dscard-label { font-family: var(--ks-font-mono); font-size: var(--ks-fs-meta); color: var(--ks-muted); text-transform: uppercase; letter-spacing: 0.1em; }",
    ".ks-dscard-name { font-family: var(--ks-font-body); font-weight: 600; font-size: 1rem; line-height: 1.2; margin-top: 0.25rem; color: var(--ks-ink); word-break: break-word; }",
    ".ks-dscard-dim { color: var(--ks-ink-soft); font-size: var(--ks-fs-meta); margin-top: 0.4rem; font-family: var(--ks-font-mono); font-variant-numeric: tabular-nums; }",
    ".ks-arrow { font-family: var(--ks-font-body); font-size: 1.4rem; color: var(--ks-muted); align-self: center; line-height: 1; }",
    # ---- Verdict --------------------------------------------------------
    ".ks-verdict { background: var(--ks-card); border: 1px solid var(--ks-rule); border-left: 3px solid var(--ks-ink); padding: 0.75rem 1rem; margin-bottom: 1rem; }",
    ".ks-verdict-label { font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); color: var(--ks-muted); text-transform: uppercase; letter-spacing: 0.08em; }",
    ".ks-verdict-text { font-family: var(--ks-font-body); font-weight: 500; font-size: var(--ks-fs-body); line-height: 1.4; margin-top: 0.25rem; color: var(--ks-ink); }",
    ".ks-verdict-bar { height: 2px; background: var(--ks-rule-soft); margin-top: 0.8rem; overflow: hidden; }",
    ".ks-verdict-bar-fill { height: 100%; background: var(--ks-emerald); transition: width 0.3s ease; }",
    ".ks-verdict-ok { border-left-color: var(--ks-emerald); }",
    ".ks-verdict-info { border-left-color: var(--ks-ink-soft); }",
    ".ks-verdict-warn { border-left-color: var(--ks-amber); }",
    ".ks-verdict-critical { border-left-color: var(--ks-vermillion); }",
    ".ks-verdict-critical .ks-verdict-bar-fill { background: var(--ks-vermillion); }",
    ".ks-verdict-warn .ks-verdict-bar-fill { background: var(--ks-amber); }",
    # ---- Recommendations ------------------------------------------------
    ".ks-recs { background: var(--ks-card); border: 1px solid var(--ks-rule); padding: 0.75rem 1rem; margin-bottom: 1.1rem; }",
    ".ks-recs-label { font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); color: var(--ks-muted); text-transform: uppercase; letter-spacing: 0.08em; margin-bottom: 0.5rem; }",
    ".ks-recs-list { list-style: none; margin: 0; padding: 0; }",
    ".ks-rec-item { display: flex; gap: 0.75rem; padding: 0.55rem 0; border-top: 1px solid var(--ks-rule-soft); }",
    ".ks-rec-item:first-child { border-top: 0; padding-top: 0.1rem; }",
    ".ks-rec-badge { flex: 0 0 auto; padding: 0.05rem 0.4rem; font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); font-weight: 600; letter-spacing: 0.04em; text-transform: uppercase; height: fit-content; margin-top: 0.1rem; border: 1px solid var(--ks-rule); color: var(--ks-ink-soft); background: var(--ks-card); border-radius: 2px; }",
    ".ks-rec-critical .ks-rec-badge { color: var(--ks-vermillion); border-color: var(--ks-vermillion); }",
    ".ks-rec-warn .ks-rec-badge { color: var(--ks-amber); border-color: var(--ks-amber); }",
    ".ks-rec-info .ks-rec-badge { color: var(--ks-ink-soft); border-color: var(--ks-rule); }",
    ".ks-rec-ok .ks-rec-badge { color: var(--ks-emerald); border-color: var(--ks-emerald); }",
    ".ks-rec-body { flex: 1 1 auto; min-width: 0; }",
    ".ks-rec-title { font-weight: 600; color: var(--ks-ink); font-size: var(--ks-fs-body); }",
    ".ks-rec-msg { color: var(--ks-ink-soft); font-size: var(--ks-fs-table); margin-top: 0.15rem; line-height: 1.5; }",
    ".ks-rec-action { color: var(--ks-ink); font-size: var(--ks-fs-table); margin-top: 0.25rem; }",
    ".ks-rec-action strong { color: var(--ks-ink); font-weight: 600; }",
    # ---- KPIs -----------------------------------------------------------
    ".ks-kpis { display: grid; grid-template-columns: repeat(auto-fit, minmax(135px, 1fr)); gap: 0; margin-bottom: 1.25rem; border-top: 1px solid var(--ks-ink); border-bottom: 1px solid var(--ks-ink); background: var(--ks-card); }",
    ".ks-kpi { padding: 0.75rem 0.9rem 0.85rem; border-right: 1px solid var(--ks-rule); position: relative; }",
    ".ks-kpi:last-child { border-right: 0; }",
    ".ks-kpi-value { font-family: var(--ks-font-body); font-size: var(--ks-fs-kpi); font-weight: 500; font-variant-numeric: tabular-nums; line-height: 1; color: var(--ks-ink); letter-spacing: -0.01em; }",
    ".ks-kpi-label { color: var(--ks-ink-soft); font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); font-weight: 500; text-transform: uppercase; letter-spacing: 0.06em; margin-top: 0.4rem; }",
    ".ks-kpi-hint { color: var(--ks-muted); font-size: var(--ks-fs-micro); margin-top: 0.15rem; font-family: var(--ks-font-body); }",
    ".ks-kpi-ok::before { content: ''; position: absolute; top: -1px; left: 0; right: 0; height: 2px; background: var(--ks-emerald); }",
    ".ks-kpi-warn::before { content: ''; position: absolute; top: -1px; left: 0; right: 0; height: 2px; background: var(--ks-vermillion); }",
    ".ks-kpi-warn .ks-kpi-value { color: var(--ks-vermillion); }",
    ".ks-kpi-neutral .ks-kpi-value { color: var(--ks-ink-soft); }",
    # ---- Mini cards -----------------------------------------------------
    ".ks-minis { display: grid; grid-template-columns: repeat(auto-fit, minmax(190px, 1fr)); gap: 0.75rem; }",
    ".ks-mini { background: var(--ks-card); border: 1px solid var(--ks-rule); padding: 0.8rem 1rem; }",
    ".ks-mini-value { font-family: var(--ks-font-body); font-weight: 600; font-size: 1.25rem; line-height: 1.1; font-variant-numeric: tabular-nums; color: var(--ks-ink); }",
    ".ks-mini-label { font-family: var(--ks-font-mono); font-size: var(--ks-fs-meta); text-transform: uppercase; letter-spacing: 0.08em; margin-top: 0.4rem; color: var(--ks-ink-soft); }",
    ".ks-mini-hint { color: var(--ks-muted); font-size: var(--ks-fs-meta); margin-top: 0.2rem; font-family: var(--ks-font-body); }",
    ".ks-mini-ok { border-top: 2px solid var(--ks-emerald); }",
    ".ks-mini-warn { border-top: 2px solid var(--ks-vermillion); }",
    # ---- Sections -------------------------------------------------------
    ".ks-section { background: var(--ks-card); border: 1px solid var(--ks-rule); padding: 0.95rem 1.15rem 1.15rem; margin-bottom: 1rem; scroll-margin-top: 1rem; position: relative; }",
    ".ks-section::before { content: ''; position: absolute; top: -1px; left: -1px; width: 24px; height: 2px; background: var(--ks-ink); }",
    ".ks-section-head { display: flex; flex-wrap: wrap; align-items: baseline; column-gap: 0.6rem; row-gap: 0.45rem; margin-bottom: 0.7rem; padding-bottom: 0.55rem; border-bottom: 1px solid var(--ks-rule); }",
    ".ks-section-head h2 { margin: 0; font-family: var(--ks-font-body); font-weight: 600; font-size: var(--ks-fs-h2); line-height: 1.2; color: var(--ks-ink); letter-spacing: -0.005em; flex: 0 0 auto; }",
    ".ks-section-blurb { flex: 1 1 100%; margin: 0; color: var(--ks-ink-soft); font-size: var(--ks-fs-body); line-height: 1.55; max-width: 90ch; }",
    ".ks-section-blurb code { background: var(--ks-paper); padding: 0.05rem 0.35rem; border: 1px solid var(--ks-rule); font-size: 0.78rem; }",
    # ---- Pills ----------------------------------------------------------
    ".ks-pill { display: inline-block; padding: 0.1rem 0.45rem; border-radius: 2px; font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); font-weight: 500; text-transform: uppercase; letter-spacing: 0.04em; line-height: 1.45; border: 1px solid var(--ks-rule); background: var(--ks-card); color: var(--ks-ink-soft); white-space: nowrap; }",
    ".ks-pill-ok { color: var(--ks-emerald); border-color: var(--ks-emerald); background: var(--ks-card); }",
    ".ks-pill-warn { color: var(--ks-vermillion); border-color: var(--ks-vermillion); background: var(--ks-card); }",
    ".ks-pill-info { color: var(--ks-ink); border-color: var(--ks-rule); background: var(--ks-paper); }",
    ".ks-pill-neutral { color: var(--ks-ink-soft); background: var(--ks-paper); border-color: var(--ks-rule); }",
    # ---- Tick / cross / dim --------------------------------------------
    ".ks-tick { color: var(--ks-emerald); font-weight: 600; font-size: 0.85rem; }",
    ".ks-cross { color: var(--ks-vermillion); font-weight: 600; font-size: 0.85rem; }",
    ".ks-cross-note { color: var(--ks-vermillion); font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); margin-left: 0.25rem; }",
    ".ks-dim { color: var(--ks-muted); }",
    # ---- Diff colours ---------------------------------------------------
    ".ks-diff-pos { color: var(--ks-vermillion); font-family: var(--ks-font-mono); font-weight: 500; }",
    ".ks-diff-neg { color: var(--ks-ink); font-family: var(--ks-font-mono); font-weight: 500; }",
    # ---- Note chips (must wrap; never truncate) -------------------------
    ".ks-note-chip { display: inline-block; background: var(--ks-paper); color: var(--ks-ink-soft); border: 1px solid var(--ks-rule); padding: 0.02rem 0.4rem; border-radius: 2px; font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); font-weight: 500; letter-spacing: 0; line-height: 1.5; margin: 0.08rem 0.12rem 0.08rem 0; white-space: normal; word-break: break-word; max-width: 100%; }",
    ".ks-note-chip-na { background: var(--ks-card); color: var(--ks-ink); border-color: var(--ks-ink-soft); font-weight: 600; }",
    # ---- Reactable theming ---------------------------------------------
    ".ks-section .ReactTable, .ks-section .rt-table { font-family: var(--ks-font-body); }",
    ".ks-section .rt-table { overflow: auto; max-height: 72vh; border: 1px solid var(--ks-rule); }",
    ".ks-section .rt-thead { position: sticky; top: 0; z-index: 3; background: var(--ks-paper); border-bottom: 1px solid var(--ks-ink); }",
    ".ks-section .rt-th { font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); font-weight: 600; text-transform: uppercase; letter-spacing: 0.03em; color: var(--ks-ink); padding: 0.3rem 0.5rem; border-right: 1px solid var(--ks-rule-soft); }",
    ".ks-section .rt-th-group { background: var(--ks-paper-soft); }",
    ".ks-section .rt-td { font-size: var(--ks-fs-table); padding: 0.22rem 0.5rem; border-right: 1px solid var(--ks-rule-soft); color: var(--ks-ink); line-height: 1.35; white-space: normal; word-break: break-word; }",
    ".ks-section .rt-td-inner { white-space: normal; overflow: visible; text-overflow: clip; }",
    ".ks-section .rt-tr:nth-child(even) .rt-td { background: var(--ks-paper-soft); }",
    ".ks-section .rt-tr:hover .rt-td { background: #efece2 !important; }",
    ".ks-section .rt-td-numeric { font-family: var(--ks-font-mono); font-variant-numeric: tabular-nums; }",
    ".ks-section .rt-search { font-family: var(--ks-font-mono); font-size: var(--ks-fs-meta); padding: 0.25rem 0.5rem; border: 1px solid var(--ks-rule); border-radius: 2px; background: var(--ks-card); color: var(--ks-ink); }",
    ".ks-section .rt-search:focus { outline: 1px solid var(--ks-ink); outline-offset: -1px; border-color: var(--ks-ink); }",
    ".ks-section .rt-pagination { font-family: var(--ks-font-mono); font-size: var(--ks-fs-meta); color: var(--ks-ink-soft); padding: 0.4rem 0.5rem; }",
    ".ks-section .rt-page-button { font-family: var(--ks-font-mono); font-size: var(--ks-fs-meta); }",
    ".ks-section .rt-page-button-content { padding: 2px 6px; }",
    ".ks-section .rt-page-info { font-size: var(--ks-fs-meta); }",
    ".ks-section .rt-page-size { font-size: var(--ks-fs-meta); }",
    # ---- Per-column header filter inputs -------------------------------
    ".ks-col-filter { width: 100%; box-sizing: border-box; font-family: var(--ks-font-mono); font-size: 10.5px; padding: 1px 3px; border: 1px solid var(--ks-rule); border-radius: 2px; background: var(--ks-card); color: var(--ks-ink-soft); font-variant-numeric: tabular-nums; font-weight: 400; }",
    ".ks-col-filter:focus { outline: 1px solid var(--ks-ink); outline-offset: -1px; border-color: var(--ks-ink); }",
    "select.ks-col-filter { appearance: auto; padding-right: 16px; cursor: pointer; }",
    # ---- Sample notice -------------------------------------------------
    ".ks-sample-notice { background: var(--ks-paper); color: var(--ks-ink-soft); border: 1px solid var(--ks-rule); border-left: 3px solid var(--ks-ink-soft); padding: 0.6rem 0.85rem; font-size: var(--ks-fs-body); margin: 0 0 0.75rem; font-family: var(--ks-font-body); }",
    ".ks-sample-notice code { background: var(--ks-card); padding: 0.05rem 0.35rem; border: 1px solid var(--ks-rule); font-size: 0.78rem; }",
    # ---- Column selector -----------------------------------------------
    ".ks-col-selector { display: flex; align-items: center; gap: 0.55rem; margin: 0 0 0.7rem; flex-wrap: wrap; }",
    ".ks-col-selector label { font-family: var(--ks-font-mono); font-size: var(--ks-fs-meta); color: var(--ks-ink); font-weight: 600; text-transform: uppercase; letter-spacing: 0.06em; }",
    ".ks-col-selector select { font-family: var(--ks-font-mono); padding: 0.32rem 0.5rem; font-size: var(--ks-fs-body); border: 1px solid var(--ks-ink); border-radius: 2px; background: var(--ks-card); min-width: 260px; max-width: 100%; font-variant-numeric: tabular-nums; }",
    ".ks-col-selector select:focus { outline: 1px solid var(--ks-ink); outline-offset: 1px; }",
    # ---- Row-matching section ------------------------------------------
    ".ks-banner { padding: 0.85rem 1.05rem; margin: 0 0 0.9rem; border: 1px solid var(--ks-rule); border-left-width: 3px; border-left-style: solid; background: var(--ks-card); }",
    ".ks-banner-title { font-family: var(--ks-font-body); font-weight: 600; font-size: 1rem; margin-bottom: 0.25rem; color: var(--ks-ink); }",
    ".ks-banner-body { font-size: var(--ks-fs-body); line-height: 1.55; color: var(--ks-ink-soft); }",
    ".ks-banner-body code { background: var(--ks-paper); padding: 0.05rem 0.35rem; border: 1px solid var(--ks-rule); font-size: 0.78rem; }",
    ".ks-banner-ok { border-left-color: var(--ks-emerald); }",
    ".ks-banner-warn { border-left-color: var(--ks-amber); }",
    ".ks-rm-stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 0; border: 1px solid var(--ks-rule); margin: 0 0 0.9rem; background: var(--ks-card); }",
    ".ks-rm-stat { padding: 0.6rem 0.85rem; border-right: 1px solid var(--ks-rule); }",
    ".ks-rm-stat:last-child { border-right: 0; }",
    ".ks-rm-stat-value { font-family: var(--ks-font-body); font-weight: 500; font-size: 1.15rem; line-height: 1; font-variant-numeric: tabular-nums; color: var(--ks-ink); }",
    ".ks-rm-stat-label { font-family: var(--ks-font-mono); font-size: var(--ks-fs-meta); text-transform: uppercase; letter-spacing: 0.08em; color: var(--ks-muted); margin-top: 0.35rem; }",
    ".ks-rm-stat-ok .ks-rm-stat-value { color: var(--ks-emerald); }",
    ".ks-rm-stat-warn .ks-rm-stat-value { color: var(--ks-vermillion); }",
    ".ks-keycard { background: var(--ks-paper); border: 1px solid var(--ks-rule); padding: 0.7rem 0.95rem; margin: 0 0 0.9rem; }",
    ".ks-keycard-label { font-family: var(--ks-font-mono); font-size: var(--ks-fs-meta); color: var(--ks-muted); font-weight: 600; text-transform: uppercase; letter-spacing: 0.08em; }",
    ".ks-keycard-list { margin: 0.35rem 0 0; padding-left: 1.1rem; font-size: var(--ks-fs-body); }",
    ".ks-keycard-list code { background: var(--ks-card); padding: 0.08rem 0.35rem; border: 1px solid var(--ks-rule); font-size: 0.8rem; }",
    ".ks-keycard-empty { font-size: var(--ks-fs-body); color: var(--ks-muted); margin-top: 0.3rem; font-family: var(--ks-font-body); }",
    # ---- Grouped value-diff blocks -------------------------------------
    ".ks-group { border: 1px solid var(--ks-rule); margin: 0 0 0.4rem; background: var(--ks-card); overflow: hidden; }",
    ".ks-group[open] { border-color: var(--ks-ink); }",
    ".ks-group-summary { display: flex; align-items: center; gap: 0.55rem; padding: 0.45rem 0.75rem; cursor: pointer; user-select: none; font-size: var(--ks-fs-body); flex-wrap: wrap; }",
    ".ks-group-summary:hover { background: var(--ks-paper); }",
    ".ks-group-summary::-webkit-details-marker { display: none; }",
    ".ks-group-summary::before { content: '+'; display: inline-block; transition: transform 0.15s; color: var(--ks-ink); font-family: var(--ks-font-mono); font-weight: 600; font-size: 0.95rem; width: 0.9rem; text-align: center; }",
    ".ks-group[open] > .ks-group-summary::before { content: '\u2212'; }",
    ".ks-group-label { font-weight: 500; font-family: var(--ks-font-mono); font-size: var(--ks-fs-table); color: var(--ks-ink); }",
    ".ks-group-chip { display: inline-block; padding: 0.08rem 0.45rem; font-family: var(--ks-font-mono); font-size: var(--ks-fs-micro); font-weight: 500; text-transform: uppercase; letter-spacing: 0.04em; background: var(--ks-card); color: var(--ks-vermillion); border: 1px solid var(--ks-vermillion); border-radius: 2px; white-space: nowrap; }",
    ".ks-group-chip-neutral { background: var(--ks-paper); color: var(--ks-ink-soft); border-color: var(--ks-rule); }",
    ".ks-group-chip-warn { background: var(--ks-card); color: var(--ks-amber); border-color: var(--ks-amber); }",
    ".ks-group-body { padding: 0.6rem 0.85rem 0.85rem; border-top: 1px solid var(--ks-rule); }",
    # ---- Empty / warning -----------------------------------------------
    ".ks-empty em { color: var(--ks-muted); font-family: var(--ks-font-body); font-style: normal; font-size: var(--ks-fs-body); }",
    ".ks-warning { color: var(--ks-amber); font-family: var(--ks-font-body); font-size: var(--ks-fs-body); margin: 0 0 0.5rem; }",
    # ---- KV table ------------------------------------------------------
    ".ks-kv { width: 100%; border-collapse: collapse; font-size: var(--ks-fs-body); }",
    ".ks-kv th { text-align: left; padding: 0.4rem 1rem 0.4rem 0; color: var(--ks-muted); font-family: var(--ks-font-mono); font-size: var(--ks-fs-meta); font-weight: 500; text-transform: uppercase; letter-spacing: 0.08em; vertical-align: top; width: 30%; border-bottom: 1px solid var(--ks-rule-soft); }",
    ".ks-kv td { padding: 0.4rem 0; word-break: break-all; border-bottom: 1px solid var(--ks-rule-soft); }",
    ".ks-kv code { background: var(--ks-paper); padding: 0.1rem 0.4rem; border: 1px solid var(--ks-rule); font-size: 0.8rem; }",
    # ---- Legend --------------------------------------------------------
    ".ks-legend { margin-top: 1.1rem; padding: 0.8rem 1.05rem; background: var(--ks-paper); border: 1px solid var(--ks-rule); font-size: var(--ks-fs-body); }",
    ".ks-legend-title { font-family: var(--ks-font-mono); font-weight: 600; color: var(--ks-ink); text-transform: uppercase; letter-spacing: 0.08em; font-size: var(--ks-fs-meta); margin-bottom: 0.5rem; }",
    ".ks-legend-list { list-style: none; margin: 0; padding: 0; }",
    ".ks-legend-list li { display: flex; align-items: flex-start; gap: 0.65rem; margin: 0.4rem 0; }",
    ".ks-legend-list li .ks-pill { flex: 0 0 auto; margin-top: 0.05rem; }",
    ".ks-legend-text { color: var(--ks-ink-soft); line-height: 1.55; }",
    ".ks-legend-text code { background: var(--ks-card); padding: 0.05rem 0.35rem; border: 1px solid var(--ks-rule); font-size: 0.78rem; }",
    # ---- Footer --------------------------------------------------------
    ".ks-footer { color: var(--ks-muted); font-family: var(--ks-font-body); font-size: var(--ks-fs-body); text-align: right; margin-top: 1.75rem; padding-top: 0.85rem; border-top: 1px solid var(--ks-rule); }",
    # ---- Responsive ----------------------------------------------------
    "@media (max-width: 900px) {",
    "  .ks-toc { position: static; width: auto; border-right: 0; border-bottom: 1px solid var(--ks-rule); padding: 1rem; }",
    "  .ks-main { margin-left: 0; padding: 1.25rem; }",
    "  .ks-datasets { grid-template-columns: 1fr; }",
    "  .ks-arrow { display: none; }",
    "  .ks-kpis { grid-template-columns: repeat(2, 1fr); }",
    "  .ks-kpi { border-bottom: 1px solid var(--ks-rule); }",
    "}",
    "@media print {",
    "  body { background: #fff; }",
    "  .ks-toc { display: none; }",
    "  .ks-main { margin-left: 0; max-width: 100%; padding: 0; }",
    "  .ks-section { break-inside: avoid; }",
    "}",
    sep = "\n"
  )
}
