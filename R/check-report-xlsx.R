#' Save a data frame to an XLSX workbook split by a grouping column
#'
#' Writes \code{data} to an XLSX file, creating one worksheet per unique value
#' of \code{split_col}. Rows can be colour-highlighted based on the content of
#' a message column (e.g. the output of \code{\link{ks_check_rules}} collapsed
#' with \code{\link{ks_collapse_check_msgs}}) and optionally a status column.
#' Column labels (stored as \code{"label"} attributes) are written as a grey
#' italic row above the data headers when present.
#'
#' @param data A data frame.
#' @param file Character. Path to the output \code{.xlsx} file.
#' @param colors Named character vector mapping message substrings to hex
#'   colour codes (e.g. \code{c("Error" = "#FFCCCC", "OK" = "#CCFFCC")}).
#'   Values with or without leading \code{"#"} are accepted.
#' @param split_col \code{<tidy-select>} Bare name of the column used to split
#'   data into worksheets. Default \code{type}.
#' @param msg_col \code{<tidy-select>} Bare name of the column that contains
#'   check messages used for row colouring. Default \code{check_msgs}.
#' @param na_to_empty Logical. If \code{TRUE}, replace \code{NA} and
#'   \code{"#N/A"} in character columns with empty strings. Default
#'   \code{FALSE}.
#' @param status_col \code{<tidy-select>} Optional bare name of a status
#'   column (e.g. from \code{\link{ks_compare_check_state}}) used for a
#'   secondary layer of row colouring that overrides \code{colors}.
#'   Pass \code{NULL} to disable. Default \code{NULL}.
#' @param status_colors Named character vector mapping status values to hex
#'   colour codes. Used only when \code{status_col} is not \code{NULL}.
#'   Values with or without leading \code{"#"} are accepted.
#'
#' @return Invisibly returns \code{NULL}. The file is written as a side
#'   effect.
#'
#' @examples
#' \dontrun{
#' ks_save2xlsx_by(
#'   results,
#'   file = "output/checks.xlsx",
#'   colors = c("Error" = "#FFCCCC", "Warning" = "#FFFF99"),
#'   split_col = type,
#'   msg_col = check_msgs
#' )
#' }
#'
#' @name ks_save2xlsx_by
#' @importFrom dplyr filter select mutate across where
#' @importFrom purrr map_chr
#' @export
ks_save2xlsx_by <- function(data, file, colors,
                            split_col = type,
                            msg_col = check_msgs,
                            na_to_empty = FALSE,
                            status_col = NULL,
                            status_colors = NULL) {
  ks_check_installed("openxlsx2", reason = "for ks_save2xlsx_by()")

  normalize_hex_color <- function(x) {
    sub("^#", "", x)
  }

  colors <- vapply(colors, normalize_hex_color, character(1), USE.NAMES = TRUE)
  if (!is.null(status_colors)) {
    status_colors <- vapply(status_colors, normalize_hex_color, character(1), USE.NAMES = TRUE)
  }

  split_col_name <- deparse(substitute(split_col))
  msg_col_name <- deparse(substitute(msg_col))
  status_col_name <- if (!is.null(substitute(status_col))) deparse(substitute(status_col)) else NULL

  wb <- openxlsx2::wb_workbook()

  for (i in seq_along(colors)) {
    wb <- openxlsx2::wb_add_dxfs_style(
      wb,
      name = paste0(".ks_mfill_", i),
      bg_fill = openxlsx2::wb_color(colors[[i]])
    )
  }
  if (!is.null(status_colors)) {
    for (j in seq_along(status_colors)) {
      wb <- openxlsx2::wb_add_dxfs_style(
        wb,
        name = paste0(".ks_sfill_", j),
        bg_fill = openxlsx2::wb_color(status_colors[[j]])
      )
    }
  }

  for (tp in unique(data[[split_col_name]])) {
    sheet_data <- data %>%
      dplyr::filter({{ split_col }} == tp) %>%
      dplyr::select(-{{ split_col }})

    nc <- ncol(sheet_data)

    labels <- purrr::map_chr(sheet_data, ~ {
      lbl <- attr(.x, "label")
      if (is.null(lbl)) "" else lbl
    })
    has_labels <- any(labels != "")

    if (na_to_empty) {
      sheet_data <- sheet_data %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::where(is.character),
            ~ replace(.x, .x == "#N/A" | is.na(.x), "")
          )
        )
    }

    row_offset <- if (has_labels) 1L else 0L

    wb <- wb %>%
      openxlsx2::wb_add_worksheet(sheet = tp)

    wb <- wb %>%
      openxlsx2::wb_add_data(
        x = sheet_data,
        start_row = 1L + row_offset,
        na.strings = if (na_to_empty) "" else NULL
      ) %>%
      openxlsx2::wb_add_filter(rows = 1L + row_offset, cols = 1:nc)

    wb <- wb %>%
      openxlsx2::wb_set_col_widths(cols = 1:nc, widths = "auto") %>%
      openxlsx2::wb_freeze_pane(first_active_row = 2L + row_offset, first_active_col = 1L)

    if (has_labels) {
      wb <- wb %>%
        openxlsx2::wb_add_data(
          x = as.data.frame(t(labels), stringsAsFactors = FALSE),
          col_names = FALSE,
          start_row = 1
        ) %>%
        openxlsx2::wb_add_font(
          dims = openxlsx2::wb_dims(rows = 1, cols = 1:nc),
          italic = TRUE,
          color = openxlsx2::wb_color("808080")
        ) %>%
        openxlsx2::wb_add_cell_style(
          dims = openxlsx2::wb_dims(rows = 1, cols = 1:nc),
          wrap_text = TRUE
        )
    }

    nr <- nrow(sheet_data)
    first_data_row <- 2L + row_offset

    if (!is.null(status_col_name) && !is.null(status_colors) &&
        status_col_name %in% names(sheet_data) && nr > 0) {
      status_col_idx <- which(names(sheet_data) == status_col_name)
      status_col_letter <- openxlsx2::int2col(status_col_idx)
      status_cf_dims <- openxlsx2::wb_dims(
        rows = first_data_row:(nr + 1L + row_offset),
        cols = status_col_idx
      )
      for (j in seq_along(status_colors)) {
        wb <- openxlsx2::wb_add_conditional_formatting(
          wb,
          sheet = tp,
          dims = status_cf_dims,
          type = "expression",
          rule = paste0(
            "$", status_col_letter, first_data_row,
            "=\"", names(status_colors)[j], "\""
          ),
          style = paste0(".ks_sfill_", j)
        )
      }
    }

    if (nr > 0 && msg_col_name %in% names(sheet_data)) {
      msg_col_idx <- which(names(sheet_data) == msg_col_name)
      msg_col_letter <- openxlsx2::int2col(msg_col_idx)
      msg_cf_dims <- openxlsx2::wb_dims(
        rows = first_data_row:(nr + 1L + row_offset),
        cols = 1:nc
      )
      for (i in seq_along(colors)) {
        wb <- openxlsx2::wb_add_conditional_formatting(
          wb,
          sheet = tp,
          dims = msg_cf_dims,
          type = "expression",
          rule = paste0(
            "ISNUMBER(FIND(\"", names(colors)[i],
            "\",$", msg_col_letter, first_data_row, "))"
          ),
          style = paste0(".ks_mfill_", i)
        )
      }
    }

    if (nr > 0) {
      data_dims <- openxlsx2::wb_dims(
        rows = (1L + row_offset):(nr + 1L + row_offset),
        cols = 1:nc
      )
      border_color <- openxlsx2::wb_color("000000")
      wb <- wb %>%
        openxlsx2::wb_add_border(
          dims = data_dims,
          bottom_border = "thin", top_border = "thin",
          left_border = "thin", right_border = "thin",
          inner_hgrid = "thin", inner_vgrid = "thin",
          inner_hcolor = border_color, inner_vcolor = border_color,
          bottom_color = border_color, top_color = border_color,
          left_color = border_color, right_color = border_color
        )
    }
  }

  wb %>% openxlsx2::wb_save(file)
  invisible(NULL)
}