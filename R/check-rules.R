#' Apply a list of validation rules to a data frame
#'
#' Evaluates each rule formula against \code{data} and records a message for
#' every row that violates a rule. Two modes are supported:
#' \describe{
#'   \item{\code{"first"}}{Each row gets the message of the \emph{first} rule
#'     it fails; subsequent rules are not evaluated for that row.}
#'   \item{\code{"all"}}{All rules are evaluated for every row; the row
#'     receives a list of all messages for rules it fails.}
#' }
#'
#' @param data A data frame.
#' @param rules A named list of rule objects, each with two fields:
#'   \describe{
#'     \item{\code{expr}}{A one-sided formula whose RHS is a logical
#'       expression (evaluated with \code{rlang::eval_tidy()}) that is
#'       \code{TRUE} for \emph{failing} rows.}
#'     \item{\code{msg}}{Character scalar. Message written to failing rows.}
#'   }
#' @param mode Character. Either \code{"first"} (default) or \code{"all"}.
#'
#' @return \code{data} with an additional list-column \code{check_msgs}.
#'   In \code{"first"} mode each element is a single string (\code{"OK"} for
#'   passing rows). In \code{"all"} mode each element is a character vector
#'   of all violated messages (or \code{"OK"}).
#'
#' @examples
#' \dontrun{
#' rules <- list(
#'   list(expr = ~ AGE < 0, msg = "Negative age"),
#'   list(expr = ~ is.na(SEX), msg = "Missing sex")
#' )
#' ks_check_rules(adsl, rules, mode = "all")
#' }
#'
#' @name ks_check_rules
#' @importFrom rlang eval_tidy f_rhs
#' @export
ks_check_rules <- function(data, rules, mode = c("first", "all")) {
  mode <- match.arg(mode)
  n <- nrow(data)

  if (length(rules) == 0L) {
    data$check_msgs <- rep(list("OK"), n)
    return(data)
  }

  rule_exprs <- lapply(rules, function(rule) rlang::f_rhs(rule$expr))
  rule_msgs <- vapply(
    rules,
    function(rule) as.character(rule$msg)[1],
    character(1),
    USE.NAMES = FALSE
  )

  if (mode == "first") {
    result_msgs <- rep("OK", n)
    pending_idx <- seq_len(n)

    for (i in seq_along(rule_exprs)) {
      if (!length(pending_idx)) break

      hit <- rlang::eval_tidy(
        rule_exprs[[i]],
        data = data[pending_idx, , drop = FALSE]
      )
      hit <- !is.na(hit) & hit
      if (!any(hit)) next

      failed_idx <- pending_idx[hit]
      result_msgs[failed_idx] <- rule_msgs[[i]]
      pending_idx <- pending_idx[!hit]
    }

    data$check_msgs <- as.list(result_msgs)
    return(data)
  }

  result_msgs <- vector("list", n)

  for (i in seq_along(rule_exprs)) {
    hit <- rlang::eval_tidy(rule_exprs[[i]], data = data)
    hit <- !is.na(hit) & hit
    failed <- which(hit)
    if (!length(failed)) next

    msg <- rule_msgs[[i]]
    for (j in failed) {
      result_msgs[[j]] <- c(result_msgs[[j]], msg)
    }
  }

  for (j in seq_len(n)) {
    if (length(result_msgs[[j]]) == 0L) result_msgs[[j]] <- "OK"
  }

  data$check_msgs <- result_msgs
  data
}

#' Collapse list-column of check messages to a character column
#'
#' Converts the list-column produced by \code{\link{ks_check_rules}} into a
#' plain character column by pasting multiple messages together with a
#' separator.
#'
#' @param data A data frame containing a list-column of check messages.
#' @param sep Character scalar. Separator used when collapsing multiple
#'   messages. Default \code{"; "}.
#' @param col \code{<tidy-select>} Bare name of the list-column to collapse.
#'   Must be provided explicitly.
#'
#' @return \code{data} with \code{col} replaced by a character column.
#'
#' @examples
#' \dontrun{
#' df_checked <- ks_check_rules(data, rules, mode = "all")
#' ks_collapse_check_msgs(df_checked, col = check_msgs)
#' }
#'
#' @name ks_collapse_check_msgs
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr
#' @export
ks_collapse_check_msgs <- function(data, sep = "; ", col) {
  if (missing(col)) {
    cli::cli_abort("{.arg col} must be provided.")
  }
  dplyr::mutate(data, {{ col }} := purrr::map_chr({{ col }}, ~ paste(.x, collapse = sep)))
}

#' Compare validation check state between two runs
#'
#' Compares \code{current} results against a \code{previous} snapshot and
#' adds a \code{status} column indicating whether each row is new, changed,
#' or unchanged relative to the previous run.
#'
#' @param current A data frame from the most recent \code{\link{ks_check_rules}}
#'   run.
#' @param previous A data frame from a previous run, or \code{NULL} (default).
#'   If \code{NULL} or empty, all rows in \code{current} receive
#'   \code{status = "new"}.
#' @param key_cols Character vector of column names used to match rows across
#'   runs. Must be provided explicitly.
#' @param compare_cols Character vector of column names whose values are used
#'   to detect changes. Must be provided explicitly.
#'
#' @return \code{current} with an additional character column \code{status}:
#'   \describe{
#'     \item{\code{"new"}}{Row key not found in \code{previous}.}
#'     \item{\code{"changed"}}{Row key found but comparison signature
#'       differs.}
#'     \item{\code{""}}{Row is unchanged.}
#'   }
#'
#' @examples
#' \dontrun{
#' result_new <- ks_check_rules(data_new, rules)
#' result_old <- readRDS("previous_check.rds")
#' ks_compare_check_state(result_new, result_old)
#' }
#'
#' @name ks_compare_check_state
#' @importFrom cli cli_abort
#' @export
ks_compare_check_state <- function(current, previous = NULL,
                                   key_cols, compare_cols) {
  if (missing(key_cols)) {
    cli::cli_abort("{.arg key_cols} must be provided.")
  }
  if (missing(compare_cols)) {
    cli::cli_abort("{.arg compare_cols} must be provided.")
  }

  if (is.null(previous) || nrow(previous) == 0) {
    current$status <- "new"
    return(current)
  }

  cur_key <- do.call(paste, c(current[key_cols], sep = "|"))
  prev_key <- do.call(paste, c(previous[key_cols], sep = "|"))

  cur_sig <- do.call(
    paste,
    c(current[intersect(compare_cols, names(current))], sep = "||")
  )
  prev_sig <- do.call(
    paste,
    c(previous[intersect(compare_cols, names(previous))], sep = "||")
  )
  names(prev_sig) <- prev_key

  status <- character(nrow(current))
  for (i in seq_along(cur_key)) {
    k <- cur_key[i]
    if (!(k %in% prev_key)) {
      status[i] <- "new"
    } else if (cur_sig[i] != prev_sig[k]) {
      status[i] <- "changed"
    } else {
      status[i] <- ""
    }
  }

  current$status <- status
  current
}