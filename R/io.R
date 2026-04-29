#' Internal: load an input for `ks_compare()`
#'
#' Accepts an in-memory data-frame-like object (returned untouched after
#' [tibble::as_tibble()] coercion) or a single character path. When given
#' a path, dispatches to an internal reader keyed off the file extension:
#'
#' - `.sas7bdat` -> [haven::read_sas()]
#' - `.xpt`      -> [haven::read_xpt()]
#' - `.rds`      -> [readRDS()]
#' - `.rdata` / `.rda` -> [load()] (must contain exactly one data frame)
#' - `.parquet`  -> [arrow::read_parquet()]
#' - `.feather` / `.arrow` -> [arrow::read_feather()]
#' - `.csv`      -> [utils::read.csv()]
#' - `.tsv`      -> [utils::read.delim()]
#'
#' The loaded tibble carries a `source` attribute (absolute path) so the
#' rest of the pipeline (display names, manifest, reports) can use it.
#'
#' @keywords internal
#' @noRd
ks_load_input <- function(x, arg_name = "input", call = rlang::caller_env()) {
  if (is.data.frame(x) || tibble::is_tibble(x) || inherits(x, "ArrowTabular")) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    if (!file.exists(x)) {
      ks_abort(
        c(
          sprintf(
            "{.arg %s} looks like a file path but the file does not exist.",
            arg_name
          ),
          x = sprintf("{.file %s}", x)
        ),
        call = call
      )
    }
    return(ks_read_by_ext(x, arg_name = arg_name, call = call))
  }
  if (is.list(x)) {
    # Allow lists/data.frame-likes that pass tibble::as_tibble() later.
    return(x)
  }
  ks_abort(
    c(
      sprintf(
        "{.arg %s} must be a data frame or a single file path.",
        arg_name
      ),
      i = sprintf("Got {.cls %s}.", class(x)[[1]])
    ),
    call = call
  )
}

#' Internal: ext-keyed dispatcher
#' @keywords internal
#' @noRd
ks_read_by_ext <- function(path, arg_name = "input", call = rlang::caller_env()) {
  ext <- tolower(tools::file_ext(path))
  out <- switch(
    ext,
    sas7bdat = ks_read_sas_internal(path),
    xpt      = ks_read_xpt_internal(path),
    rds      = ks_read_rds_internal(path, arg_name = arg_name, call = call),
    rdata    = ,
    rda      = ks_read_rdata_internal(path, arg_name = arg_name, call = call),
    parquet  = ks_read_arrow_internal(path, format = "parquet"),
    feather  = ,
    arrow    = ks_read_arrow_internal(path, format = "feather"),
    csv      = ks_read_delim_internal(path, sep = ","),
    tsv      = ,
    txt      = ks_read_delim_internal(path, sep = "\t"),
    ks_abort(
      c(
        sprintf(
          "Cannot guess reader for {.arg %s} from extension {.val %s}.",
          arg_name, ext
        ),
        i = paste0(
          "Supported: .sas7bdat, .xpt, .rds, .rdata/.rda, .parquet, ",
          ".feather/.arrow, .csv, .tsv."
        )
      ),
      call = call
    )
  )
  attr(out, "source") <- normalizePath(path, mustWork = FALSE)
  out
}

ks_read_sas_internal <- function(path) {
  ks_check_installed("haven", reason = "to read .sas7bdat files")
  haven::read_sas(path)
}

ks_read_xpt_internal <- function(path) {
  ks_check_installed("haven", reason = "to read .xpt files")
  haven::read_xpt(path)
}

ks_read_rds_internal <- function(path, arg_name, call) {
  obj <- readRDS(path)
  if (!is.data.frame(obj) && !tibble::is_tibble(obj)) {
    ks_abort(
      c(
        sprintf(
          "{.arg %s} ({.file %s}) is an RDS file but does not contain a data frame.",
          arg_name, path
        ),
        i = sprintf("Got {.cls %s}.", class(obj)[[1]])
      ),
      call = call
    )
  }
  obj
}

ks_read_rdata_internal <- function(path, arg_name, call) {
  env <- new.env(parent = emptyenv())
  nms <- load(path, envir = env)
  dfs <- nms[vapply(nms, function(n) is.data.frame(env[[n]]), logical(1L))]
  if (length(dfs) == 0L) {
    ks_abort(
      c(
        sprintf(
          "{.arg %s} ({.file %s}) does not contain any data frame.",
          arg_name, path
        ),
        i = sprintf("Found objects: {.val %s}.", paste(nms, collapse = ", "))
      ),
      call = call
    )
  }
  if (length(dfs) > 1L) {
    ks_abort(
      c(
        sprintf(
          "{.arg %s} ({.file %s}) contains multiple data frames; cannot pick one.",
          arg_name, path
        ),
        i = sprintf("Found: {.val %s}.", paste(dfs, collapse = ", ")),
        i = "Load the file yourself and pass the chosen data frame to {.fn ks_compare}."
      ),
      call = call
    )
  }
  env[[dfs]]
}

ks_read_arrow_internal <- function(path, format) {
  ks_check_installed("arrow", reason = "to read Parquet / Feather files")
  switch(
    format,
    parquet = arrow::read_parquet(path),
    feather = arrow::read_feather(path),
    ks_abort("Unknown Arrow format {.val {format}}.")
  )
}

ks_read_delim_internal <- function(path, sep) {
  utils::read.delim(
    path,
    sep = sep,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8"
  )
}

#' Internal: derive display names for two inputs
#'
#' Used by [ks_compare()] when one or both inputs are file paths to
#' disambiguate two files that share the same basename but live in
#' different folders (e.g. `prod/adsl.sas7bdat` vs `qc/adsl.sas7bdat`).
#'
#' Strategy:
#' - If neither side is a path, return the captured expression labels.
#' - Otherwise compute basename-without-extension for each side.
#' - If those collide, prepend the parent directory name on whichever
#'   side(s) came from a file path.
#'
#' @keywords internal
#' @noRd
ks_display_names <- function(base, comp, base_label, comp_label) {
  base_path <- ks_input_path(base)
  comp_path <- ks_input_path(comp)
  bn <- if (!is.null(base_path)) ks_path_stem(base_path) else base_label
  cn <- if (!is.null(comp_path)) ks_path_stem(comp_path) else comp_label
  if (identical(bn, cn) && (!is.null(base_path) || !is.null(comp_path))) {
    if (!is.null(base_path)) {
      bn <- ks_path_stem_with_parent(base_path)
    }
    if (!is.null(comp_path)) {
      cn <- ks_path_stem_with_parent(comp_path)
    }
  }
  list(base = bn, comp = cn)
}

ks_input_path <- function(x) {
  src <- attr(x, "source", exact = TRUE)
  if (is.character(src) && length(src) == 1L && !is.na(src) && nzchar(src)) {
    return(src)
  }
  NULL
}

ks_path_stem <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

ks_path_stem_with_parent <- function(path) {
  parent <- basename(dirname(path))
  stem <- ks_path_stem(path)
  if (!nzchar(parent) || parent %in% c(".", "/", "\\")) {
    return(stem)
  }
  paste0(parent, "/", stem)
}
