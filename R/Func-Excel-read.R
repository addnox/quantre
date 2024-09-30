#' DT version of read_excel
#'
#' @export
readxl <- function(wb, ws = NULL, range = NULL, rows = NULL, cols = NULL, col_names = TRUE, col_types = NULL, ...) {
  res <- readxl_raw(wb, ws, range, rows, cols, col_names, col_types, ...)
  res
}

#' Read entire Excel sheet from cell `A1`
#'
#' @export
#' @rdname readxl
readxl_raw <- function(wb, ws = NULL, range = NULL, rows = NULL, cols = NULL, col_names = FALSE, col_types = "text", alpha_type_cols = FALSE, ...) {
  # if (is.null(range) & is.null(rows) & is.null(cols)) range <- readxl::cell_limits(c(1L, 1L))
  if (is.null(ws)) ws <- 1L

  rng <- xlrange(range, rows, cols)

  if (!is.na(rng$sheet)) ws <- NULL ## range ws spec will overwrite ws

  res <- readxl::read_excel(wb,
                            ws,
                            range = xlrange(range, rows, cols),
                            col_names = col_names,
                            col_types = col_types,
                            trim_ws = FALSE,
                            .name_repair = function(x) makenm_numbered(x, blank_only = TRUE),
                            ...)

  data.table::setDT(res)

  if (alpha_type_cols) {# v1 to A, v3 to C and etc.
    colnum <- seq_len(ncol(res))
    newcolnm <- stringi::stri_sub(openxlsx2::get_cell_refs(data.frame(1, colnum)), 1L, -2L)
    data.table::setnames(res, newcolnm)
  }

  res[]
}

#' Read named ranges in workbook
#' @export
#' @rdname readxl
readxl_named_range <- function(wb, ws = NULL) {
  x <- openxlsx2::wb_get_named_regions(wb)

  raw <- data.table::data.table(
    Sheet = attr(x, "sheet"),
    Name = x,
    Range = attr(x, "position")
  )

  res <- raw[!Sheet %in% c("", "#REF") & Range != ""]

  if (!is.null(ws)) {
    res <- res[Sheet == ws]
  }

  res
}

#' Subset sheets by patterns
#'
#' @export
readxl_sheets <- function(wb, patterns = NULL, exclusions = NULL) {
  all_sheets <- readxl::excel_sheets(wb)
  if (!is.null(patterns)) {
    res <- all_sheets[vdetect(all_sheets, patterns, exclusions)]
  } else {
    res <- all_sheets
  }
  res
}

#' Read ranges and cbind the results
#' @export
readxl_dfc <- function(wb, ws = NULL, range, col_names = TRUE, clean_names = TRUE, sep = NULL) {
  if (length(range) == 1 && !is.null(sep)) range <- stringi::stri_trim_both(unlist(stringi::stri_split_regex(range, sep)))

  use_col_names <- length(col_names) == 1 && col_names == TRUE
  list_df <- lapply(range, function(rng) readxl_raw(wb, ws, range = rng, col_names = use_col_names))
  res <- do.call("cbind", list_df)

  if (!is.logical(col_names) & length(col_names) == ncol(res)) data.table::setnames(res, col_names)
  if (clean_names) data.table::setnames(res, makenm_clean)
  res
}


#' Read two columns into list
#' @export
readxl_list <- function(wb, ws = NULL, range, names_id = 1L) {
  rng <- readxl:::standardise_limits(range) # range to min_row, max_row, min_col, max_col
  n_cols <- rng["max_col"] - rng["min_col"] + 1

  if (n_cols != 2) stop("Number of columns needs to be 2", call. = FALSE)

  dt <- readxl_raw(wb, ws, range = xlrange(range), col_names = c("V1", "V2"))
  data.table::setDT(dt)

  res <- as.list(data.table::transpose(dt, make.names = "V1"))

  res
}

#' Read one column or row into vector
#' @export
readxl_vector <- function(wb, ws = NULL, range, data_type = c("text", "date", "numeric")) {
  data_type <- match.arg(data_type)
  rng <- xlrange(range)
  dim_rng <- dim(rng)
  if (!any(dim_rng == 1)) stop("Range must be 1-dimension, i.e. either one row or one column.")

  dt <- readxl_raw(wb, ws, range = rng)

  if (all(dim(dt) == 0)) {
    res <- character(0L)
  } else if (is.na(dim_rng[2]) || dim_rng[2] > 1) {
    res <- data.table::transpose(dt)[[1L]]
  } else {
    res <- dt[[1L]]
  }

  if (data_type == "date") {
    res <- stri_date_parse(res)
  } else if (data_type == "numeric") {
    res <- as.numeric(res)
  }

  res
}

#' Parse Excel range information
#'
#'@export
#'@examples
#'xlrange("A1")
#'xlrange("A1:B3", ws = "Data")
#'xlrange(rows = 2:10)
#'xlrange(rows = c(3, NA)) # NA means to page end
#'xlrange(cols = "A:C")
#'xlrange(rows = 2:5, cols = c(9, NA))
xlrange <- function(range = NULL, rows = NULL, cols = NULL, ws = NULL) {
  if (!is.null(range)) {
    stdRng <- readxl:::standardise_limits(range)
    stdRng[stdRng == -1] <- NA
    stdRng <- stdRng + 1
    rng_byrow <- readxl::cell_rows(stdRng[1:2])
    rng_bycol <- readxl::cell_cols(stdRng[3:4])
  } else {
    rng_byrow <- readxl::cell_rows(rows)
    rng_bycol <- readxl::cell_cols(cols)
  }

  res <- do.call(readxl::cell_limits, Map(data.table::fcoalesce, rng_byrow, rng_bycol))
  if (!is.null(ws)) res$sheet <- ws

  res
}

#'@export
xlrange_offset <- function(range, offset = c(0, 0), size = dim(rng)) {
  if (inherits(range, "cell_limits")) {
    rng <- range
  } else if (is.character(range) && length(range) == 1) {
    rng <- xlrange(range)
  }

  res <- rng
  res$ul <- rng$ul + offset
  res$lr <- res$ul + size - c(1L, 1L)

  res
}
