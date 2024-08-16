#' Tidy up messy raw data
#'
#'
#' @param header_rows An integer vector specifying row numbers for headers.  The rows below will be treated as data, and the rows above will be ignored.
#' @param long_cols Specifying long columns, `.cnames` compatible.
#' @param wide_cols Specifying wide columns, `.cnames` compatible.
#' @param wide_names Names for each layer of wide headers.
#' @param wide_fill Position of wide header layers to be fill (to the right).
#' @param wide_split Split character (e.g. "_") in case wide header is in single row (e.g. in form of "Gross_Premium").
#' @param cols_delete Specifying columns to be deleted, `.cnames` compatible.  `NA` means to delete all-NA headers.
#' @param cols_keepnames Specifying columns to use Original data.frame header, `.cnames` compatible.
#' @param clean_long_names Logical, indicating whether to use `makenm_clean` for long names.
#' @param pivot Logical, indicating whether to pivot wide columns into long data.
#' @param value_name Name of value column.  Only relavent when `pivot` is TRUE
#' @param wide_as_numeric Logical, indicating whether to convert all wide columns into numeric type.
#' @param verbose Logical.  Whether to show more messages.
#'
#' @examples
#' x <- data.table::data.table(
#'           V1 = c("CompA Data", "RiskProfile", NA, "Lbound", "0", "100"),
#'           V2 = c(NA, NA, NA, NA, NA, NA),
#'           V3 = c(NA, NA, NA, "Ubound", "100", "500"),
#'           V4 = c(NA, "Property", "Gross", "No. of Risks", "10", "2"),
#'           V5 = c(NA, NA, NA, "Sum Insured", "1000", "2000"),
#'           V6 = c(NA, NA, NA, "Premium", "1", "2"),
#'           V7 = c(NA, NA, "Net", "N", "10", "2"),
#'           V8 = c(NA, NA, NA, "Sum Insured", "500", "200"),
#'           V9 = c(NA, NA, NA, "Premium", "0.5", "0.2"),
#'          V10 = c(NA, "CAR/EAR", "Gross", "No. of Risks", "5", "0"),
#'          V11 = c(NA, NA, NA, "Sum Insured", "500", "0"),
#'          V12 = c(NA, NA, NA, "Premium", "10", "0"),
#'          V13 = c(NA, NA, "Net", "N", "5", "0"),
#'          V14 = c(NA, NA, NA, "Sum Insured", "100", "0"),
#'          V15 = c(NA, NA, NA, "Premium", "2", "0"),
#'          V16 = c(NA, NA, NA, NA, NA, NA),
#'          V17 = c(NA, NA, NA, "Company", "A", "A")
#'   )
#' (HD <- tidy_header(x[2:4], wide_cols = 4:16, wide_names = c("LOB", "Account", "Var"), cols_delete = NA))
#' tidy_data(x[5:6], HD)
#' tidy_data(x[5:6], HD, pivot = FALSE)
#' (HD2 <- data.table::copy(HD)[grepl("^No\\.", Var), Var := "N"][...col... == 1, ...LONG... := "Lbound"][])
#' tidy_data(x[5:6], HD2)
#' tidy_table(x, header_rows = 2:4, wide_cols = 4:16, wide_names = c("LOB", "Account", "Var"), cols_delete = NA)
#' tidy_table(x, header_rows = 2:4, wide_cols = 4:16, wide_names = c("LOB", "Account", "Var"), pivot = FALSE, cols_delete = NULL)
#' tidy_lift_header(x, 2:4, wide_cols = 4:16)
#' tidy_lift_header(x, 2:4)
#'
#' y <- data.table::data.table(
#'   V1 = c("Gross_SI", 10),
#'   V2 = c("Gross_Prem", 1),
#'   V3 = c("Net_SI", 8)
#' )
#' tidy_table(y, 1, wide_cols = 1:3, wide_names = c("Account", "Var"), wide_split = "_")

#' @export
tidy_table <- function(DT, header_rows, long_cols = NULL, wide_cols = NULL,
                       wide_names = NULL, wide_fill = 1:(length(header_rows) - 1), wide_split = NULL,
                       cols_delete = NULL, cols_keepnames = NULL, clean_long_names = TRUE,
                       pivot = TRUE, value_name = "value", wide_as_numeric = FALSE,
                       verbose = FALSE) {

  DT <- data.table::as.data.table(DT)

  ## delete rows above header_rows
  if (max(header_rows) > nrow(DT)) stop("`header_rows` is out of boundary of rows of DT.", call. = FALSE)
  DT_header <- DT[header_rows]
  DT_data <- DT[!(1:max(header_rows))]

  .tidyHeader <- tidy_header(
    DT_header,
    long_cols = long_cols,
    wide_cols = wide_cols,
    wide_names = wide_names,
    wide_fill = wide_fill,
    wide_split = wide_split,
    cols_delete = cols_delete,
    cols_keepnames = cols_keepnames,
    clean_long_names = clean_long_names,
    verbose = verbose
  )

  res <- tidy_data(DT_data, .tidyHeader, pivot = pivot, value_name = value_name, wide_as_numeric = wide_as_numeric)

  res
}
#' Escape version for tidy_header
#' @export
tidy_header <- function(DT, long_cols = NULL, wide_cols = NULL,
                        wide_names = NULL, wide_fill = 1:(nrow(DT) - 1), wide_split = NULL,
                        cols_delete = NULL, cols_keepnames = NULL, clean_long_names = TRUE, verbose = FALSE) {
  # Preperation
  DT <- data.table::as.data.table(DT)
  x <- data.table::transpose(DT)
  names_DT <- paste(names(DT))

  # Columns classification (into long or wide)
  ## get pre-specified columns names
  is_null_long <- is.null(long_cols)
  is_null_wide <- is.null(wide_cols)
  is_null_delete <- is.null(cols_delete)
  idx_allNA <- colSums(is.na(DT)) == nrow(DT)

  cols_long0 <- .cnames(DT, long_cols)
  cols_wide0 <- .cnames(DT, wide_cols)
  ## More setups for cols_delete
  if (is_null_delete) {
    cols_delete <- NULL
  } else if (is.na(cols_delete) & nrow(DT) > 1) { ## not delete blank 1-row header
    ## cols_delete == NA means to delete allNA columns
    cols_delete <- names_DT[idx_allNA]
  } else {
    cols_delete <- .cnames(DT, cols_delete)
  }

  # cases when long_cols or wide_cols is NULL
  if (is_null_long) {
    if (is_null_wide) {
      ## assume all cols are long
      cols_long <- names_DT
      cols_wide <- NULL
      if (verbose) message("Both `long_cols` and `wide_cols` are NULL.  Assume all columns are long.")
    } else {
      ## assume the rest columns are long
      cols_long <- setdiff(names_DT, cols_wide0)
      cols_wide <- cols_wide0
      if (verbose) message("`long_cols` is NULL and assumed to be as follows: ", paste(cols_long, collapse = ","))
    }
  } else {
    if (is_null_wide) {
      ## assume the rest columns are wide
      cols_long <- cols_long0
      cols_wide <- setdiff(names_DT, cols_long0)
      if (verbose) message("`wide_cols` is NULL and assumed to be as follows: ", paste(cols_wide, collapse = ","))
    } else {
      cols_long <- cols_long0
      cols_wide <- cols_wide0
    }
  }

  ## final clean up to move cols_keepnames to longnames
  cols_long <- unique(c(cols_long, cols_keepnames))
  cols_wide <- setdiff(cols_wide, cols_keepnames)

  ## cols_long and cols_wide should not overlap
  cols_common <- intersect(cols_long, cols_wide)
  if (length(cols_common) > 0) stop("`cols_long` and `cols_wide` have overlaps: ", paste(cols_common, collapse = ", "), call. = FALSE)

  ## Delete those columns that are not specified in either long_cols or wide_cols
  cols_delete <- unique(c(cols_delete, setdiff(names(DT), c(cols_long, cols_wide))))

  # Create flagging columns (e.g. ...Var...)
  ## keep wide_col names (old)
  wide_names_old <- paste(names(x)) ## created by transpose
  ## Create new columns to hold extra information
  x[
    j = `:=`(
      ...col... = as.character(.I),
      ...OriginalHeader... = names(DT)
    )
  ]

  x[, c("...isLong...", "...isWide...", "...delete...") := FALSE]
  x[...OriginalHeader... %in% cols_long, ...isLong... := TRUE]
  x[...OriginalHeader... %in% cols_wide, ...isWide... := TRUE]
  x[...OriginalHeader... %in% cols_delete, ...delete... := TRUE]

  # Create vector for raw header for Long columns
  longname_raw <- dt_unite(data.table::copy(x), cols = wide_names_old, into = "x")[["x"]]

  # Clean up Wide names
  ## split if needed
  if (!is.null(wide_split)) {
    dt_unite(x, 1:nrow(DT), into = "...SingleName...", na.rm = TRUE)
    id_names <- base::setdiff(names(x), "...SingleName...")
    suppressWarnings(dt_separate(x, "...SingleName...", into = wide_names, sep = wide_split))
    wide_names_old <- base::setdiff(names(x), id_names) ## dt_separate may create new names
    wide_names <- stringi::stri_replace_first_fixed(wide_names_old, "...", ".wide")
    data.table::setnames(x, wide_names_old, wide_names)
  } else {
    ## make names if NULL
    if (is.null(wide_names)) {
      wide_names <- paste0(".wide", seq_along(wide_names_old))
    } else if (length(wide_names) != nrow(DT)) {
      stop("Length of wide_names is ", length(wide_names), ", but headerDT has ", nrow(DT), " rows", call. = FALSE)
    }

    ## Fill down if needed, both allNA and long_cols will be NA
    wide_fill <- intersect(wide_fill, seq_along(wide_names_old))
    if (!is.null(wide_fill)) {
      x[j = (wide_fill) := lapply(.SD, vnafill, direction = "down", breaks = idx_allNA | names_DT %in% cols_long),
        .SDcols = wide_fill]
    }

    ## Delete column if wide_names has NA
    cols_to_drop <- colnames(x)[which(is.na(wide_names))]
    if (length(cols_to_drop) > 0) x[, (cols_to_drop) := NULL]
    wide_names_old <- wide_names_old[!is.na(wide_names)]
    wide_names <- wide_names[!is.na(wide_names)]
    data.table::setnames(x, wide_names_old, wide_names)
    #### Note: from now on, ncol(x) for wide columns might be fewer than nrow(DT), because of above dropping operation
  }

  # Clean up Long names
  ## Use cleaned wide names to create long names, in case user does not want to pivot using tidy_data
  dt_unite(x, cols = wide_names, into = "...LONG...", sep = "_", remove = FALSE, na.rm = TRUE)
  x[, ...LONG... := data.table::fifelse(...isLong..., longname_raw, ...LONG...)]

  ## use original_header for long if specified
  if (is.null(cols_keepnames)) {
    cols_useNames <- NULL
  } else {
    cols_useNames <- intersect(.cnames(DT, cols_keepnames), cols_long) ## can only use long_cols' original names
  }

  x[...OriginalHeader... %in% cols_useNames, ...LONG... := ...OriginalHeader...]
  ## make long names for blanks and NAs
  x[...LONG... == "" | is.na(...LONG...), ...LONG... := paste0("V", ...col...)]

  ## Clean up wide columns contents
  x[...isLong... == TRUE, (wide_names) := NA] ## long cols should not have wide names
  if (!x[, any(...isWide...)]) x[, (wide_names) := NULL]

  ## Clean up longnames if needed
  if (clean_long_names) x[, ...LONG... := makenm_clean(...LONG...)]

  x[]
}

#'
#' @export
#' @rdname tidy_table

tidy_data <- function(DT, tidyHeader, pivot = TRUE, value_name = "value", wide_as_numeric = TRUE) {
  DT <- data.table::as.data.table(DT)
  if (ncol(DT) != nrow(tidyHeader)) stop("Number of headers cannot match column number of DT.", call. = FALSE)

  ## rename DT for easier joining
  data.table::setnames(DT, as.character(seq_along(DT)))

  ## Names
  wide_names <- names(tidyHeader)[!grepl("...", names(tidyHeader), fixed = TRUE)]
  wide_cols <- tidyHeader[...isWide... == TRUE, (...col...)]

  ## delete cols
  cols_delete <- tidyHeader[...delete... == TRUE, as.integer(...col...)]
  if (length(cols_delete) > 0) {
    DT[, (cols_delete) := NULL]
    wide_cols <- setdiff(wide_cols, cols_delete)
  }

  # if not pivot
  if (!pivot | length(wide_names) == 0) {
    res <- data.table::copy(DT)
    if (wide_as_numeric & length(wide_cols) > 0) res[, (wide_cols) := lapply(.SD, as.numeric), .SDcols = wide_cols]
    ## all use cleaned long names
    data.table::setnames(res, tidyHeader[["...col..."]], tidyHeader[["...LONG..."]], skip_absent = TRUE)
    return(res[])
  }

  # Pivot case
  x <- data.table::melt(DT, measure.vars = wide_cols, ## layer_cols are names rather than positions, as some DT cols has been deleted
                        variable.name = "...col...", variable.factor = FALSE,
                        value.name = value_name)

  ## Rename
  data.table::setnames(x, tidyHeader[["...col..."]], tidyHeader[["...LONG..."]], skip_absent = TRUE)

  ## Join header and data
  res <- tidyHeader[...isWide... == TRUE, c("...col...", wide_names), with = FALSE
  ][x, on = "...col..."]

  res[, c("...col...") := NULL]

  ## rearrange column orders
  data.table::setcolorder(res, c(setdiff(colnames(res), c(wide_names, value_name)), wide_names, value_name))

  ## numeric value
  if (wide_as_numeric) res[, (value_name) := lapply(.SD, as.numeric), .SDcols = value_name]

  res[]
}

#' @export
#' @rdname tidy_table
tidy_lift_header <- function(DT, header_rows, long_cols = NULL, wide_cols = NULL,
                             wide_fill = 1:(length(header_rows) - 1), clean_names = TRUE,
                             cols_delete = NULL, cols_keepnames = NULL, wide_as_numeric = FALSE) {
  res <- tidy_table(
    DT,
    header_rows,
    long_cols = long_cols,
    wide_cols = wide_cols,
    wide_fill = wide_fill,
    clean_long_names = clean_names,
    pivot = FALSE,
    wide_as_numeric = wide_as_numeric,
    cols_delete = cols_delete,
    cols_keepnames = cols_keepnames
  )
  res
}


.cnames <- function(DT, cols = NULL) if (is.null(cols)) return(names(DT)) else return(DT[, names(.SD), .SDcols = cols])

