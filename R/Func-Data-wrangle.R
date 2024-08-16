#' Make names from a character vector
#'
#' @param blank_only Only change names for blank items, i.e. keep original non-NA names.
#' @param prefix Prefix of numbered names, e.g. "v" as a prefix for "v1".
#' @export
#' @examples
#' x <- c(NA, "Gross Premium ", "Loss --- Incurred", "Premium/Limit", "# of Risks", "Adj. Rate (%)", NA, "GrossPremium")
#' makenm_numbered(x)
#' makenm_numbered(x, prefix = "...")
#' makenm_numbered(x, blank_only = TRUE)
#' makenm_unique(c("A", "B", "A", NA))
#' makenm_clean(x)
#' makenm_clean(x, style = "title")
#' makenm_clean(x, sep = "")
#' makenm_clean(x, style = "asis", sep = "", allow_na = TRUE, allow_dup = TRUE)
#' ## A more common use of makenm functions are for reading data into tibbles using packages like readr or readxl
#' list(LETTERS[1:5], 1:5) |> tibble::as_tibble(.name_repair = makenm_numbered)

makenm_numbered <- function(x, prefix = "V", blank_only = FALSE) {
  if (length(x) == 0L) {
    return(character(0L))
  } else {
    name_numbered <- paste0(prefix, seq_along(x))
  }

  if (blank_only) {
    name_final <- ifelse(x == "" | is.na(x), name_numbered, x)
  } else {
    name_final <- name_numbered
  }

  return(name_final)
}

#' @rdname makenm_numbered
#' @export
makenm_unique <- function(x, prefix = "V") {
  # clean all blanks and NAs
  x2 <- makenm_numbered(x, prefix = prefix, blank_only = TRUE)

  # return x2 if no dups
  if (!anyDuplicated(x2)) return(x2)

  # find dups
  xDT <- data.table::data.table(x = x2)
  xDT[, `:=`(N = seq_len(.N), dup = .N > 1), by = "x"]
  xDT[, newname := data.table::fifelse(dup, paste(x, N, sep = "..."), as.character(x))]

  return(xDT[["newname"]])
}

#' `makenm_clean` is inspired by `janitor::make_clean_names` and `snakecase::to_any_case`
#'
#' @param style Case style of names, will change the case of prefix if applicable.
#' @param allow_na If FALSE, will replace `NA`s to v-prefix names, e.g. "v1".
#' @param allow_dup If FALSE, duplicated occurrence will be labeled. e.g. "Var", "Var2", "Var3".  Note that it is case-sensitive so "var" and "Var" are viewed as different names.
#' @rdname makenm_numbered
#' @export
makenm_clean <- function(x, prefix = "V", sep = "_", style = c("asis", "lower", "title", "upper"), allow_na = FALSE, allow_dup = FALSE) {
  style <- match.arg(style)
  f_style <- switch (style,
                     "asis" = function(x) x,
                     "lower" = stringi::stri_trans_tolower,
                     "title" = stringi::stri_trans_totitle,
                     "upper" = stringi::stri_trans_toupper
  )

  na_pos <- is.na(x) ##NA position for future use

  v_pattern <- c("'", "\"", "%", "#", "+", "$")
  v_replace  <- c("", "", "_percent_", "_number_", "_plus_", "_dollar_")
  replaced_names <- stringi::stri_replace_all_fixed(str = x, v_pattern, v_replace, vectorize_all = FALSE)

  good_start <- replaced_names |>
    stringi::stri_replace_first_regex(pattern = "\\A[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]*(.*)$",
                                      replacement = "$1") |> ## stringi uses $1 instead of \\1
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_trim_both() |>
    makenm_numbered(prefix = prefix, blank_only = TRUE)

  if (style == "asis") {
    made_names <- makenm_numbered(good_start, blank_only = TRUE, prefix = prefix)
  } else {
    made_names <- make.names(good_start)
  }

  cased_names_list <- made_names |>
    stringi::stri_replace_all_regex("[^[:alnum:]]", "_") |>
    stringi::stri_split_fixed(pattern = "_")

  cased_names <- cased_names_list |>
    lapply(function(x) {return(x[x != ""])}) |>
    lapply(f_style) |>
    vapply(paste, collapse = sep, character(1L), USE.NAMES = FALSE)

  if (!allow_dup) cased_names <- makenm_unique(cased_names, prefix = prefix)

  if (allow_na) cased_names[na_pos] <- NA_character_

  cased_names
}

#' Patch new data y to old dataset x
#'
#' Essentially a safer way of doing left_join
#' @param x Data to be patched
#' @param y Patching dataset
#' @param by A character vector specifying common join ids
#' @param vars A character vector specifying common fields to be patched
#' @param ties Specify what to do when both `x` and `y` has valid values for `vars`
#' @export
#' @examples
#' dt1 <- data.table::data.table(ID1 = c(1, 1, 2, 2, 3), ID2 = c("A", "B", "A", "C", "B"), v1 = 1:5, v2 = 5:1)
#' dt2 <- data.table::data.table(ID1 = c(1, 2, 3), ID2 = "A", v1 = 11L, v2 = 22L)
#' dt_patch(dt1, dt2, by = c("ID1", "ID2"), vars = c("v1"))
#' dt_patch(dt1, dt2, by = c("ID1", "ID2"), vars = c("v1", "v2"))

dt_patch <- function(x, y, by, vars = NULL, ties = c("y", "x")) {
  x <- data.table::as.data.table(x)
  y <- data.table::as.data.table(y)
  ties <- match.arg(ties)
  if (is.null(vars)) vars <- base::setdiff(names(y), by)
  if (!all(by %in% colnames(x))) stop("Not all variables in `by` are in x", call. = FALSE)
  if (!all(by %in% colnames(y))) stop("Not all variables in `by` are in y", call. = FALSE)
  if (!all(vars %in% colnames(x))) stop("Not all variables in `vars` are in x", call. = FALSE)
  if (!all(vars %in% colnames(y))) stop("Not all variables in `vars` are in y", call. = FALSE)
  if (anyDuplicated(y, by = by)) stop("Variables in `by` cannot uniquely defined data `y`", call. = FALSE)

  ## Joining
  y_cols <- c(by, vars)
  x_full <- merge(x, y[, ..y_cols], by = by, all.x = TRUE, all.y = FALSE, suffixes = c("...x", "...y"), sort = FALSE)

  ## Coalescing
  for (patch_var in vars) {
    var_x <- paste0(patch_var, "...x")
    var_y <- paste0(patch_var, "...y")
    value_x <- x_full[[var_x]]
    value_y <- x_full[[var_y]]

    if (typeof(value_x) != typeof(value_y)) { #`typeof` will treat Date and POSIXct as double
      stop("Different types: ", var_x, " - ", typeof(value_x), "; ", var_y, " - ", typeof(value_y), call. = FALSE)
    }

    if (ties == "x") {
      data.table::set(x_full, j = patch_var, value = data.table::fcoalesce(value_x, value_y))
    } else {
      data.table::set(x_full, j = patch_var, value = data.table::fcoalesce(value_y, value_x))
    }
  }

  ## Delete ...x and ...y vars
  var_pairs <- data.table::CJ(var = vars, xy = c("...x", "...y"))[, paste0(var, xy)] ## e.g. A...x, A...y
  x_full[, (var_pairs) := NULL]

  ## Restore dataset x col sequence
  data.table::setcolorder(x_full, colnames(x))

  return(x_full[])
}

#' Similar to `tibble::trible` to create `data.table` using row-by-row input
#' @export
#' @param ...
#'   Arguments specifying the structure of a `data.table`.
#'   Variable names should be formulas, and may only appear before the data.
#' @examples
#' data.trable(
#' ~colA, ~colB,
#' "a",   1,
#' "b",   2,
#' "c",   3
#' )

data.trable <- function(...) {
  dots <- list(...)
  headers <- Filter(function(x) class(x) == "formula", dots)
  headers_label <- vapply(headers, function(x) as.character(as.list(x)[[2]]), character(1L))
  n_col <- length(headers)
  if (n_col == 0) stop("No header is found", call. = FALSE)
  if (length(dots) %% n_col != 0) stop("Length of `...` cannot be divided by the number of headers", call. = FALSE)
  n_row <- length(dots) / n_col - 1

  res_list <- vector("list", n_col)
  dots_data <- dots[-seq_len(n_col)]
  for (i in 1:n_col) {
    res_list[[i]] <- Reduce(c, dots_data[seq(from = i, by = n_col, length.out = n_row)])
    #class(res_list[[i]]) <- class(dots_data[[i]])
  }

  names(res_list) <- headers_label
  data.table::as.data.table(res_list)
}

#' By-ref equivalent of `tidyr::separate`
#'
#' @param dt A `data.table` or `data.frame`.
#' @param col Column name to be split.
#' @param into Names of new variables to create as character vector.
#' @param sep Separator between columns.
#' @param remove If `TRUE`, remove input column from output data frame.
#' @param extra Controls when there are too many pieces after split.
#'
#' @export
#' @examples
#' df <- data.table(x = c("A_B_C", "D_E_F"), y = 1:2, z = 3:4)
#' dt_separate(copy(df), col = "x")[]
#' dt_separate(copy(df), col = "x", into = paste0("v", 1:2))[]
#' dt_separate(copy(df), col = "x", into = paste0("v", 1:2), extra = "merge")[]
#' dt_separate(copy(df), col = "x", into = paste0("v", 1:2), extra = "drop")[]
#' dt_separate(copy(df), col = "x", into = paste0("v", 1:4))[]

dt_separate <- function(dt, col, into = NULL, sep = "[^[:alnum:]]+", remove = TRUE, extra = c("complete", "merge", "drop")) {
  dt <- data.table::setDT(dt)
  extra <- match.arg(extra)
  idx_col <- which(names(dt) == col)

  data_split <- data.table::as.data.table(data.table::tstrsplit(dt[[col]], split = sep))
  n_split <- ncol(data_split)
  n_into <- length(into)

  if (n_split < n_into) {
    warning("Split columns are more than the length of `into`.  Unused `into` names will be dropped.", call. = FALSE)
    into <- into[seq_len(n_split)]
    n_into <- length(into)
  }

  raw_names <- paste0("...", seq_len(n_split))
  raw_names[seq_along(into)] <- into
  data.table::setnames(data_split, raw_names)

  if (n_split > n_into & n_into > 0) {
    if (extra == "merge") {
      cols_merge <-  seq(n_into, n_split)
      dt_unite(data_split, cols = cols_merge, into = "...merged...")
      data.table::setnames(data_split, into)
    } else if (extra == "drop") {
      cols_drop <- seq(n_into + 1, n_split)
      data_split[, (cols_drop) := NULL]
    }
  }

  dt[, names(data_split) := data_split]

  cols_seq <- c(names(dt)[1:idx_col], names(data_split))
  data.table::setcolorder(dt, cols_seq)
  if (remove & !(col %in% into)) dt[, (col) := NULL]

  dt
}
#' By-ref equivalent of `tidyr::unite`
#'
#' @param DT A `data.table` or `data.frame`.
#' @param cols A char vector containing the colnames to be united.
#' @param into The name of the new column, as a string.
#' @param sep Separator to use between values.
#' @param remove If `TRUE`, remove input columns from output data frame.
#' @export
#' @examples
#' df <- as.data.table(expand.grid(x = c("a", NA), y = c("b", NA)))
#' dt_unite(copy(df), c("x", "y"), into = "new")[]
#' dt_unite(copy(df), c("x", "y"), into = "new", sep = "...", remove = FALSE)[]
#' dt_unite(copy(df), into = "new")[]
#' dt_unite(copy(df), into = "new", na.rm = TRUE)[]
dt_unite <- function(DT, cols = NULL, into, sep = "_", remove = TRUE, na.rm = TRUE) {
  DT <- data.table::setDT(DT)

  if (is.null(cols)) cols <- paste(names(DT)) # use paste, otherwise colnames is by-ref and changing

  if (na.rm) {
    tDT <- data.table::transpose(DT[, ..cols])
    united <- vapply(tDT, function(x) paste0(x[!is.na(x)], collapse = sep), character(1L))
    DT[, (into) := data.table::fifelse(united == "", NA_character_, united)]
  } else {
    DT[, (into) := do.call(paste, c(.SD, sep = sep)), .SDcols = cols]
  }

  if (remove & length(setdiff(cols, into)) > 0) DT[, setdiff(cols, into) := NULL]

  DT
}
