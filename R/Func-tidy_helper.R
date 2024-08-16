#' Split data into list of `data.table`s horizontally or vertically
#'
#' @param x A data.frame
#' @param by Numeric vector indicating position
#' @param na.rm When TRUE, those rows with all `cols` being `NA` will be deleted before splitting
#' @examples
#' DT1 <- data.table(ID1 = c(NA, "A", "A", NA, NA, "B"), ID2 = c(rep("1", 3), NA, "2", "2"), X1 = NA, X2 = c(NA, 1:2, NA, NA, 3), X3 = c(rep(NA, 5), .1))
#' tidy_hsplit(DT1)
#' tidy_hsplit(DT1, by = 1:2)
#' tidy_vsplit(data.table::transpose(DT1))
#'
#' DT2 <- data.table(ID1 = c(1, NA, NA, 2, NA), ID2 = c("A", NA, "B", "C", NA), X1 = 1:5, X2 = LETTERS[1:5])
#' tidy_hsplit(DT2, by = 1:2)
#'
#' @export
tidy_hsplit <- function(x, by = NULL, na.rm = FALSE, keep.by = TRUE, factor.by = FALSE) {
  res <- tidy_hsplit_(x, by = by, na.rm = na.rm, keep.by = keep.by, factor.by = factor.by)
  res <- lapply(res, function(x) {data.table::setattr(x, "row.names", seq_len(nrow(x))); x})
  res
}

#' @rdname tidy_hsplit
#' @export
tidy_vsplit <- function(x, by = NULL, na.rm = TRUE, keep.by = TRUE, factor.by = FALSE) {
  stopifnot(is.integer(by) | is.null(by) | factor.by)
  x <- data.table::as.data.table(x)
  x1 <- data.table::transpose(x)
  rownames(x1) <- names(x)

  res_t <- tidy_hsplit_(x1, by = by, na.rm = na.rm, keep.by = keep.by, factor.by = factor.by)
  res <- lapply(res_t, function(x) data.table::setnames(data.table::transpose(x), rownames(x)))
  res <- lapply(res, function(x) {data.table::setattr(x, "row.names", seq_len(nrow(x))); x})
  res
}

#' @keywords internal
tidy_hsplit_ <- function(x, by = NULL, na.rm = TRUE, keep.by = TRUE, fill.by = TRUE, factor.by = FALSE) {
  # tidy_hsplit_q is the escape version.  It preserves rownames mainly for tidy_vsplit

  x1 <- data.table::as.data.table(x)

  idx_blank <- rowSums(!is.na(as.matrix(x1))) == 0 ## all-blank rows
  x1[, ...rn... := rownames(x1)] ## once added, there will be no all-blank rows

  ## get break positions
  if (is.null(by)) {
    ## use blank rows to split
    idx_blocks <- data.table::data.table(data.table::rleidv(idx_blank))
  } else if (factor.by == TRUE & length(by) == nrow(x1)) {
    idx_blocks <- data.table::data.table(by)
  } else {
    ## use repeat values
    idx_blocks <- x1[, ..by]
    if (fill.by) idx_blocks[, (by) := lapply(.SD, vnafill, direction = "down")]
  }

  # clean up blanks
  by_vars <- paste0("...by", seq_along(idx_blocks))
  data.table::setnames(idx_blocks, by_vars)

  ## split
  x2 <- cbind(x1, idx_blocks)[!idx_blank] ## all-blank rows will be deleted automatically

  if (na.rm) {
    all_NA_ids <- rowSums(is.na(x2[, ..by_vars])) == length(by_vars)
    x2 <- x2[!all_NA_ids]
  }

  res <- split(x2, by = by_vars, drop = TRUE, keep.by = FALSE) ## use by.  Using f will result in re-ordered list elements

  ## delete names for blank cuts
  if (is.null(by)) {
    data.table::setattr(res, "names", NULL)
  }

  ## delete rowname column
  res <- lapply(res, function(x) {data.table::setattr(x, "row.names", x[["...rn..."]]); x[, ...rn... := NULL]; x})

  ## keep split-by cols
  if (!keep.by & !is.null(by)) res <- lapply(res, function(DT) DT[, (by) := NULL])

  res
}

#' Trim all-blank row and cols of a data.table
#'
#' @param DT A `data.table`
#' @export
#' @examples
#' DT <- data.table(A = c(NA, 1, 2, NA, NA, 3), B = NA, C = c(NA, "A", NA, NA, NA, "B"), D = NA)
#' tidy_trim(DT)

tidy_trim <- function(DT) {
  m <- as.matrix(data.table::as.data.table(DT))
  m_na <- is.na(m)

  rows_allna <- rowSums(!m_na) == 0
  cols_allna <- colSums(!m_na) == 0

  res <- DT[!rows_allna, !..cols_allna]
  rownames(res) <- rownames(DT)[!rows_allna]
  res
}

#' Return row index (or logical vector) that match the pre-defined patterns
#'
#' @inheritParams vdetect
#' @export
#' @examples
#' x <- data.table(
#'   c("ABC P&C Limited", "Layer", "Limit", NA, 10, 10, 10, "Total Limit"),
#'   c(NA, NA, "Excess", NA, 10, 20, 30, "")
#' )
#' tidy_where(x, "limit")
#' tidy_where(x, c("limit|excess"), exclusions = "ABC")
#' tidy_which(x, c("limit|excess"), exclusions = "ABC")

tidy_where <- function(DT, patterns, exclusions = NULL) {
  DT <- data.table::as.data.table(DT)

  res <- vapply(DT, vdetect, logical(nrow(DT)), patterns = patterns, exclusions = exclusions, USE.NAMES = FALSE)

  res
}

#' @export
#' @rdname tidy_where

tidy_which <- function(DT, patterns, exclusions = NULL, as.DT = TRUE) {
  res_where <- tidy_where(DT, patterns, exclusions)
  res <- which(res_where, arr.ind = TRUE, useNames = FALSE)
  if (as.DT) {
    res <- data.table::as.data.table(res)
    data.table::setnames(res, c("row", "col"))
  }
  res
}

