#' `Triangle`: Create a triangle object
#' @export
#' @examples
#' loss <- Triangle(TRI$Incurred)
#'
#' ## two triangles can do arithmetic
#' loss / loss
#' loss * 2
#'
#' # get latest diagonal
#' tri_LatestDiagonal(loss)
#'
#' # Take a look at different averages for link ratio
#' tri_LinkRatioSummary(loss)
#'
#' # Use selected link ratios to develope a triangle
#' ata <- tri_LinkRatioAverage(loss, "weighted", 5) ## weighted avg. for latest 5 years
#' reportPct <- tri_ReportedPercent(ata, tail_factor = 1.02)
#' tri_FullTriangle(loss, reportPct)

Triangle <- function(x, from = 12, by = 12, latest = from) {
  if (!inherits(x, "data.frame")) stop("`x` must be a data.frame.", call. = FALSE)
  xOrigin <- x[[1L]]
  xData <- x[, -1L]

  dev <- seq(from = from, by = by, length.out = ncol(xData))

  mat <- as.matrix(xData)
  dimnames(mat) <- list("origin" = xOrigin, "dev" = dev) ## assign names

  structure(
    mat,
    class = c("Triangle", "matrix"),
    dev_latest = latest
  )
}

#' @export
print.Triangle <- function(x, ...) {
  cat("<Triangle>\n")
  xOrigin <- rownames(x)
  xDev <- colnames(x)
  xDev_from <- xDev[1L]
  xDev_latest <- attr(x, "dev_latest")

  cat(stringi::stri_pad_left("Origin: ", 20), format_Numeric(xOrigin, "dim"), "\n", sep = "")
  cat(stringi::stri_pad_left("Development: ", 20), "from.month = ", xDev_from, "; latest.month = ", xDev_latest, "\n", sep = "")
  cat(stringi::stri_pad_left("Data: ", 20), format_Numeric(x, "dim"), "\n", sep = "")
  cat(stringi::stri_pad_left("LatestDiagonal: ", 20), format_Numeric(tri_LatestDiagonal(x), "dim"), "\n", sep = "")
  cat("\n")
  data.table:::print.data.table(
    data.table::as.data.table(x, keep.rownames = "origin"),
    class = FALSE, row.names = FALSE, trunc.cols = TRUE
  )

  invisible(x)
}

#' `tri_LatestDiagonal`: Get latest diagonal from a Triangle
#' @export
#' @rdname Triangle
tri_LatestDiagonal <- function(x) {
  res <- unlist(apply(x, 1L, function(z) data.table::last(na.omit(z)), simplify = FALSE))
  res
}

#' `tri_FullTriangle`: Develope and fill bottom-right part of the triangle
#' @export
#' @rdname Triangle

tri_FullTriangle <- function(x, report_pct, ultimate_loss = NULL) {
  if (is.null(ultimate_loss)) {
    # forwardly loop through columns (i.e. dev)
    res <- cbind(x, "ultimate" = NA_real_)
    cumLinkRatio <- 1 / report_pct
    link_ratio <- cumLinkRatio[-length(cumLinkRatio)] / cumLinkRatio[-1L]
    tail_factor <- cumLinkRatio[length(cumLinkRatio)]
    for (j in seq_len(ncol(x))) {
      col_from <- res[, j]
      col_to <- res[, j + 1]
      res[is.na(col_to), j + 1] <- (col_from * link_ratio[j])[is.na(col_to)]
    }
    res[, ncol(x) + 1] <- res[, ncol(x)] * tail_factor
  } else {
    res <- cbind(x, "ultimate" = ultimate_loss)
    for (j in rev(seq_len(ncol(x)))) {
      col_from <- res[, j + 1]
      col_to <- res[, j] ## backward fill
      res[is.na(col_to), j] <- (col_from * rev(report_pct)[j])[is.na(col_to)]
    }
  }

  res
}

#' `tri_LinkRatio`: Get age-to-age link ratio for each origin row
#' @export
#' @rdname Triangle
tri_LinkRatio <- function(x) {
  if (!inherits(x, "Triangle")) stop("`x` must be a `Triangle` object.", call. = FALSE)
  nmx <- colnames(x)
  tri_from <- x[, -ncol(x)]
  tri_to <- x[, -1L]

  res <- tri_to / tri_from
  colnames(res) <- paste0(nmx[-length(nmx)], "-", nmx[-1L])
  res
}

#' `tri_LinkRatioSummary`: Quick summary of commonly used link ratio averages
#' @export
#' @rdname Triangle
tri_LinkRatioSummary <- function(x) {
  resList <- list(
    "Simple" = tri_LinkRatioAverage(x, "simple"),
    "Simple_Latest5" = tri_LinkRatioAverage(x, "simple", window = 5),
    "Simple_Latest3" = tri_LinkRatioAverage(x, "simple", window = 3),
    "Weighted" = tri_LinkRatioAverage(x, "weighted"),
    "Weighted_Latest5" = tri_LinkRatioAverage(x, "weighted", window = 5),
    "Weighted_Latest3" = tri_LinkRatioAverage(x, "weighted", window = 3),
    "Weighted_Latest5x1" = tri_LinkRatioAverage(x, "weighted", window = 4, exclude = 1),
    "Geometric_Latest5" = tri_LinkRatioAverage(x, "geometric", window = 3)
  ) |>
    lapply(function(z) data.table::as.data.table(t(z)))

  res <- data.table::rbindlist(resList, idcol = "Method")
  res
}

#' `tri_LinkRatioAverage`: Calculate average link ratio factors for development
#' @export
#' @rdname Triangle
tri_LinkRatioAverage <- function(x, type = c("weighted", "simple", "geometric"), window = Inf, exclude = 0) {
  if (!inherits(x, "Triangle")) stop("`x` must be a `Triangle` object.", call. = FALSE)
  type <- match.arg(type)
  xList <- tri_block(x, window = window, exclude = exclude)

  f <- switch(
    type,
    "simple" = function(m) if (!is.matrix(m)) 1 else mean(m[, 2] / m[, 1], na.rm = TRUE),
    "weighted" = function(m) if (!is.matrix(m)) 1 else sum(m[, 2], na.rm = TRUE) / sum(m[, 1], na.rm = TRUE),
    "geometric" = function(m) if (!is.matrix(m)) 1 else exp(mean(log(m[, 2] / m[, 1]), na.rm = TRUE))
  )

  vapply(xList, f, double(1L))
}

#' `tri_ReportedPercent`: Convert age-to-age link ratios to reported percents
#' @export
#' @rdname Triangle
tri_ReportedPercent <- function(link_ratio, tail_factor = 1) {
  res <- 1 / tri_CDF(link_ratio, tail_factor)
  unname(res)
}

#' `tri_CDF`: Convert age-to-age link ratios to cumulative development factors
#' @export
#' @rdname Triangle
tri_CDF <- function(link_ratio, tail_factor = 1) {
  ata <- c(link_ratio, tail_factor)
  cdf <- rev(cumprod(rev(ata)))
  unname(cdf)
}

#' Split triangle into pairs of development years for further average link ratio calculation
tri_block <- function(x, window = Inf, exclude = 0) {
  if (!inherits(x, "Triangle")) stop("`x` must be a `Triangle` object.", call. = FALSE)
  nRow <- nrow(x)
  IdxRow <- 1:nRow
  res <- vector("list", ncol(x) - 1)
  nmx <- colnames(x)
  names(res) <- paste0(nmx[-length(nmx)], "-", nmx[-1L])

  for (i in seq_along(res)) {
    n_NA <- i
    n_data <- nRow - n_NA
    pos_row_end <- n_data - exclude
    pos_row_start <- pmax(1, pos_row_end - window + 1)

    if (pos_row_start <= pos_row_end) {
      pos_row <- seq(pos_row_start, pos_row_end, by = 1)
      pos_col <- c(i, i + 1)
      res[[i]] <- x[pos_row, pos_col, drop = FALSE]
    } else {
      res[[i]] <- NA
    }
  }

  res
}
