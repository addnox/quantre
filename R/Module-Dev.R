#' Methods to estimate ultimates for a triangle
#'
#' @param x A Triangle object
#' @param link_ratio A numeric vector of link ratios
#' @param tail_factor A numeric number for tail factor
#' @param expected_loss A numeric vector of expected losses, used for B-F method
#' @param premium A numeric vector of premiums, used for Cape-Cod method
#' @export
#' @examples
#' loss <- Triangle(TRI$Incurred)
#' ata <- tri_LinkRatioAverage(loss, "simple")
#' premium <- seq(10000, 28000, by = 2000)
#'
#' # Chain Ladder
#' ChainLadder(loss, link_ratio = ata)
#'
#' # B-F
#' ELoss_BF <- c(5000, 6000, 7000, 8000, 9000, 8000, 8800, 9600, 10400, 11200)
#' BornhuetterFerguson(loss, ELoss_BF, link_ratio = ata)
#'
#' # Cape-Cod
#' CapeCod(loss, premium, ata)
#' CapeCod_ELR(loss, premium, ata) ## CapeCod ELR that is used in `CapeCod` function call
ChainLadder <- function(x, link_ratio = NULL, tail_factor = 1) {
  if (!inherits(x, "Triangle")) stop("`x` must be a `Triangle` object.", call. = FALSE)
  if (is.null(link_ratio)) link_ratio <- tri_LinkRatioAverage(x, "weighted")
  # determine age-to-age factor (ata)
  report_pct <- tri_ReportedPercent(link_ratio, tail_factor)

  tri_FullTriangle(x, report_pct)
}

#' @export
#' @rdname ChainLadder
BornhuetterFerguson <- function(x, expected_loss, link_ratio = NULL, tail_factor = 1) {
  if (!inherits(x, "Triangle")) stop("`x` must be a `Triangle` object.", call. = FALSE)
  if (is.null(link_ratio)) link_ratio <- tri_LinkRatioAverage(x, "weighted")
  report_pct <- tri_ReportedPercent(link_ratio, tail_factor)

  ult_loss <- tri_LatestDiagonal(x) + (1 - rev(report_pct)) * expected_loss ## rev so earliest year comes first

  tri_FullTriangle(x, report_pct = report_pct, ultimate_loss = ult_loss)
}

#' @export
#' @rdname ChainLadder
BF <- BornhuetterFerguson

#' @export
#' @rdname ChainLadder
CapeCod_ELR <- function(x, premium, link_ratio = NULL, tail_factor = 1) {
  if (!inherits(x, "Triangle")) stop("`x` must be a `Triangle` object.", call. = FALSE)
  if (is.null(link_ratio)) link_ratio <- tri_LinkRatioAverage(x, "weighted")

  report_pct <- rev(tri_ReportedPercent(link_ratio, tail_factor)) ## rev so that earliest year comes first
  latest_loss <- tri_LatestDiagonal(x)

  sum(latest_loss) / sum(premium * report_pct)
}

#' @export
#' @rdname ChainLadder
CapeCod <- function(x, premium, link_ratio = NULL, tail_factor = 1) {
  if (!inherits(x, "Triangle")) stop("`x` must be a `Triangle` object.", call. = FALSE)
  if (is.null(link_ratio)) link_ratio <- tri_LinkRatioAverage(x, "weighted")

  ELR <- CapeCod_ELR(x, premium, link_ratio, tail_factor)
  BornhuetterFerguson(x, ELR * premium, link_ratio, tail_factor)
}
