#' Create a RiskProfile object
#' @examples
#' x <- data.frame(N = c(100, 50, 10), SumInsured = c(100 * 10, 50 * 20, 10 * 30), Prem = c(10, 10, 10))
#' rp <- RiskProfile(x, TSI = "SumInsured", Premium = "Prem")
#' rp
#'
#' ## Scale a Risk Profile
#' fScale(rp, 3)
#' fScale(rp, c(3, 2, 0))
#' @export
RiskProfile <- function(x, ...) {
  RP <- field_map(x, ...) |>
     field_check(required_field = c("N", "TSI", "Premium"))

   structure(RP, class = c("RiskProfile", class(RP)))
}

#' @export
print.RiskProfile <- function(x, ...) {
  cat("<RiskProfile>\n")
  data.table:::print.data.table(x, topn = 3, nrows = 8, calss = FALSE)
  invisible(x)
}

#' @export
#' @rdname RiskProfile
#' @usage
#' ## Scale a Risk Profile
#' fScale(RiskProfile, k)
fScale.RiskProfile <- function(x, k = 1) {
  # if (!length(k) == 1 & !length(k) == nrow(x)) stop("`k` must be atomic, or with the same length as the number of rows of `x`.", call. = FALSE)

  data.table::copy(x)[, `:=`(N = k * N, TSI = k * TSI, Premium = k * Premium)]
}

