#' Generic function for mean
#' @export
fMean <- function(x, ...) UseMethod("fMean")

#' @rdname fMean
#' @export
fMean.default <- function(x, na.rm = TRUE) {
  mean(x, na.rm = na.rm)
}

#' Generic function for standard deviation
#' @rdname fMean
#' @export
fSD <- function(x, ...) UseMethod("fSD")

#' @rdname fMean
#' @export
fSD.default <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm)
}

#' Generic function for percentile (i.e. inverse of Value at Risk)
#' @rdname fMean
#' @export
fPerc <- function(x, ...) UseMethod("fPerc")

#' @rdname fMean
#' @export
fPerc.default <- function(x, q) {
  ecdf(x)(q)
}

#' Generic function for Value at Risk (i.e. quantile)
#' @rdname fMean
#' @export
fVaR <- function(x, ...) UseMethod("fVaR")

#' @rdname fMean
#' @export
fVaR.default <- function(x, prob) {
  quantile(x, prob, names = FALSE, type = 2)
}

#' Generic function for Tail Value at Risk (a.k.a Conditional Tail Expectation, or CTE)
#' @rdname fMean
#' @export
fTVaR <- function(x, ...) UseMethod("fTVaR")

#' @rdname fMean
#' @export
fTVaR.default <- function(x, prob) {
  vapply(
    prob,
    function(.p) collapse::fmean(x[x >= quantile(x, .p, names = FALSE, type = 2)]),
    double(1L)
  )
}

#' Generic function for minimum
#' @rdname fMean
#' @export
fMin <- function(x, ...) UseMethod("fMin")

#' @rdname fMean
#' @export
fMin.default <- function(x, na.rm = TRUE) {
  min(x, na.rm = na.rm)
}

#' Generic function for maximum
#' @rdname fMean
#' @export
fMax <- function(x, ...) UseMethod("fMax")

#' @rdname fMean
#' @export
fMax.default <- function(x, na.rm = TRUE) {
  max(x, na.rm = na.rm)
}

#' Generic function for scaling
#' @rdname fMean
#' @export
fScale <- function(x, ...) UseMethod("fScale")

#' @rdname fMean
#' @export
fScale.default <- function(x, k = 1) {
  if (!length(k) == 1 & length(k) != length(x)) stop("`k` must be atomic, or with the same length as `x`.", call. = FALSE)
  x * k
}
