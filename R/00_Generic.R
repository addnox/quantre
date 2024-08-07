#' Generic function for mean
#' @export
fMean <- function(x, ...) UseMethod("fMean")

#' @rdname fMean
#' @export
fMean.default <- function(x, na.rm = TRUE) {
  mean(x, na.rm = na.rm)
}

#' Generic function for standard deviation
#' @export
fSD <- function(x, ...) UseMethod("fSD")

#' @rdname fSD
#' @export
fSD.default <- function(x) {
  sd(x, na.rm = na.rm)
}

#' Generic function for percentile (i.e. inverse of Value at Risk)
#' @export
fPerc <- function(x, ...) UseMethod("fPerc")

#' @rdname fPerc
#' @export
fPerc.default <- function(x, q) {
  ecdf(x)(q)
}

#' Generic function for Value at Risk (i.e. quantile)
#' @export
fVaR <- function(x, ...) UseMethod("fVaR")

#' @rdname fVaR
#' @export
fVaR.default <- function(x, prob) {
  quantile(x, prob, names = FALSE, type = 2)
}

#' Generic function for Tail Value at Risk (a.k.a Conditional Tail Expectation, or CTE)
#' @export
fTVaR <- function(x, ...) UseMethod("fTVaR")

#' @rdname fTVaR
#' @export
fTVaR.default <- function(x, prob) {
  vapply(
    prob,
    function(.p) collapse::fmean(x[x >= quantile(x, .p, names = FALSE, type = 2)]),
    double(1L)
  )
}

#' Generic function for scaling
#' @export
fScale <- function(x, ...) UseMethod("fScale")

#' @rdname fScale
#' @export
fScale.default <- function(x, k = 1) {
  if (!length(k) == 1 & length(k) != length(x)) stop("`k` must be atomic, or with the same length as `x`.", call. = FALSE)
  x * k
}

find_method <- function(single_dispatch, method) {
  .f <- get(paste0(single_dispatch, ".", method), envir = parent.frame())
  .f
}
