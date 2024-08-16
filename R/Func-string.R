#' Parse a character vector to numeric
#' @examples
#' stri_numeric_parse(c("$123,456.789", "N/A", "3.1415926525K", "99.95%", "Inf", "inf"))
#' @export
stri_numeric_parse <- function(x, na = c("", "NA", "N/A")) {
  pos_NA <- x %in% na
  pos_Inf <- x %in% c("Inf", "inf", "unlimited")
  pos_NegInf <- x %in% c("-Inf", "-inf")

  res <- stringi::stri_replace_first_regex(x, "^\\D*", "") |>
    stringi::stri_replace_all_fixed(c("%", "K", "M", "B", "T", ","), c("e-2", "e3", "e6", "e9", "e12", ""), vectorize_all = FALSE, case_insensitive = TRUE)

  res <- as.numeric(res)
  res[pos_NA] <- NA_real_
  res[pos_Inf] <- Inf
  res[pos_NegInf] <- -Inf
  res
}

#' Parse a character vector to Date
#' @export
#' @examples
#' x <- c("2021-01-03", "12-31-2019", "Jan 2 2022", "2023/1/1")
#' stri_date_parse(x)
stri_date_parse <- function(x) {
  if (!requireNamespace("anytime", quietly = TRUE)) {
    stop("Package \"anytime\" must be installed to use this function.", call. = FALSE)
  }
  x <- trimws(x)
  res <- data.table::fifelse(
    grepl("\\d{5}", x),
    as.Date(x, origin = "1899-12-30"),
    anytime::anydate(x)
  )

  res
}

#' Truncate a string to maximum width
#'
#' Code borrowed and modified from `stringr` package
#' @param x Input vector. Either a character vector, or something coercible to one.
#' @param width Maximum width of string.
#' @param side,ellipsis Location and content of ellipsis that indicates content has been removed.
#' @export
#' @examples
#' x <- "This string is moderately long"
#' rbind(
#'   stri_trunc(x, 20, "right"),
#'   stri_trunc(x, 20, "left"),
#'   stri_trunc(x, 20, "center")
#' )
stri_trunc <- function (x, width, side = c("right", "left", "center"), ellipsis = "...") {
  side <- match.arg(side)
  # if (!is.integer(width)) stop("`width` must be an integer", call. = FALSE)
  # if (width > 0) stop("`width` must be a positive integer", call. = FALSE)
  too_long <- !is.na(x) & stringi::stri_length(x) > width
  width... <- width - stringi::stri_length(ellipsis)
  if (width... < 0) stop("`width` is shorter than `ellipsis`", .call = FALSE)
  x[too_long] <- switch(
    side,
    right = stringi::stri_c(stringi::stri_sub(x[too_long], 1, width...), ellipsis),
    left = stringi::stri_c(ellipsis, stringi::stri_sub(x[too_long], -width..., -1)),
    center = stringi::stri_c(stringi::stri_sub(x[too_long], 1, ceiling(width.../2)), ellipsis, stringi::stri_sub(x[too_long], -floor(width.../2), -1))
  )
  x
}

#' In addition to trimming, `stri_squish` also reduces repeated whitespace inside a string.
#' @export
#' @rdname stri_trunc
stri_squish <- function(x) {
  stringi::stri_trim_both(stringi::stri_replace_all_regex(x, "\\s+", " "))
}

#' Cross-join character vectors and paste
#' @param ... One or more character vectors, as in `paste` function
#' @param sep A character string to separate the terms
#' @export
#' @examples
#' stri_cj(c("SI", "Prem"), c("Gross", "SP", "Net"), c("Actual", "AsIf"), sep = "..")

stri_cj <- function(..., sep = "_") {
  x <- data.table::CJ(..., sorted = FALSE)
  res <- x[, do.call(paste, c(.SD, sep = sep))]
  res
}

#' Extract Chinese characters from strings
#' @export
#' @examples
#' x <- c("Sunflower Insurance (葵花保险) （新型）", "Sugarbeet Insurance（甜菜保险）", "Corn Full Cost Insurance (Irrigated Land)（水地玉米完全成本保险）", "Corn Full Cost Insurance (Dry Land)（旱地玉米完全成本保险）")
#' stri_extract_Chinese(x)
#' stri_extract_Chinese(x, simplify = FALSE)

stri_extract_Chinese <- function(x, sep = "_", simplify = TRUE) {
  pattern <- "(\\p{Han}){1,}"
  x1 <- stringi::stri_extract_all_regex(x, pattern)
  if (simplify == TRUE) {
    res <- stringi::stri_c_list(x1, sep = sep)
  } else {
    res <- x1
  }

  res
}

#' Approximate string mapping
#' @export
#' @examples
#' x1 <- c("水稻种植保险\\nRice Insurance", "水地玉米种植保险\\nIrrigated Land Corn Insurance", "旱地玉米种植保险\\nDryland Corn Insurance", "水地小麦种植保险\\nIrrigated Land Wheat Insurance ", "旱地小麦种植保险\\nDryland Wheat Insurance", "水地马铃薯保险\\nIrrigated Potato Insurance ", "旱地马铃薯保险\\nDryland Potato Insurance", "油菜种植保险\\nRape Insurance")
#' x2 <- c("Irrigated Land Corn Insurance (水地玉米种植保险)", "Rice Insurance (水稻种植保险)", "Irrigated Land Wheat Insurance（水地小麦种植保险）", "Rape Insurance （油菜种植保险)", "Dryland Potato Insurance (旱地马铃薯保险)", "Dryland Wheat Insurance（旱地小麦种植保险）", "Irrigated Potato Insurance (水地马铃薯保险)", "Dryland Corn Insurance (旱地玉米种植保险)")
#' data.frame(Orig = x1, Mapped = stri_amap(x1, x2))
stri_amap <- function(x, y) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package \"stringdist\" must be installed to use this function.", call. = FALSE)
  }

  res <- y[stringdist::amatch(x, y, maxDist = Inf)]
  res
}

#' Convert full-width characters into half-width
#' @export
#' @examples
#' x <- c("(Irrigated Land Corn InsuranceABC,.?!) （水地玉米种植保险ＡＢＣ，。？！）", "Rice Insurance (水稻种植保险)")
#' stri_full_to_half(x)

stri_full_to_half <- function(x) {

  stri_full_to_half_single <- function(x1) {
    # 全角空格为12288，半角空格为32
    # 其他字符半角(33-126)与全角(65281-65374)的对应关系是：均相差65248
    int1 <- utf8ToInt(x1)
    int1_half <- data.table::fcase(
      int1 > 65280 & int1 < 65375, int1 - 65248L,
      int1 == 12288L,               32L,
      int1 > 0,                    int1
    )

    res <- intToUtf8(int1_half)
    res
  }

  vapply(x, stri_full_to_half_single, character(1L), USE.NAMES = FALSE)
}

#' Wrap words into nicely formatted paragraphs
#'
#' @export
stri_wrap0 <- function (string, width = 80, indent = 0, exdent = 0, whitespace_only = TRUE) {
  if (width <= 0) {
    width <- 1
  }
  out <- stringi::stri_wrap(string, width = width, indent = indent, exdent = exdent, whitespace_only = whitespace_only, simplify = FALSE)
  vapply(out, paste, character(1), collapse = "\n")
}

#' Format numbers
#'
#' @export
#' @examples
#' x <- c(1.23e12, 3.45e7, -6.78e3, Inf, NA)
#' num(x)
#' num(x, "M", digits = 1)
#' num(x, "B", inf.label = "Unlimited", na.label = "")
num <- function(x, scale = c("auto", "C", "K", "M", "B", "P", "pp"), digits = 2, na.label = "N/A", inf.label = "Inf", ...) {
  scale <- match.arg(scale)
  .accuracy <- 1 / 10 ^ digits

  if (scale == "auto") {
    tmp_scale <- cut(abs(x), c(0, 1e3, 1e6, 1e9, Inf), c("C", "K", "M", "B"))
    final_scale_label <- names(which.min(table(tmp_scale)))
    if (is.na(final_scale_label)) final_scale_label <- "C"
    final_scale_factor <- switch(final_scale_label, "C" = 1, "K" = 1e3, "M" = 1e6, "B" = 1e9)
    if (final_scale_label == "C") final_scale_label <- ""

    res <- comma(x, digits, final_scale_factor, final_scale_label)

  } else {
    final_scale_factor <- switch(scale, "C" = 1, "K" = 1e3, "M" = 1e6, "B" = 1e9, "P" = .01, "pp" = .01)
    final_scale_label <- switch(scale, "C" = "", "K" = "K", "M" = "M", "B" = "B", "P" = "%", "pp" = " p.p.")

    res <- comma(x, digits, final_scale_factor, final_scale_label)
  }

  res <- ifelse(is.na(x), na.label, res)
  res <- ifelse(is.infinite(x), inf.label, res)

  return(res)
}


#' @export
#'@rdname num
num_format <- function(scale = c("auto", "C", "K", "M", "B", "P", "pp"), digits = 0, ...) {
  scale <- match.arg(scale)
  f <- function(x) num(x, scale, digits, ...)
  f
}

comma <- function(x, digits = 2, scale_factor = 1, scale_label = "") {
  x1 <- round(x / scale_factor, digits = digits)
  c1 <- format(x1, nsmall = digits, big.mark = ",", big.interval = 3, scientific = FALSE, trim = TRUE)
  paste0(c1, scale_label)
}
