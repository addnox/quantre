#' Parse a character vector to numeric
#' @examples
#' parse_numeric(c("$123,456.789", "N/A", "3.1415926525K", "99.95%", "Inf", "inf"))
#' @export
parse_numeric <- function(x, na = c("", "NA", "N/A")) {
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
#' @examples
#' x <- c("2021-01-03", "12-31-2019", "Jan 2 2022", "2023/1/1")
#' parse_date(x)
parse_date <- function(x) {
  x <- trimws(x)
  res <- data.table::fifelse(
    grepl("\\d{5}", x),
    as.Date(x, origin = "1899-12-30"),
    anytime::anydate(x)
  )

  res
}
