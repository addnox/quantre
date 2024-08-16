#' Create a ReportOutput object
#' @examples
#' set.seed(123)
#' DT <- data.table::data.table(Year = 1:10, x = runif(10, max = 2), y = runif(10, max = 10))
#' ReportOutput(DT, Realization = "Year", Loss = "y") ## note that x is deleted, and y is kept as Loss
#'
ReportOutput <- function(x, ...) {
  x <- data.table::as.data.table(x)

  cols <- c("Realization", "Loss", "Premium", "Commission", "LossParticipation", "ProfitCommission", "Brokerage", "Expense")

  x |>
    field_map(...) |>
    field_check(required_field = cols[1:2]) |>
    field_fill(cols[-(1:2)]) |>
    data.table::setcolorder(cols)

  res <- RealizationTable(x, Value = cols[-1L])

  structure(
    res,
    class = c("ReportOutput", class(res))
  )
}

#' @export
print.ReportOutput <- function(x) {
  cat("<ReportOutput>\n")
  format_col.numeric <<- function(x, ...) prettyNum(round(x, 0), big.mark = ",", scientific = FALSE)
  data.table:::print.data.table(x, row.names = FALSE, print.keys = FALSE, class = TRUE)
  format_col.numeric <<- NULL
  invisible(x)
}

