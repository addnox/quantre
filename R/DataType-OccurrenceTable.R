#' Occurrence Table
#' @export
#' @examples
#' set.seed(123)
#' DT <- data.table::CJ(Year = 1:100000, OccId = 1:2, EventId = NA)[, `:=`(x = runif(200000), y = runif(200000))]
#' YOT <- OccurrenceTable(DT, "Year", "OccId", "EventId")
#' fMean(YOT)
#' fMean(YOT, "OEP")
#' fSD(YOT)
#' fPerc(YOT, c(1.000291, 1.550170, 1.859187))
#' fVaR(YOT, c(.5, .9, .99), "AEP")
#' fVaR(YOT, c(.5, .9, .99), "OEP")
#' fTVaR(YOT, c(.5, .9, .99))
#' fMax(YOT)
OccurrenceTable <- function(x, Realization = "Realization", OccurrenceId = "OccurrenceId", EventId = "EventId") {
  x <- data.table::as.data.table(x)

  # fill NA with 0
  num_cols <- get_num_cols(x, c(Realization, OccurrenceId, EventId))
  x[, (num_cols) := lapply(.SD, data.table::nafill, fill = 0), .SDcols = num_cols]

  missing_cols <- setdiff(c(Realization, OccurrenceId, EventId), names(x))
  if (length(missing_cols) > 0) stop("Cannot find below columns: ", paste(missing_cols, collapse = ","))

  # Missing OccId and EventId
  if (is.null(OccurrenceId)) {
    x[, OccId := NA_integer_]
    OccurrenceId <- "OccurrenceId"
  }

  if (is.null(EventId)) {
    x[, EventId := NA_integer_]
    EventId <- "EventId"
  }

  # names/keys
  data.table::setnames(x, c(Realization, OccurrenceId, EventId), c("Realization", "OccurrenceId", "EventId"))
  x[, c("Realization", "OccurrenceId", "EventId") := lapply(.SD, as.integer), .SDcols = c("Realization", "OccurrenceId", "EventId")]
  data.table::setkeyv(x, c("Realization", "OccurrenceId", "EventId"))

  # max Realization
  log_maxRealization <- ceiling(log10(x[, max(Realization)]))
  maxRealization <- 10 ^ log_maxRealization

  if (maxRealization != x[, max(Realization)]) {
    fullRealization <- data.table::data.table(Realization = seq_len(Realization), key = "Realization")
    res <- x[fullRealization]
  } else {
    res <- x
  }

  data.table::setcolorder(res, c("Realization", "OccurrenceId", "EventId"))

  structure(
    res,
    class = c("OccurrenceTable", class(res))
  )
}

#' @export
EP <- function(x, type = c("AEP", "OEP")) {
  type <- match.arg(type)
  num_cols <- get_num_cols(x)

  if (type == "AEP") {
    res <- x[, lapply(.SD, sum), by = "Realization", .SDcols = num_cols]
  } else if (type == "OEP") {
    res <- x[, lapply(.SD, max), by = "Realization", .SDcols = num_cols]
  }

  res
}

#' @export
fPerc.OccurrenceTable <- function(x, q, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  res <- DT[, c(list(quantile = q), lapply(.SD, fPerc.default, q = q)), .SDcols = get_num_cols(DT)]
  data.table::setDT(res)[]
}

#' @export
fVaR.OccurrenceTable <- function(x, prob, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  res <- DT[, c(list(prob = prob), lapply(.SD, fVaR.default, prob = prob)), .SDcols = get_num_cols(DT)]
  data.table::setDT(res)[]
}

#' @export
fTVaR.OccurrenceTable <- function(x, prob, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  res <- DT[, c(list(prob = prob), lapply(.SD, fTVaR.default, prob = prob)), .SDcols = get_num_cols(DT)]
  data.table::setDT(res)[]
}

#' @export
fMean.OccurrenceTable <- function(x, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  res <- DT[, lapply(.SD, fMean.default), .SDcols = get_num_cols(DT)]
  data.table::setDT(res)[]
}

#' @export
fSD.OccurrenceTable <- function(x, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  res <- DT[, lapply(.SD, fSD.default), .SDcols = get_num_cols(DT)]
  data.table::setDT(res)[]
}

#' @export
fMin.OccurrenceTable <- function(x, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  res <- DT[, lapply(.SD, fMin.default), .SDcols = get_num_cols(DT)]
  data.table::setDT(res)[]
}

#' @export
fMax.OccurrenceTable <- function(x, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  res <- DT[, lapply(.SD, fMax.default), .SDcols = get_num_cols(DT)]
  data.table::setDT(res)[]
}

#' @export
print.OccurrenceTable <- function(x) {
  cat("<OccurrenceTable>\n")
  data.table:::print.data.table(x, row.names = TRUE, print.keys = FALSE, class = FALSE)
  invisible(x)
}

get_num_cols <- function(x, except = c("Realization", "OccurrenceId", "EventId")) {
  setdiff(x[, names(.SD), .SDcols = is.numeric], except)
}

#' Sum across multiple columns
#' @param x An instance of `OccurrenceTable` object
#' @param var Either a numeric vector of positions, or a character vector of names
sum_across_var <- function(x, var = -(1:3)) {
  if (length(var) == 1) {
    res <- x[, .(Realization, OccurrenceId, EventId, Value = x[[var]])]
  } else {
    res <- x[, .(Realization, OccurrenceId, EventId, Value = rowSums(x[, var, with = FALSE]))]
  }
  res
}


