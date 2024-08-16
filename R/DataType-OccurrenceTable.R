#' Occurrence Table
#' @export
#' @examples
#' set.seed(123)
#' DT <- data.table::CJ(Year = 1:100000, OccId = 1:2, EventId = NA)[, `:=`(x = runif(200000), y = runif(200000))]
#' OccTbl <- OccurrenceTable(DT, "Year", "OccId", "EventId")
#' OEPTbl <- EP(OccTbl, "OEP")
#' fMean(OccTbl)
#' fMean(OccTbl, "OEP")
#' fMean(OEPTbl)
#' fSD(OccTbl)
#' fPerc(OccTbl, c(1.000291, 1.550170, 1.859187))
#' fVaR(OccTbl, c(.5, .9, .99), "AEP")
#' fVaR(OccTbl, c(.5, .9, .99), "OEP")
#' fTVaR(OccTbl, c(.5, .9, .99))
#' fMax(OccTbl)
OccurrenceTable <- function(x, Realization = "Realization", OccurrenceId = "OccurrenceId", EventId = "EventId", Value = NULL) {
  x <- data.table::as.data.table(x)

  # fill NA with 0
  Value <- substitute(Value)
  if (is.null(Value)) {
    Value <- setdiff(x[, names(.SD), .SDcols = is.numeric], c(Realization, OccurrenceId, EventId))
  } else {
    Value <- eval(substitute(x[, names(.SD), .SD = .v], env = list(.v = Value)))
  }

  x[, (Value) := lapply(.SD, data.table::nafill, fill = 0), .SDcols = Value]

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

  res <- complete_Realization(x)
  data.table::setkeyv(res, c("Realization", "OccurrenceId", "EventId"))

  structure(
    res[, c("Realization", "OccurrenceId", "EventId", Value), with = FALSE],
    class = c("OccurrenceTable", class(res))
  )
}

RealizationTable <- function(x, Realization = "Realization", Value = NULL) {
  x <- data.table::as.data.table(x)

  # fill NA with 0
  if (is.null(Value)) {
    if (data.table::haskey(x)) {
      keys <- data.table::key(x)
      Value <- setdiff(x[, names(.SD), .SDcols = is.numeric], keys)
    } else {
      Value <- setdiff(x[, names(.SD), .SDcols = is.numeric], c(Realization, "Realization", "OccurrenceId", "EventId"))
    }
  }

  # names/keys
  data.table::setnames(x, Realization, "Realization")
  x[, `:=`(Realization = as.integer(Realization))]

  res <- complete_Realization(x)
  data.table::setkeyv(res, "Realization")

  structure(
    res[, c("Realization", Value), with = FALSE],
    class = c("RealizationTable", class(res))
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

  RealizationTable(res)
}

#' @export
fPerc.OccurrenceTable <- function(x, q, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  fPerc.RealizationTable(DT, q)
}

#' @export
fPerc.RealizationTable <- function(x, q) {
  x <- data.table::as.data.table(x)
  x[, c(list(quantile = q), lapply(.SD, fPerc.default, q = q)), .SDcols = get_num_cols(x)]
}

#' @export
fVaR.OccurrenceTable <- function(x, prob, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  fVaR.RealizationTable(DT, prob)
}

#' @export
fVaR.RealizationTable <- function(x, prob) {
  x <- data.table::as.data.table(x)
  x[, c(list(prob = prob), lapply(.SD, fVaR.default, prob = prob)), .SDcols = get_num_cols(x)]
}

#' @export
fTVaR.OccurrenceTable <- function(x, prob, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  fTVaR.RealizationTable(DT, prob)
}

#' @export
fTVaR.RealizationTable <- function(x, prob) {
  x <- data.table::as.data.table(x)
  x[, c(list(prob = prob), lapply(.SD, fTVaR.default, prob = prob)), .SDcols = get_num_cols(x)]
}


#' @export
fMean.OccurrenceTable <- function(x, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  fMean.RealizationTable(DT)
}

#' @export
fMean.RealizationTable <- function(x) {
  x <- data.table::as.data.table(x)
  x[, lapply(.SD, fMean.default), .SDcols = get_num_cols(x)]
}

#' @export
fSD.OccurrenceTable <- function(x, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  fSD.RealizationTable(DT)
}

#' @export
fSD.RealizationTable <- function(x) {
  x <- data.table::as.data.table(x)
  x[, lapply(.SD, fSD.default), .SDcols = get_num_cols(x)]
}

#' @export
fMin.OccurrenceTable <- function(x, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  fMin.RealizationTable(DT)
}

#' @export
fMin.RealizationTable <- function(x) {
  x <- data.table::as.data.table(x)
  x[, lapply(.SD, fMin.default), .SDcols = get_num_cols(x)]
}


#' @export
fMax.OccurrenceTable <- function(x, type = c("AEP", "OEP")) {
  DT <- EP(x, type)
  fMax.RealizationTable(DT)
}

#' @export
fMax.RealizationTable <- function(x) {
  x <- data.table::as.data.table(x)
  x[, lapply(.SD, fMax.default), .SDcols = get_num_cols(x)]
}

#' @export
print.OccurrenceTable <- function(x) {
  cat("<OccurrenceTable>\n")
  data.table:::print.data.table(x, row.names = TRUE, print.keys = FALSE, class = FALSE)
  invisible(x)
}

#' @export
print.RealizationTable <- function(x) {
  cat("<RealizationTable>\n")
  data.table:::print.data.table(x, row.names = TRUE, print.keys = FALSE, class = FALSE)
  invisible(x)
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

# Internal -----------------

get_num_cols <- function(x) {
  keys <- data.table::key(x)
  setdiff(x[, names(.SD), .SDcols = is.numeric], keys)
}

complete_Realization <- function(x) {
  # max Realization
  log_maxRealization <- ceiling(log10(x[, max(Realization)]))
  maxRealization <- 10 ^ log_maxRealization

  if (maxRealization != x[, max(Realization)]) {
    fullRealization <- data.table::data.table(Realization = seq_len(Realization), key = "Realization")
    res <- x[fullRealization]
  } else {
    res <- x
  }

  res
}
