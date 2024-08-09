#' Read MetaRisk By-Occurrence detailed data
#' @export
mr_read_OccurrenceTable <- function(file, ...) {
  if (grepl("sqlite$", basename(file), ignore.case = TRUE)) {
    stop("sqlite results to be implemented")
  } else {
    file_def <- gsub("Details.Data.", "Details.Definitions.", file)
    raw_data <- data.table::fread(file)
    raw_def <- data.table::fread(file_def)
    res <- raw_data[raw_def[, .(Id, Component = gsub(" ", "_", Component))], on = "Id"][, Id := NULL] |>
      data.table::dcast(... ~ Component, value.var = c("Value"))
  }

  OccurrenceTable(res, Realization = "Realization", OccurrenceId = "OccurrenceId", EventId = "EventId")
}
