% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Func-Data-wrangle.R
\name{dt_separate}
\alias{dt_separate}
\title{By-ref equivalent of \code{tidyr::separate}}
\usage{
dt_separate(
  dt,
  col,
  into = NULL,
  sep = "[^[:alnum:]]+",
  remove = TRUE,
  extra = c("complete", "merge", "drop")
)
}
\arguments{
\item{dt}{A \code{data.table} or \code{data.frame}.}

\item{col}{Column name to be split.}

\item{into}{Names of new variables to create as character vector.}

\item{sep}{Separator between columns.}

\item{remove}{If \code{TRUE}, remove input column from output data frame.}

\item{extra}{Controls when there are too many pieces after split.}
}
\description{
By-ref equivalent of \code{tidyr::separate}
}
\examples{
df <- data.table(x = c("A_B_C", "D_E_F"), y = 1:2, z = 3:4)
dt_separate(copy(df), col = "x")[]
dt_separate(copy(df), col = "x", into = paste0("v", 1:2))[]
dt_separate(copy(df), col = "x", into = paste0("v", 1:2), extra = "merge")[]
dt_separate(copy(df), col = "x", into = paste0("v", 1:2), extra = "drop")[]
dt_separate(copy(df), col = "x", into = paste0("v", 1:4))[]
}
