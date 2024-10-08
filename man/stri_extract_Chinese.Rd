% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Func-string.R
\name{stri_extract_Chinese}
\alias{stri_extract_Chinese}
\title{Extract Chinese characters from strings}
\usage{
stri_extract_Chinese(x, sep = "_", simplify = TRUE)
}
\description{
Extract Chinese characters from strings
}
\examples{
x <- c("Sunflower Insurance (葵花保险) （新型）", "Sugarbeet Insurance（甜菜保险）", "Corn Full Cost Insurance (Irrigated Land)（水地玉米完全成本保险）", "Corn Full Cost Insurance (Dry Land)（旱地玉米完全成本保险）")
stri_extract_Chinese(x)
stri_extract_Chinese(x, simplify = FALSE)
}
