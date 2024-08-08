Incurred <- data.table::data.table(
  check.names = FALSE,
   Year = c(2010L,2011L,2012L,2013L,2014L,2015L,2016L,2017L,2018L,2019L),
   `12` = c(3995.71,3968.04,4217.01,4374.24,4499.68,4530.24,4572.63,4680.56,4696.68,4945.89),
   `24` = c(4635.02,4682.28,5060.42,5205.34,5309.62,5300.38,5304.25,5523.06,5495.11,NA),
   `36` = c(4866.78,4963.22,5364.04,5517.67,5628.2,5565.4,5569.47,5854.44,NA,NA),
   `48` = c(4964.1,5062.49,5508.87,5661.12,5785.79,5715.66,5714.27,NA,NA,NA),
   `60` = c(5013.74,5113.11,5558.45,5740.38,5849.43,5772.82,NA,NA,NA,NA),
   `72` = c(5038.82,5138.67,5586.24,5780.56,5878.68,NA,NA,NA,NA,NA),
   `84` = c(5058.97, 5154.09, 5608.59, 5803.68, NA, NA, NA, NA, NA, NA),
   `96` = c(5074.14, 5169.56, 5625.41, NA, NA, NA, NA, NA, NA, NA),
  `108` = c(5084.29, 5179.89, NA, NA, NA, NA, NA, NA, NA, NA),
  `120` = c(5089.38, NA, NA, NA, NA, NA, NA, NA, NA, NA)
)

IncurredLong <- Incurred |>
  data.table::melt(id.var = 1L, variable.name = "Dev", variable.factor = FALSE, value.name = "Incurred")

TRI <- list(Incurred = Incurred, IncurredLong = IncurredLong)

usethis::use_data(TRI, overwrite = TRUE)
