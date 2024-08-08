RP <- data.table(Lbound = c(0, 100, 200), Ubound = c(100, 200, Inf), N = 10, TSI = c(50, 150, 250) * 10, Premium = c(50, 150, 250) * 10 * .001) |>
  RiskProfile()

MBBEFD <- RiskCurve(b = 0.0218339035107131, g = 1096.98894971097)
PropExpoRating <- ExposureRating(RP, ExpectLossRatio = .45, SubjectPremium = 9, MBBEFD)

test_that("Property Exposure Rating: Mean Loss in Layer",{

  res1 <- fMean(PropExpoRating, Limit = c(50, 200), Deductible = c(50, 100))
  res2 <- fMean(PropExpoRating, c("50 xs 50", "200 xs 100"))
  # correct_output from GC Prop Expo Rating Tool
  exp <- data.table(
    ExpectedLayerLoss = c(0.353446803879989,0.174087164715577),
    ExpectedLayerCount = c(0.0127048410006586,0.00371897701275048),
    ExpectedLayerSeverity = c(27.8198525949019, 46.8104976499506)
  )

  # compare
  expect_equal(res1, exp, tolerance = .0001)
  expect_equal(res2, exp, tolerance = .0001)
})

test_that("Property Exposure Rating: build CDF", {
  ECDF0 <- build_CDF(PropExpoRating, mseq(50, 250, 200))

  res_sev <- ECDF0$LargeSeverity[c(4, 96, 200),]  # row 4, 96 and 200

  correct_sev <- data.table(
    Loss = c(51.2217667795136, 107.394182488543, 247.996275597472),
    CDF = c(0.0356009793813233, 0.749298085304252, 0.981176369979008)
  )

  res_freq <- ECDF0$LargeFrequency
  correct_freq <- 0.0127048410006686

  expect_equal(res_sev, correct_sev, tolerance = .0001)
  expect_equal(res_freq, correct_freq, tolerance = .0001)
})
