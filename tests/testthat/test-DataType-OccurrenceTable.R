set.seed(12345)
nSim <- 100000
loss <- data.table::CJ(Year = 1:nSim, OccId = 1:2)[, `:=`(x = runif(2 * nSim, max = 10e6), y = runif(2 * nSim, max = 5e6))] |>
  OccurrenceTable("Year", "OccId", NULL)

expect_near <- function(x, y, rel = .0025) {
  expect_true(all(abs(x / y - 1) <= rel))
}

test_that("OccTable: AEP",{
  expect_near(unlist(fMean(loss)), c(10005800.60, 4995298.18))
  expect_near(unlist(fSD(loss)), c(4081368.733,	2043666.521), rel = .005)
  expect_near(unlist(fMax(loss)), c(19979346.7773273, 9973295.61534236))

  AEP_VaR <- fVaR(loss, c(.9, .95, .99))
  expect_near(AEP_VaR[, 2], c(15526069.97, 16818888.78, 18614143.13))
  expect_near(AEP_VaR[, 3], c(7760893.36, 8420546.21, 9284405.15))

  AEP_TVaR <- fTVaR(loss, c(.9, .95, .99))
  expect_near(AEP_TVaR[, 2], c(17008788.48, 17884799.33, 19071745.69))
  expect_near(AEP_TVaR[, 3], c(8504909.67, 8940168.795, 9525405.00))
})

test_that("OccTable: OEP",{
  expect_near(unlist(fMean(loss, "OEP")), c(6668481.47, 3328692.14))
  expect_near(unlist(fSD(loss, "OEP")), c(2358014.19,	1179520.18), rel = .005)
  expect_near(unlist(fMax(loss, "OEP")), c(9999988.42, 4999981.93))

  OEP_VaR <- fVaR(loss, c(.9, .95, .99), "OEP")
  expect_near(OEP_VaR[, 2], c(9485601.54, 9743960.23, 9949738.25))
  expect_near(OEP_VaR[, 3], c(4741229.44, 4874246.03, 4974794.67))

  OEP_TVaR <- fTVaR(loss, c(.9, .95, .99), "OEP")
  expect_near(OEP_TVaR[, 2], c(9744836.695, 9872741.13, 9974254.21))
  expect_near(OEP_TVaR[, 3], c(4872646.23, 4937231.68, 4987490.13))
})
