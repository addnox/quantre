loss <- Triangle(TRI$Incurred)
ata <- tri_LinkRatioAverage(loss, "simple")
premium <- seq(10000, 28000, by = 2000)

test_that("ChainLadder is correct", {
  resCL <- ChainLadder(loss, link_ratio = ata)

  res <- unname(resCL[, "ultimate"])
  exp <- c(5089.38, 5185.08, 5642.3, 5838.57, 5936.2, 5860.78, 5863.22, 6156.35, 6103.54, 6464.02)
  expect_equal(res, exp, tolerance = .01)
})

test_that("B-F is correct", {
  ELoss_BF <- c(5000, 6000, 7000, 8000, 9000, 8000, 8800, 9600, 10400, 11200)
  resBF <- BornhuetterFerguson(loss, ELoss_BF, ata)

  res <- unname(resBF[, "ultimate"])
  exp <- c(5089.38, 5185.89, 5646.36, 5851.48, 5965.88, 5892.88, 5937.82, 6325.23, 6531.83, 7576.31)
  expect_equal(res, exp, tolerance = .01)
})

test_that("CapCod is correct", {
  CapeCod_ELR <- CapeCod_ELR(loss, premium, ata)
  expect_equal(CapeCod_ELR, .31, tolerance = .001)

  resCC <- CapeCod(loss, premium, ata)
  res <- unname(resCC[, "ultimate"])
  exp <- c(5089.38, 5183.61, 5638.41, 5833.33, 5932.78, 5865.93, 5887.63, 6219.52, 6299.05, 6985.69)
  expect_equal(res, exp, tolerance = .01)
})
