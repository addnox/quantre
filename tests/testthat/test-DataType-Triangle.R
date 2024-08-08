loss <- Triangle(TRI$Incurred)

test_that("Latest Diagonal is correct", {
  res <- tri_LatestDiagonal(loss)
  exp <- c(5089.38, 5179.89, 5625.41, 5803.68, 5878.68, 5772.82, 5714.27, 5854.44, 5495.11, 4945.89)

  expect_equal(unname(res), exp)
})


test_that("Age-to-Age factor is correct", {
  res <- tri_LinkRatio(loss)

  expect_equal(unname(res[6, 1:4]), c(1.17, 1.05, 1.027, 1.01), tolerance = .001)
  expect_equal(unname(res[1:2, 1]), c(1.16, 1.18), tolerance = .001)
})

test_that("LinkRatioAverage is correct", {
  expect_equal(unname(tri_LinkRatioAverage(loss, "simple", 5)), c(1.1720, 1.0560, 1.0268, 1.0108, 1.0054, 1.0038, 1.0030, 1.0020, 1.0010), tolerance = .00005)
  expect_equal(unname(tri_LinkRatioAverage(loss, "weighted", 3)), c(1.1701, 1.0534, 1.0270, 1.0117, 1.0057, 1.0037, 1.0030, 1.0020, 1.0010), tolerance = .00005)
  expect_equal(unname(tri_LinkRatioAverage(loss, "weighted", 5, exclude = 1)), c(1.1759, 1.0559, 1.0257, 1.0108, 1.0055, 1.0037, 1.0030, 1.0020, 1), tolerance = .00005)
  expect_equal(unname(tri_LinkRatioAverage(loss, "geometric", 4)), c(1.1700, 1.0550, 1.0267, 1.0110, 1.0055, 1.0037, 1.0030, 1.0020, 1.0010), tolerance = .00005)
})

test_that("Report Percent is correct", {
  LinkRatio <- tri_LinkRatioAverage(loss, "simple")
  res1 <- tri_ReportedPercent(LinkRatio)
  exp1 <- c(.7651, .9003, .9510, .9746, .9850, .9903, .9940, .9970, .9990, 1)
  expect_equal(unname(res1), exp1, tolerance = .00005)

  res2 <- tri_ReportedPercent(LinkRatio, 1.1)
  exp2 <- exp1 / 1.1
  expect_equal(unname(res2), exp2, tolerance = .00005)
})
