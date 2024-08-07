x <- data.frame(N = c(100, 50, 10, 5), SumInsured = c(100 * 10, 50 * 20, 10 * 30, 5 * 50), Prem = c(10, 10, 10, 5))
rp <- RiskProfile(x, TSI = "SumInsured", Premium = "Prem")

test_that("Scale profile",{

  res1 <- fScale(rp, 3)
  exp1 <- RiskProfile(x * 3, TSI = "SumInsured", Premium = "Prem")

  expect_equal(res1, exp1, tolerance = .0001)
})
