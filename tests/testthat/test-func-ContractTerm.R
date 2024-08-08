test_that("Term: Excess", {
  loss <- c(100, 200, 250, 400)
  expect_equal(ct_Excess(100, 200)(loss), c(0, 0, 50, 100))
  expect_equal(ct_Excess(100, 200, is.FranchiseDed = TRUE)(loss), c(0, 0, 100, 100))
  expect_equal(ct_Excess(100, 200, is.FranchiseLimit = TRUE)(loss), c(0, 0, 50, 0))
  expect_equal(ct_Excess(100, 200, TRUE, TRUE)(loss), c(0, 0, 0, 0))
})

test_that("Term: LPC", {
  loss <- c(.6, .9, 1.2)
  expect_equal(ct_LossParticipation(list(.8, Inf, .2))(loss), c(0, -.02, -.08))
  expect_equal(ct_LossParticipation(data.frame(c(0, .8), c(.8, Inf), c(0, .2)))(loss), c(0, -.02, -.08))
  expect_equal(ct_LossParticipation(data.frame(c(0, .8, 1), c(.8, 1, Inf), c(0, .2, 0)))(loss), c(0, -.02, -.04))
})

test_that("Term: PC", {
  profit <- c(-.2, .05, .2, .4)
  expect_equal(ct_ProfitCommission(.2, .1)(profit), c(0, 0, .02, .06))
  expect_equal(ct_ProfitCommission(data.frame(c(0, .2), c(.2, Inf), c(.1, .5)), .1)(profit), c(0, 0, .01, .02 + .05))
})

SS3 <- data.frame(
Lower = c(0,0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,
               0.54,0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,
               0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96),
Upper = c(0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,0.54,
               0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,
               0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,Inf),
Commission = c(0.67,0.66,0.65,0.64,0.63,0.62,0.61,0.6,0.59,0.58,0.57,0.56,0.55,0.54,0.53,0.52,0.51,0.5,0.49,0.48,0.47,0.46,0.45,0.44,0.43,
               0.42,0.41,0.4,0.39,0.38,0.37,0.36,0.35,0.34,0.33,0.32,0.31,0.3,0.29,0.28,0.27,0.26,0.25,0.24,0.23,0.22,0.21,0.2,0.19,0.18,0.17,0.16,0.15,0.14,
               0.13,0.12,0.11,0.1,0.09,0.08,0.07,0.06,0.05,0.04,0.03,0.02,0.01,0.01)
)

SS2 <- data.frame(
  LR = c(0.3,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,0.4,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,0.54,
                   0.55,0.56,0.57,0.58,0.59,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.8,0.81,0.82,0.83,
                   0.84,0.85,0.86,0.87,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,Inf),
  Commission = c(0.67,0.66,0.65,0.64,0.63,0.62,0.61,0.6,0.59,0.58,0.57,0.56,0.55,0.54,0.53,0.52,0.51,0.5,0.49,0.48,0.47,0.46,0.45,0.44,0.43,
                 0.42,0.41,0.4,0.39,0.38,0.37,0.36,0.35,0.34,0.33,0.32,0.31,0.3,0.29,0.28,0.27,0.26,0.25,0.24,0.23,0.22,0.21,0.2,0.19,0.18,0.17,0.16,0.15,0.14,
                 0.13,0.12,0.11,0.1,0.09,0.08,0.07,0.06,0.05,0.04,0.03,0.02,0.01,0.01)
)

test_that("Term: Commission", {
  loss <- c(0.3, 0.35, 0.345, 0.77, 1.02)
  exp <- c(0.67, 0.62, 0.62, 0.2, 0.01)

  expect_equal(ct_Commission(SS2)(loss), exp)
  expect_equal(ct_Commission(SS3)(loss), exp)
  expect_equal(ct_Commission(.3)(loss), rep(.3, times = length(loss)))
})

test_that("Term: parse reinstatement", {
  df_reins <- function(reins, perc) data.table::data.table(Reinstatement = reins, Percentage = perc)

  exp1 <- df_reins(1:2, c(1.2, 1))
  expect_equal(parse_reinstatement("1@120%, 2@100%"), exp1)
  expect_equal(parse_reinstatement("1@120%;2@100%"), exp1)
  expect_equal(parse_reinstatement(c(1.2, 1)), exp1)
  expect_equal(parse_reinstatement(0), df_reins(1, 0)) ## note this mean 1@0%, rather than Nil reinstatement
  expect_equal(parse_reinstatement("2@120%"), df_reins(1:2, 1.2))
  expect_equal(parse_reinstatement("2@free"), df_reins(1:2, 0))
  expect_equal(parse_reinstatement("uf"), df_reins(Inf, 0))
  expect_equal(parse_reinstatement("nil"), df_reins(0, 1))
  expect_equal(parse_reinstatement("0@100%"), df_reins(0, 1))
})

test_that("Term: parse XL", {
  exp0 <- data.table::data.table(
    Limit = 100e6,
    Deductible = c(200e6, 300e6),
    AAL = Inf,
    AAD = 0,
    Reinstatement = NA_character_,
    Premium = 0
  )

  expect_equal(parse_XL(c("100M xs 200M", "100M xs 300M")), exp0)
  expect_equal(parse_XL(c("100M xs 200M xs 50M", "0.1B xs 300M")), data.table::copy(exp0)[, AAD := c(50e6, 0)])
  expect_equal(
    parse_XL(Limit = 100e6, Deductible = c(200e6, 300e6), AAL = c(50e6, NA), Reinstatement = "2@100%", Premium = c(NA, 15e6)),
    data.table::copy(exp0)[, `:=`(AAL = c(50e6, Inf), Reinstatement = "2@100%", Premium = c(0, 15e6))]
  )
})
