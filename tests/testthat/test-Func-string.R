test_that("parse numbers", {
  expect_equal(
    stri_numeric_parse(c("$123,456.789", "N/A", "3.1415926525K", "99.95%", "Inf", "inf", NA_character_)),
    c(123456.789, NA, 3141.5926525, .9995, Inf, Inf, NA)
  )
})

test_that("parse date", {
  expect_equal(
    stri_date_parse(c("2021-01-03", "12-31-2019", "Jan 2 2022", "2023/1/1", NA)),
    anytime::anydate(c(20210103, 20191231, 20220102, 20230101, NA), tz = "")
  )
})

test_that("format number", {
  x <- c(1.23e12, 3.45e7, -6.78e5, Inf, NA)
  expect_equal(num(x, "M", digits = 1), c("1,230,000.0M", "34.5M", "-0.7M", "Inf", "N/A"))
  expect_equal(num(x, "B", inf.label = "Unlimited", na.label = ""), c("1,230.00B", "0.03B", "0.00B", "Unlimited", ""))
})
