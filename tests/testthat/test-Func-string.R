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
