test_that("range is correctly parsed ", {
  expect_equal(xlrange("A1"), readxl::cell_limits(c(1, 1), c(1, 1)))
  expect_equal(xlrange("A1:B3", ws = "Data"), readxl::cell_limits(c(1, 1), c(3, 2), sheet = "Data"))
  expect_equal(xlrange(rows = 2:10), readxl::cell_limits(c(2, NA), c(10, NA)))
  expect_equal(xlrange(rows = c(3, NA)), readxl::cell_limits(c(3, NA), c(NA, NA)))
  expect_equal(xlrange(cols = "D:F"), readxl::cell_limits(c(NA, 4), c(NA, 6)))
  expect_equal(xlrange(rows = 2:5, cols = c(9, NA)), readxl::cell_limits(c(2, 9), c(5, NA)))
})

file <- system.file("extdata", "ExcelData_Example.xlsx", package = "quantre")
block1 <- rbind(
  data.table::data.table(Band = c(0, 100, 200), V2 = c(100, 200, 300), Type = "A", Var = "X", value = 1:3),
  data.table::data.table(Band = c(0, 100, 200), V2 = c(100, 200, 300), Type = "A", Var = "Y", value = 4:6),
  data.table::data.table(Band = c(0, 100, 200), V2 = c(100, 200, 300), Type = "B", Var = "X", value = 7:9),
  data.table::data.table(Band = c(0, 100, 200), V2 = c(100, 200, 300), Type = "C", Var = "X", value = 10:12)
)

block2 <- block1[, value := 13:24]
block3 <- block1[, value := 25:36]
block4 <- block1[, value := 37:48]

test_that("tidy data: one block", {
  raw <- readxl_raw(file, "BlockData", rows = 3:7, cols = "B:G")
  res <- tidy_table(raw, long_cols = 1:2, header_rows = 1:2, wide_names = c("Type", "Var"))
  res[, c("Band", "V2", "value") := lapply(.SD, as.numeric), .SDcols = c("Band", "V2", "value")]

  exp <- rbind(
    data.table::data.table(Band = c(0, 100, 200), V2 = c(100, 200, 300), Type = "A", Var = "X", value = 1:3),
    data.table::data.table(Band = c(0, 100, 200), V2 = c(100, 200, 300), Type = "A", Var = "Y", value = 4:6),
    data.table::data.table(Band = c(0, 100, 200), V2 = c(100, 200, 300), Type = "B", Var = "X", value = 7:9),
    data.table::data.table(Band = c(0, 100, 200), V2 = c(100, 200, 300), Type = "C", Var = "X", value = 10:12)
  )

  expect_equal(res, exp)
})

