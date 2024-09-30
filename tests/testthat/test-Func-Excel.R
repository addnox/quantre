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
  data.table::data.table(Lbound = c(0, 100, 200), Ubound = c(100, 200, 300), Type = "A", Var = "X", value = 1:3),
  data.table::data.table(Lbound = c(0, 100, 200), Ubound = c(100, 200, 300), Type = "A", Var = "Y", value = 4:6),
  data.table::data.table(Lbound = c(0, 100, 200), Ubound = c(100, 200, 300), Type = "B", Var = "X", value = 7:9),
  data.table::data.table(Lbound = c(0, 100, 200), Ubound = c(100, 200, 300), Type = "C", Var = "X", value = 10:12)
)

block2 <- data.table::copy(block1)[, value := 13:24]
block3 <- data.table::copy(block1)[, value := 25:36]
block4 <- data.table::copy(block1)[, value := 37:48]

test_that("tidy data: one block", {
  raw <- readxl_raw(file, "BlockData", rows = 3:7, cols = "B:G")
  res <- tidy_table(raw, long_cols = 1:2, header_rows = 1:2, wide_names = c("Type", "Var")) |>
    data.table::setnames(1:2, c("Lbound", "Ubound"))
  res[, c("Lbound", "Ubound", "value") := lapply(.SD, as.numeric), .SDcols = c("Lbound", "Ubound", "value")]

  exp <- block1

  expect_equal(res, exp)
})

test_that("tidy data: vertical blocks", {
  raw <- readxl_raw(file, "BlockData", rows = 2:14, cols = "A:G")
  res <- raw |>
    tidy_hsplit() |>
    lapply(function(x) x[, `:=`(Year = vpick(V1, "\\d{4}", fill_direction = "down"), Class = vpick(V2, "Class", fill_direction = "down"))][, `:=`(V1 = NULL)]) |>
    lapply(tidy_table, long_cols = 1:2, header_rows = 1:3, wide_names = c(NA, "Type", "Var"), cols_keepnames = c("Year", "Class")) |>
    data.table::rbindlist() |>
    data.table::setnames(1:2, c("Lbound", "Ubound"))

  res[, c("Lbound", "Ubound", "value") := lapply(.SD, as.numeric), .SDcols = c("Lbound", "Ubound", "value")]

  exp <- rbind(
    "2019" = block1,
    "2020" = block3,
    idcol = "Year"
  )[, .(Lbound, Ubound, Year, Class = "Class1", Type, Var, value)]

  expect_equal(res, exp)
})


test_that("tidy data: horizontal blocks", {
  raw <- readxl_raw(file, "BlockData", rows = 2:7, cols = "B:N")
  res <- raw |>
    tidy_vsplit() |>
    lapply(function(x) data.table::setnames(x, makenm_numbered)) |>
    lapply(function(x) x[, `:=`(Class = vpick(V1, "Class", fill_direction = "down"))]) |>
    lapply(tidy_table, long_cols = 1:2, header_rows = 1:3, wide_names = c(NA, "Type", "Var"), cols_keepnames = c("Class")) |>
    lapply(function(x) data.table::setnames(x, 1:2, c("Lbound", "Ubound"))) |>
    data.table::rbindlist()

  res[, c("Lbound", "Ubound", "value") := lapply(.SD, as.numeric), .SDcols = c("Lbound", "Ubound", "value")]

  exp <- rbind(
    "Class1" = block1,
    "Class2" = block2,
    idcol = "Class"
  )[, .(Lbound, Ubound, Class, Type, Var, value)]

  expect_equal(res, exp)
})

test_that("Read in data.frame", {
  exp_df <- data.table::data.table(
    Char = LETTERS[1:3],
    Int = as.numeric(1:3),
    Num = c(3.1, 3.14, 3.141),
    Date = as.POSIXct(as.Date(c("2019/01/01", "2020/12/31", "2021/06/30")))
  )

  ws <- "DataType"
  ## df
  expect_equal(readxl(file, ws, "B2:E5"), exp_df)
  ## df, diff spec
  expect_equal(readxl(file, ws, rows = 2:5, cols = "B:E"), exp_df)
  ## dfc
  expect_equal(readxl_dfc(file, ws, list(c("B2:C5"), "E2:E5"))[, `:=`(Int = as.numeric(Int), Date = as.POSIXct(stri_date_parse(Date)))], exp_df[, c(1, 2, 4)])
})

test_that("Read in list", {
  exp_list <- list(Name = "ABC", Age = "30", Nationality = "China")

  ws <- "DataType"
  expect_equal(readxl_list(file, ws, "G2:H4"), exp_list)
})

test_that("Read in vector", {
  exp_vNum <- 1:5
  exp_vChar <- data.table::last(LETTERS, 4)


  ws <- "DataType"
  ## vertical vector: num
  expect_equal(readxl_vector(file, ws, "J2:J6", "numeric"), exp_vNum)
  ## vertical vector: char
  expect_equal(readxl_vector(file, ws, "L2:L5"), exp_vChar)
  ## horizontal vector: char
  expect_equal(readxl_vector(file, ws, "N2:Q2"), exp_vChar)
})

test_that("Read in entire sheet", {
  exp1 <- data.table(X = LETTERS[1:3], Y = 1:3, Z = 4:6)
  expect_equal(readxl(file, "EntireSheet"), exp1)

  exp2 <- rbind(data.table(V1 = "X", V2 = "Y", V3 = "Z"), exp1, use.names = FALSE)
  expect_equal(readxl_raw(file, "EntireSheet"), exp2)
})
