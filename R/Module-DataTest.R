#' A container for all data tests
#' @export
#' @examples
#' tester <- DataTest$new()
#' DT <- data.table::as.data.table(mtcars)
#' tester$test_that("cyl is below 20", {DT[cyl > 20]})
#' tester$test_that("cyl cannot be 6", {DT[cyl == 6]}, "cyl2")
#' ## or wrap them all into a function
#' test_f <- function() {
#'   tester <- DataTest$new()
#'   tester$test_that("cyl is below 20", {DT[cyl > 20]})
#'   tester$test_that("cyl cannot be 6", {DT[cyl == 6]}, "cyl2")
#' }
#' test_f()

DataTest <- R6::R6Class(
  "DataTest",
  public = list(
    #' @description
    #' Create a new DataTest object
    #' @param cols_keep A character vector for columns to keep
    initialize = function(cols_keep = NULL) {
      self$keep_cols(cols_keep)
    },
    #' @description
    #' Create a new test
    #' @param desc Human-readble description for the test
    #' @param code Code for testing, can be embraced with curly bracket (i.e. {...})
    #' @param test_name Concise test id, usually for outputing as Excel sheetname.  If left blank, an id like "test-001" will be created automatically
    #' @param cols_forward Columns to bring forward in the result dataset
    #' @param cols_keep Columns in output dataset (for invalid records).  This will add to the `cols_keep` input during object initialization
    test_that = function(desc, code, test_name = NULL, cols_forward = NULL, cols_keep = NULL) {
      code <- substitute(code) ## borrow from test_that
      test_res <- eval(code, parent.frame())
      if (!is.data.frame(test_res)) stop("`code` must produce a `data.frame` object.", call. = FALSE)

      if (is.null(test_name)) test_name <- paste0("test-", stringi::stri_pad_left(length(private$.results) + 1, width = 3, pad = "0"))
      msg_test_result <- "all valid"
      msg_prefix <- "\u2705"
      n_invalid <- nrow(test_res)

      if (n_invalid > 0) {
        msg_test_result <- paste0(n_invalid, " invald rows")
        msg_prefix <- "\u26D4"
      }

      # screen output
      desc_label <- paste0("test that `", desc, "` ") |> stringi::stri_pad_right(100, ".")
      cat(msg_prefix, " ", desc_label, " ", msg_test_result, "\n", sep = "")

      # pick columns
      if (is.null(private$.cols_to_keep) & is.null(cols_keep)) {
        cols_keep <- names(test_res)
      } else {
        cols_keep <- base::intersect(c(private$.cols_to_keep, cols_keep), names(test_res))
      }

      if (!is.null(cols_forward)) {
        cols_forward <- base::intersect(cols_forward, cols_keep)
        cols_keep <- unique(c(cols_forward, cols_keep))
      }

      # output to results (only DT results are recorded)
      private$.results[[test_name]] <- list(name = test_name, desc = desc, data = test_res[, ..cols_keep])

      invisible(self)
    },
    #' @description
    #' Summarise the results for all tests
    #' @param invalid_only Logical, only show invalid tests
    #' @param sorted Logical, whether to reorder the tests by `test_name`
    summary = function(invalid_only = TRUE, sorted = FALSE) {
      toc <- lapply(
        private$.results,
        function(.x) data.table(Test = .x$desc, Sheet = .x$name, InvalidRows = nrow(.x$data))
      ) |> rbindlist()

      if (invalid_only) {
        toc <- toc[InvalidRows > 0]
      }

      if (sorted) {
        toc <- toc[order(Sheet)]
      }

      toc
    },
    #' @description
    #' Output test summary and invalid datasets to Excel
    #' @param file File path for excel output
    #' @param headerStyle Not implemented yet
    write_to_excel = function(file, headerStyle = FALSE) {
      toc <- self$summary()

      failed_tests <- private$.results |>
        Filter(f = function(x) !is.null(x$data)) |>
        lapply(function(x) x$data)

      failed_tests <- failed_tests[toc$Sheet]

      sheets <- c(list(TOC = toc), failed_tests)
      names_sheets <- names(sheets)

      wb <- openxlsx2::wb_workbook()

      for (ii in seq_along(sheets)) {
        ws_data <- sheets[[ii]]
        ws_name <- names_sheets[[ii]]

        wb <- openxlsx2::wb_add_worksheet(wb, sheet = ws_name)
        wb <- openxlsx2::wb_add_data(wb, ws_name, ws_data, na.strings = NULL)
      }

      openxlsx2::wb_save(wb, file = file, overwrite = TRUE)
      invisible(self)
    },
    #' @description
    #' For S3 print method
    print = function() {
      tbl_summary <- self$summary(invalid_only = FALSE)

      cat("<DataTest>: \n")
      if (nrow(tbl_summary) > 0) {
        cat("  number of tests: ", nrow(tbl_summary), "\n", sep = "")
        cat("  number of failed tests: ", tbl_summary[InvalidRows > 0, .N], "\n", sep = "")
      } else {
        cat("  0 test")
      }

      invisible(self)
    },
    #' @description
    #' Overwrite keep_cols on the flight
    #' @param cols_keep A character vector for columns to keep
    keep_cols = function(cols_keep = NULL) {
      private$.cols_to_keep <- cols_keep
      invisible(self)
    }
  ),
  private = list(
    .results = list(),
    .cols_to_keep = c()
  )
)

#' In-line checker
#'
#' @examples
#' cli_check_that(nrow(mtcars) == 32)
#' cli_check_that(nrow(mtcars) == 31, "# of rows for `mtcars` is 31")
#' cli_check_that(all.equal(data.frame(A = 1:3), data.frame(A = 3:1)))
#' cli_check_that(all.equal(data.frame(A = 1:3), data.frame(A = 1:3)), "two df are the same")
#'
#' @export
cli_check_that <- function(code, desc = NULL) {
  test_res <- eval(code, parent.frame())

  if (is.null(desc)) desc <- deparse(substitute(code))

  if (length(test_res) == 1 & (is.logical(test_res) || is.character(test_res))) {
    if (test_res == TRUE) {
      prefix <- "\u2705"
    } else {
      prefix <- "\u26D4"
    }
  } else {
    stop("`code` must yield a logical output after evaluation.", call. = FALSE)
  }

  cat(paste0(prefix, " ", desc), "\n", sep = "")
  invisible(NULL)
}

