#' Work on fields of a `data.frame`
#' @examples
#' DT <- data.frame(A = 1:3, B = LETTERS[5:7])
#' data.table::copy(DT) |> field_map(Num = "A", Char = "B")
#' data.table::copy(DT) |> field_map(c(Num = "A", Char = "B")) |> field_check("Char") |> field_fill(c("X", "Y"))
field_map <- function(x, ...) {
  if (!is.data.frame(x)) stop("`x` must be a data.frame.", call. = FALSE)
  data.table::setDT(x)
  dots <- unlist(dots_list(...))
  if (!is.null(dots)) data.table::setnames(x, unname(dots), names(dots), skip_absent = TRUE)
  x
}

field_check <- function(x, required_field = character()) {
  if (!is.data.frame(x)) stop("`x` must be a data.frame.", call. = FALSE)

  if (!all(required_field %in% names(x))) {
    stop("The following fields are not found in `x`: ", paste(setdiff(required_field, names(x)), collapse = ", "), call. = FALSE)
  }

  x
}

field_fill <- function(x, vars, fill = NA_real_) {
  x <- data.table::setDT(x)

  nm <- names(vars)
  for (v in vars) {
    if (!v %in% nm) data.table::set(x, j = v, value = fill)
  }

  x[]
}

#' Work on `...`
#' @examples
#' dots_list(a = 1, b = head(mtcars))
#' dots_list(list(a = 1, b = head(mtcars))) # when the 1st and only component is a list
#' add_spec <- function(x, ...) c(x, dots_list(...))
#' add_spec(list(a = 1), b = 2, c = 3)

dots_list <- function(...) {
  dots <- list(...)

  # if (anyDuplicated(names(dots))) stop("Arguments can't have the same name.", call. = FALSE)

  if (length(dots) == 1 && is.list(dots[[1]])) {
    dots <- dots[[1]]
  }

  dots
}

format_Numeric <- function(x, style = c("vector", "range", "dim"), decimal = 0) {
  style <- match.arg(style)

  if (is.null(x)) {
    res <- "NULL"
  } else {
    if (style == "vector") {
      x1 <- format(round(x, decimal), nsmall = decimal) |> paste(collapse = " ")
    } else if (style == "range") {
      x1 <- format(round(range(x), decimal), nsmall = decimal) |> paste(collapse = " ")
    } else if (style == "dim") {
      if (is.null(dim(x))) {
        dimX <- c(length(x), 1)
      } else {
        dimX <- dim(x)
      }
      x1 <- paste(dimX, collapse = "x")
    }

    res <- paste0("[", x1, "]")
  }

  res
}
