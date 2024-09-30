#' Basic format for `gt` table
#'
#' @export
#' @details
#' Some tips of using `gt` package
#' * use `gt::gtsave` to save table as png
#' * check `gt::cols_label` for column header renaming
#' * check `gt::cols_merge_range` to combine Lbound and Ubound into one
#' @examples
#' x <- data.table::data.table(
#'   " " = as.character(1:3),
#'   "Terms_Limit" = c(10e6, 10e6, 20e6),
#'   "Terms_Deductible" = c(10e6, 20e6, 30e6),
#'   "Terms_Reinstatement" = c("2@100%", "2@100%", "1@100%"),
#'   "Price_ROL" = c(.4, .35, NA)
#'   )
#' gt(x)
#' gt(x) |> gt_theme(spanner_split = "_", theme = "green", title = "Price Table") |> gt::fmt_number(2:3, suffixing = TRUE)
#' gt(x) |> gt_theme(title = "Price Table", title_block_color = "blue") |> gt_split(2)

gt_theme <- function(data, theme = c("blue", "blue.fill", "green", "green.fill"), spanner_split = NA, title = NULL, title_block_color = "#ee1c25",...) {
  if (!inherits(data, "gt_tbl")) stop("`data` must be a `gt_tbl` object.", call. = FALSE)

  theme <- match.arg(theme)
  .theme_color <- ifelse(grepl("^blue", theme), "blue", "cyan")
  .theme_style <- ifelse(grepl("fill$", theme), 6, 1)

  gt0 <- data |>
    gt::opt_stylize(style = .theme_style, color = .theme_color, add_row_striping = TRUE)

  # add spanner (i.e. column groups)
  if (!is.na(spanner_split))  {
    gt0 <- gt0 |>
      gt::tab_spanner_delim(delim = spanner_split)
  }

  if (!is.null(title)) {
    if (!is.na(title_block_color)) {
      gt0 <- gt0 |>
        gt::tab_header(title = gt::html("<div>", "<span style='background-color:", title_block_color, ";color:", title_block_color, ";'>...</span>","<span><b>", title, "</b></span></div>"))
    } else {
      gt0 <- gt0 |>
        gt::tab_header(title = gt::html("<div>","<span><b>", title, "</b></span></div>"))
    }
  }

  # final touch
  gt0 <- gt0 |>
    gt::tab_options(
      table.border.top.color = "transparent",
      heading.align = "left",
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold"
    )

  gt0
}

#' @export
gt_save <- function(x, file, vwidth = 1000, vheight = 800, expand = 10, trim = FALSE, ...) {
  if (inherits(x, "gt_tbl")) {
    gt_save_(x, file, vwidth = vwidth, vheight = vheight, expand = expand, trim = trim, ...)
  } else if (inherits(x, "gt_group")) {
    for (i in 1:nrow(x$gt_tbls)) {
      tmp_gt <- gt::grp_pull(x, i)
      tmp_file <- gsub(".png", paste0("_", i, ".png"), file, fixed = TRUE)
      gt_save_(tmp_gt, tmp_file, vwidth = vwidth, vheight = vheight, expand = expand, trim = trim, ...)
    }
  } else {
    stop("`x` must be a `gt_tbl` or a `gt_group`.", call. = FALSE)
  }
}

gt_save_ <- function(gt, file, vwidth = 1000, vheight = 800, expand = 10, trim = FALSE, ...) {
  gt::gtsave(gt, file, vwidth = vwidth, vheight = vheight, expand = expand, ...)

  if (trim & requireNamespace("magick", quietly = TRUE)) {
    img_rework <- magick::image_read(file)
    img_rework <- magick::image_trim(img_rework)
    img_info <- magick::image_info(img_rework)
    magick::image_write(img_rework, file)
    attr(file, "info") <- img_info
  }
  (gt)
}
