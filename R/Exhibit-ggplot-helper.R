#' Basic ggplot2 theme
#'
#' @importFrom ggplot2 %+replace%
#' @export

gg_theme <- function(base_size = 12, base_family = "",
                     legend_position = "top",
                     show_axis = c("x", "xy", "y", "none"),
                     show_axis_text = c("xy", "x", "y", "none"),
                     show_grid = c("y", "xy", "x", "none"),
                     show_border = FALSE,
                     x_text_angle = 0,
                     x_text_hjust = NULL,
                     ...) {

  show_axis <- match.arg(show_axis)
  show_grid <- match.arg(show_grid)
  show_axis_text <- match.arg(show_axis_text)

  .theme <- ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.2), margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(color = "#7e7e7e", hjust = 0, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(color = "#7e7e7e", hjust = .1, face = "italic"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = "#d4dddd"),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#F2F2F2", color = "#d4dddd", size = 0.7),
      #plot.margin = margin(base_size / 2, base_size / 2, base_size / 2, base_size / 2),
      legend.position = legend_position,
      complete = TRUE
    )

  if (x_text_angle != 0 && grepl("x", show_axis)) {
    if (x_text_angle > 5 && is.null(x_text_hjust)) x_text_hjust <- 1

    .theme <- .theme + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = x_text_angle, hjust = x_text_hjust, color = "#4a4a4a"))
  }

  if (show_axis == "x") {
    .theme <- .theme +
      ggplot2::theme(
        axis.line.x = ggplot2::element_line(),
        axis.ticks.x = ggplot2::element_line(color = "#4a4a4a")
      )
  } else if (show_axis == "y") {
    .theme <- .theme +
      ggplot2::theme(
        axis.line.y = ggplot2::element_line(),
        axis.ticks.y = ggplot2::element_line(color = "#4a4a4a")
      )
  } else if (show_axis == "xy") {
    .theme <- .theme +
      ggplot2::theme(
        axis.line = ggplot2::element_line(),
        axis.ticks = ggplot2::element_line(color = "#4a4a4a")
      )
  }

  if (show_axis_text == "x") {
    .theme <- .theme +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(color = "#4a4a4a")
      )
  } else if (show_axis_text == "y") {
    .theme <- .theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(color = "#4a4a4a")
      )
  } else if (show_axis_text == "xy") {
    .theme <- .theme +
      ggplot2::theme(
        axis.text = ggplot2::element_text(color = "#4a4a4a")
      )
  }

  if (show_grid == "x") {
    .theme <- .theme +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_line())
  } else if (show_grid == "y") {
    .theme <- .theme +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line())
  } else if (show_grid == "xy") {
    .theme <- .theme +
      ggplot2::theme(panel.grid.major = ggplot2::element_line())
  }

  if (!show_border) .theme <- .theme + ggplot2::theme(panel.border = ggplot2::element_blank())

  .theme <- .theme + ggplot2::theme(...)
  .theme
}

#' output a ggplot chart
#' @param plt Plot object
#' @param file File name (with path)
#' @param width,height Width and height in inches
#' @param scale Scale factor for `width` and `height`.  If `scale` is 2, and `width` is 10, then scaled width will be 20
#' @export
gg_save <- function(plt, file, width = 9, height = 6, scale = 1) {
  ggplot2::ggsave(file, plot = plt, width = width, height = height, scale = scale)
  (plt)
}

#' A selection of color palettes
#'
#' @export
#' @examples
#' color_palette() |> plot()
#' color_palette("economist", c(1, 3, 5, 6)) |> plot()

color_palette <- function(name = c("economist", "gc"), idx = NULL) {
  name <- match.arg(name)
  paletteList <- list(
    "economist" = c("#01a2d9", "#014d64", "#6794a7", "#7ad2f6", "#00887d", "#76c0c1", "#7c260b", "#ee8f71", "#a18376", "#ecc265", "#f15a40")
    # "gc" = c("#002e5c", "#1490ad", "#046f80", "#6dc0c4", "#00977d", "#009e3c", "#76949d", "#8ccbb8", "#768278", "#a6b29f"),
  )

  paletteChosen <- paletteList[[name]]
  if (is.null(idx)) idx <- seq_along(paletteChosen)
  paletteChosen <- paletteChosen[idx]

  structure(paletteChosen, class = "ColorScheme", idx = idx)
}

#' @export
plot.ColorScheme <- function(p, ...) {
  idx <- attr(p, "idx")
  barplot(height = rep(1, times = length(p)), col = p, names.arg = idx, border = NA, axes = FALSE)
}

#' calculate contrast color for better visualization
#' @export
#' @examples
#' cors <- cor(mtcars[, 1:4])
#'
#' # Melt matrix
#' df <- data.frame(
#'   col = colnames(cors)[as.vector(col(cors))],
#'   row = rownames(cors)[as.vector(row(cors))],
#'   value = as.vector(cors)
#' )
#'
#' # Basic plot
#' p <- ggplot(df, aes(row, col, fill = value)) +
#'   geom_raster() +
#'   geom_text(aes(label = round(value, 2), color = after_scale(color_contrast(fill))))
#' p + scale_fill_viridis_c(direction =  1)

color_contrast <- function(color) {
  # if (!requireNamespace("farver", quietly = TRUE)) {
    col_light <- "#ffffff"
    col_dark <- "#000000"
    col_rgb <- col2rgb(color) / 255
    col_mat <- apply(col_rgb, 2, function(x) ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055) ^ 2.4))
    L  <- 0.2126 * col_mat[1, ] + 0.7152 * col_mat[2, ] + 0.0722 * col_mat[3, ]
    out <- ifelse(L > 0.179, col_dark, col_light)
    out
  # }
  #
  # out   <- rep("black", length(color))
  # light <- farver::get_channel(color, "l", space = "hcl")
  # out[light < 50] <- "white"
  # out

}

#' Helper function for dual-y-axis plot
#' @param y_left,y_right Numeric vector.
#'   If with two elements, linear transformation will be used
#'   If longer than two, min/max (or zero/max, if `force_zero` is `TRUE`) are used for linear transformation
#' @export
#' @examples
#' a <- data.table(Year = 2021:2023, Value = c(10, 12, 9), Growth = c(-.1, .2, -.25))
#' trans1 <- transform_dual_axis(c(10, 20), c(0, .2))
#' trans2 <- transform_dual_axis(a$Value, a$Growth)
#' trans3 <- transform_dual_axis(a$Value, a$Growth, force_zero = TRUE)
#' ggplot(a, aes(Year, Value)) +
#' geom_col() +
#' geom_line(aes(y = trans2$to_left(Growth))) +
#' scale_y_continuous(sec.axis = sec_axis(transform = trans2$transform))
#'
transform_dual_axis <- function(y_left, y_right, force_zero = FALSE) {
  min_primary <- min(y_left, na.rm = TRUE)
  min_secondary <- min(y_right, na.rm = TRUE)
  max_primary <- max(y_left, na.rm = TRUE)
  max_secondary <- max(y_right, na.rm = TRUE)

  if (force_zero) {
    min_primary <- 0
    min_secondary <- 0
  }

  b <- (max_secondary - min_secondary) / (max_primary - min_primary)
  a <- max_secondary - b * max_primary

  to_left <- function(x) (x - a) / b
  to_right <- function(x) a + b * x # sec_axis's trans function is converting primary to secondary

  res <- list(
    to_left = to_left,
    transform = to_right,
    to_right = to_right
  )

  return(res)
}
