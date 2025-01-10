# -----------------------------------------------------------------------------
#' Create a heatmap showing the value across all items
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param fill_colour colour to use for the tiles
#' @param y_label string for y-axis label. Optional
#' @return ggplot
#' @noRd
plot_heatmap_static <- function(prepared_df,
                                fill_colour = "blue",
                                y_label = NULL) {

  # initialise known column names to prevent R CMD check notes
  item <- timepoint <- value <- NULL

  if (nrow(prepared_df) == 0){
    return(empty_plot_static())
  }

  data <- prepared_df |>
    # replace separator with prettier one if multiple item_cols were specified.
    # assumes the separator is unusual enough to not bother checking the inputspec
    dplyr::mutate(item = gsub(":~:", " - ", item, fixed = TRUE)) |>
    dplyr::mutate(item = factor(item, levels = unique(item)))

  # when the only values are zero, make sure the fill colour is white (as
  # geom_tile uses the 'high' colour)
  if (all(data$value == 0, na.rm = TRUE)) {
    fill_colour <- "white"
  }

  g <-
    ggplot2::ggplot(
      data,
      ggplot2::aes(timepoint, item, fill = value)
    ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      "",
      low = "white",
      high = fill_colour,
      na.value = "grey",
      limits = c(0, NA)
    ) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
      expand = c(0, 0)
    ) +
    ggplot2::labs(y = y_label, x = NULL) +
    # facet by variable (item) to create separate bars
    ggplot2::facet_grid(item ~ ., scales = "free", space = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # remove grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # remove facet labels and their background
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      # add borders to the bars
      panel.border = ggplot2::element_rect(
        colour = "darkgrey",
        fill = NA,
        size = 0.75
      ),
      # remove space between facets
      panel.spacing = ggplot2::unit(0, "lines"),
      # remove y-axis ticks
      axis.ticks.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.35,
        hjust = 1,
        size = 7
      ),
      axis.text.y = ggplot2::element_text(size = 7),
      legend.position = "right",
    )

  g
}

# -----------------------------------------------------------------------------
#' Create a column of plots showing value across all items
#'
#' Currently only creates scatter plots
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param sync_axis_range Set the y-axis to be the same range for all the plots.
#'   X-axes are always synced.
#' @param y_label string for y-axis label. Optional
#' @return ggplot
#' @noRd
plot_multipanel_static <- function(prepared_df,
                                   sync_axis_range = FALSE,
                                   y_label = NULL) {

  # initialise known column names to prevent R CMD check notes
  item <- timepoint <- value <- NULL

  if (nrow(prepared_df) == 0){
    return(empty_plot_static())
  }

  data <- prepared_df |>
    # replace separator with prettier one if multiple item_cols were specified.
    # assumes the separator is unusual enough to not bother checking the inputspec
    dplyr::mutate(item = gsub(":~:", " - ", item, fixed = TRUE)) |>
    dplyr::mutate(item = factor(item, levels = unique(item)))

  g <-
    ggplot2::ggplot(
      data,
      ggplot2::aes(timepoint, value)
    ) +
    ggplot2::geom_point(na.rm = TRUE, shape = 4, size = 0.5) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      position = "right",
    ) +
    ggplot2::labs(
      y = y_label,
      x = NULL) +
    # create column of plots
    ggplot2::facet_grid(
      item ~ .,
      switch = "y",
      scales = ifelse(sync_axis_range, "fixed", "free")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # remove grid lines
      panel.grid.minor = ggplot2::element_blank(),
      # format facet labels
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0),
      # add borders to the bars
      panel.border = ggplot2::element_rect(
        colour = "darkgrey",
        fill = NA,
        size = 0.75
      ),
      # set space between facets
      panel.spacing = ggplot2::unit(0.5, "lines"),
      axis.title = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.35,
        hjust = 1,
        size = 7
      ),
      axis.text.y = ggplot2::element_text(size = 7),
      plot.title = ggplot2::element_text(size = 8, face = "bold", hjust = 0.5),
      legend.position = "none",
    )

  g
}


#' Create an empty gglot object
#'
#' Use when there is no data but you still want to display a ggplot (for consistency)
#'
#' @return ggplot
#' @noRd
empty_plot_static <- function(){

  # initialise known column names to prevent R CMD check notes
  x <- y <- NULL

  ggplot2::ggplot(data.frame(x = 0, y = 0),
                  ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_bw() +
    ggplot2::geom_blank() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      # remove axis ticks and labels
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      # remove grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::annotate("text", x = 0, y = 0, label = "No data")

}
