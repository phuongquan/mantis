# -----------------------------------------------------------------------------
#' Create a heatmap showing the value across all items
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param outputspec options for plot specified using `outputspec_heatmap_static()`
#' @return ggplot
#' @noRd
plot_heatmap_static <- function(prepared_df,
                                outputspec = outputspec_heatmap_static(
                                  fill_colour = "blue",
                                  y_label = NULL)) {

  # initialise known column names to prevent R CMD check notes
  item <- timepoint <- value <- NULL

  data <- prepared_df %>%
    dplyr::mutate(item = factor(item, levels = unique(prepared_df$item)))

  fill_colour <- outputspec$fill_colour

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
      "Instances",
      low = "white",
      high = fill_colour,
      na.value = "grey",
      labels = NULL,
      limits = c(0, NA)
    ) +
    ggplot2::scale_x_date(
      breaks = scales::breaks_pretty(12),
      labels = scales::label_date_short(sep = " "),
      expand = c(0, 0)
    ) +
    ggplot2::labs(y = outputspec$y_label, x = NULL) +
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
      legend.position = "none",
    )

  g
}

# -----------------------------------------------------------------------------
#' Create a grid of plots showing value across all items
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param outputspec options for plot specified using `outputspec_multiplot_static()`
#' @return ggplot
#' @noRd
plot_multiplot_static <- function(prepared_df,
                                  outputspec = outputspec_multiplot_static(
                                    sync_axis_range = FALSE,
                                    y_label = NULL)) {

  # initialise known column names to prevent R CMD check notes
  item <- timepoint <- value <- NULL

  data <- prepared_df %>%
    dplyr::mutate(item = factor(item, levels = unique(prepared_df$item)))

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
      y = outputspec$y_label,
      x = NULL) +
    # create column of plots
    ggplot2::facet_grid(
      item ~ .,
      switch = "y",
      scales = ifelse(outputspec$sync_axis_range, "fixed", "free")) +
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
