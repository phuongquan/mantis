# -----------------------------------------------------------------------------
#' Create a heatmap showing the value across all items
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param fill_colour colour to use for the tiles
#' @param item_label string for end of y-axis label
#' @param plot_value_type "value" or "delta"
#' @return ggplot
#' @noRd
plot_heatmap_static <- function(prepared_df,
                                fill_colour = "blue",
                                item_label = "item",
                                plot_value_type = "value") {

  # initialise known column names to prevent R CMD check notes
  item <- timepoint <- value <- NULL

  data <- prepared_df %>%
    dplyr::mutate(item = factor(item, levels = unique(prepared_df$item)))

  if(plot_value_type == "delta"){
    data <- data %>%
      # TODO: should this be done in prepare_df() instead?
      dplyr::mutate(value = as.numeric(value) - dplyr::lag(as.numeric(value)))
    value_label = "Delta"
  } else{
    value_label = "Value"
  }

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
    ggplot2::labs(y = paste(value_label, "per", item_label), x = NULL) +
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
