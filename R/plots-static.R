# -----------------------------------------------------------------------------
#' Create a heatmap showing the value across all items
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param inputspec Specification of data in df
#' @param fill_colour colour to use for the tiles
#' @param y_label string for y-axis label. Optional
#' @return ggplot
#' @noRd
plot_heatmap_static <- function(
  prepared_df,
  inputspec,
  fill_colour = "blue",
  y_label = NULL
) {
  # initialise known column names to prevent R CMD check notes
  item <- timepoint <- value <- NULL

  if (nrow(prepared_df) == 0) {
    return(empty_plot_static())
  }

  # if an item_col is used for a tabset, move it to the y_label
  item_cols_plot <- base::setdiff(inputspec$item_cols, inputspec$tab_col)
  if (is.null(y_label)) {
    y_label <- paste0(item_cols_plot, collapse = " - ")
    if (!is.null(inputspec$tab_col)) {
      current_tab <- prepared_df[item_cols_prefix(inputspec$tab_col)][[1]][1]
      y_label <- paste(y_label, current_tab, sep = " - ")
    }
  }
  data <- prepared_df |>
    # combine item_cols into single variable
    tidyr::unite(
      col = "item",
      dplyr::all_of(item_cols_prefix(item_cols_plot)),
      sep = " - "
    ) |>
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
        linewidth = 0.75
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
#' @param inputspec Specification of data in df
#' @param sync_axis_range Set the y-axis to be the same range for all the plots.
#'   X-axes are always synced.
#' @param y_label string for y-axis label. Optional
#' @return ggplot
#' @noRd
plot_multipanel_static <- function(
  prepared_df,
  inputspec,
  sync_axis_range = FALSE,
  y_label = NULL
) {
  # initialise known column names to prevent R CMD check notes
  item <- timepoint <- value <- NULL

  if (nrow(prepared_df) == 0) {
    return(empty_plot_static())
  }

  # if the item_col is used for a tabset, move it to the y_label
  item_cols_plot <- base::setdiff(inputspec$item_cols, inputspec$tab_col)
  if (is.null(y_label)) {
    y_label <- paste0(item_cols_plot, collapse = " - ")
    if (!is.null(inputspec$tab_col)) {
      current_tab <- prepared_df[item_cols_prefix(inputspec$tab_col)][[1]][1]
      y_label <- paste(y_label, current_tab, sep = " - ")
    }
  }
  data <- prepared_df |>
    # combine item_cols into single variable
    tidyr::unite(
      col = "item",
      dplyr::all_of(item_cols_prefix(item_cols_plot)),
      sep = " - "
    ) |>
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
    # TODO: try to place scale on right but axis label on left
    # secondary axis errors when some panels are all NAs
    ggplot2::scale_y_continuous(
      position = "right"
    ) +
    ggplot2::labs(
      y = y_label,
      x = NULL
    ) +
    # create column of plots
    ggplot2::facet_grid(
      item ~ .,
      switch = "y",
      scales = ifelse(sync_axis_range, "fixed", "free")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # remove grid lines
      panel.grid.minor = ggplot2::element_blank(),
      # format facet labels
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1),
      # add borders to the bars
      panel.border = ggplot2::element_rect(
        colour = "darkgrey",
        fill = NA,
        linewidth = 0.75
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
      legend.position = "none",
    )

  g
}


# -----------------------------------------------------------------------------
#' Create an empty gglot object
#'
#' Use when there is no data but you still want to display a ggplot (for
#' consistency)
#'
#' @return ggplot
#' @noRd
empty_plot_static <- function() {
  # initialise known column names to prevent R CMD check notes
  x <- y <- NULL

  ggplot2::ggplot(
    data.frame(x = 0, y = 0),
    ggplot2::aes(x = x, y = y)
  ) +
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
