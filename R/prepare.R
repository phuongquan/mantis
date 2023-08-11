#' Convert supplied df into required format for generating tables/plots
#'
#' Supplied df needs to be long (at least for now)
#'
#' @param df
#' @param timepoint_col
#' @param item_col
#' @param value_col
#' @param history_type "value" or "delta
#'
#' @return data frame
#' @export
#'
#' @examples
prepare_table <-
  function(df,
           timepoint_col,
           item_col,
           value_col,
           history_type = "value") {

  # TODO: Validate supplied colnames against df
  # TODO: insert any missing timepoint values? Need to take care if df is long.
  # TODO: allow df to be passed in wide with vector of value_cols?
  # TODO: keep original item order? Or allow order to be passed in?

  # initialise column names to avoid R CMD check Notes
  timepoint <- item <- value <- value_for_history <- NULL

  # rename cols for ease. may want to figure out how to keep original colnames
  table_df <-
    df %>%
    dplyr::rename(timepoint = timepoint_col,
                  item = item_col,
                  value = value_col)

  # add history column
  table_df <-
    table_df %>%
    dplyr::group_by(item) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::mutate(
      value_for_history = dplyr::case_when(
        history_type == "none" ~ NA_integer_,
        history_type == "value" ~ as.numeric(value),
        history_type == "delta" ~ as.numeric(value) - dplyr::lag(as.numeric(value))
      )
    ) %>%
    dplyr::summarise(
      last_timepoint = max(timepoint[!is.na(value)]),
      last_value = rev(value)[1],
      # TODO: match precision to values
      mean = round(mean(value, na.rm = TRUE),
                   digits = 1),
      mean_last14 = round(mean(rev(value)[1:14], na.rm = TRUE),
                          digits = 1),
      history = history_to_list(value_for_history,
                                timepoint,
                                history_type),
      .groups = "drop"
    )

  table_df
}


history_to_list <-
  function(value_for_history,
           timepoint,
           history_type) {

    ts <-
      data.frame(value_for_history,
                 timepoint,
                 row.names = 2) %>%
      xts::as.xts() %>%
      list()
    names(ts[[1]]) <- history_type
    ts
  }
