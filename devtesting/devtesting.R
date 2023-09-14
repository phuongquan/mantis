devtools::load_all(".")

data("example_data")

df <- example_data
timepoint_col <- "timepoint"
item_col <- "item"
value_col <- "value"
item_label <- "Item"

table <-
  prepare_table(
    df,
    timepoint_col,
    item_col,
    value_col,
    item_order = c("norm", "norm_na", "na_norm", "zero")
  )

table %>%
  output_table_html(
    item_label = "item",
    history_style = "bar",
    summary_cols = c("last_value", "mean"),
    sync_axis_range = FALSE
  )


table <-
  prepare_table(
    df,
    timepoint_col,
    item_col,
    value_col,
    fill_with_zero = TRUE
  )

table %>%
  output_table_html(
    item_label = "item",
    history_style = "bar",
    summary_cols = c("last_value", "mean"),
    sync_axis_range = FALSE
  )

# set timepoint limits
prepare_table(
  df,
  timepoint_col,
  item_col,
  value_col,
  timepoint_limits = c(as.Date("2022-06-01"), NA)
) %>%
output_table_html(
  item_label = "item",
  history_style = "bar",
  summary_cols = c("last_value"),
  sync_axis_range = FALSE
)

prepare_table(
  df %>% dplyr::filter(startsWith(item, "sparse")),
  timepoint_col,
  item_col,
  value_col,
  timepoint_limits = c(min(df$timepoint), max(df$timepoint))
) %>%
  output_table_html(
    item_label = "item",
    history_style = "bar",
    summary_cols = c("last_value"),
    sync_axis_range = FALSE
  )


tinduck::export_table_html(df,
               colspec = list(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value")
               )

dfgroups <- df %>%
  dplyr::mutate(group = dplyr::case_when(grepl("norm", item) ~ "norm",
                                         grepl("sparse", item) ~ "sparse",
                                         TRUE ~ "other"))

tinduck::export_grouped_table_html(dfgroups,
                                   colspec = tinduck::colspec(timepoint_col = "timepoint",
                                                    item_col = "item",
                                                    value_col = "value",
                                                    group_col = "group"),
                                   )
