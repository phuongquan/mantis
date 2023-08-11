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
    value_col
  )

table %>%
  output_table_html(
    item_label = "item",
    history_style = "bar",
    summary_cols = c("last_value", "mean"),
    sync_axis_range = FALSE
  )

