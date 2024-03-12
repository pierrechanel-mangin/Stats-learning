source("./scripts/modelling_setup.R")

stage1_pred <- read_parquet("./output/stage1_pred.parquet")
stage2_pred <- read_parquet("./output/stage2_pred.parquet")
final_pred_tb <- 
  intersections_df |> 
  select(int_no, acc, acc_bin, x, y) |> 
  left_join(stage2_pred, by = "int_no") |> 
  left_join(select(stage1_pred, -acc_bin), by = "int_no") |> 
  mutate(.pred = replace_na(.pred, 0)) |> 
  arrange(desc(.pred), desc(pred_prob))

final_pred_tb |> 
  select(int_no) |> 
  mutate(rank = row_number()) |> 
  write_csv("./output/rank_results.csv")

library(sf)
library(leaflet)
# Maps ----
inter_sf <- 
  sf::st_as_sf(
    final_pred_tb,
    # Pretty sure x represents longitude (NOT latitude)
    coords = c("x", "y"), # coords takes longitude first
    # Set our coordinate reference system to EPSG:32188 ,
    # the standard WGS84 geodetic coordinate reference system
    crs = 32188
  )

pred_rank <- 
  inter_sf |> 
  mutate(rank = row_number()) |> 
  filter(rank<=100) |> 
  st_transform(crs = 4326)
true_rank <- 
  inter_sf |> 
  arrange(desc(acc)) |> 
  mutate(rank = row_number()) |> 
  filter(rank<=100)|> 
  st_transform(crs = 4326)

# Actual
pal <- colorNumeric(
  palette = "viridis",
  domain = true_rank$acc)
leaflet(data = true_rank) |> 
  addTiles() |> 
  addProviderTiles("CartoDB.Positron") |> 
  addCircleMarkers(radius = 7,
                   fillColor = ~pal(acc), stroke = FALSE, fillOpacity = 0.8, 
                   label = ~str_glue("Intersection number: {int_no}")) %>%
  addLegend(pal = pal, values = ~acc, position = "bottomright", title = "Accidents")

# Predicted
pal <- colorNumeric(
  palette = "viridis",
  domain = pred_rank$.pred)
leaflet(data = pred_rank) |> 
  addTiles() |> 
  addProviderTiles("CartoDB.Positron") |> 
  addCircleMarkers(radius = 7,
                   fillColor = ~pal(.pred), stroke = FALSE, fillOpacity = 0.8, 
                   label = ~str_glue("Intersection number: {int_no}")) %>%
  addLegend(pal = pal, values = ~.pred, position = "bottomright", title = "Accidents")



library(gt)
data <- read_parquet("./processed_data/data_final.parquet")
data |> 
  select(int_no, acc, x, y, rue_1, rue_2) |> 
  left_join(stage2_pred, by = "int_no") |> 
  left_join(select(stage1_pred, -acc_bin), by = "int_no") |> 
  mutate(.pred = replace_na(.pred, 0)) |> 
  arrange(desc(.pred), desc(pred_prob)) |> 
  mutate(rank = row_number()) |> 
  filter(rank<=10) |> 
  select(rank, predicted_accident = .pred, rue_1, rue_2) |> 
  mutate(intersection = str_glue("{rue_1}/{rue_2}")) |> 
  select(-c(rue_1, rue_2)) |> 
  gt(rowname_col = "rank") |>
  cols_label(
    predicted_accident = "**Predicted**<br>Number of accidents",
    intersection = "**Intersection**",
    .fn = md
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = md("Top 10 **riskiest** intersections"),
    subtitle = md("Ranking of intersections and the predicted number of accidents")
  ) |> 
  tab_options(table.background.color = "#F5F5F5")



