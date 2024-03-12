source("./scripts/modelling_setup.R")
library(sf)
library(leaflet)

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






