library(arrow) # parquet file format
library(sf) # SF for spatial analysis
library(geosphere) # calculating geodistances
library(tidyverse) # data analysis

# Donnée disponible: https://donnees.montreal.ca/dataset/stm-traces-des-lignes-de-bus-et-de-metro
# Date d'ajout de l'ensemble de données: 2016-04-25 19:56
stm <- 
  read_sf("./raw_data/stm_stops/stm_arrets_sig.shp") |> 
  filter(loc_type == 0) # these are bus stops (loc_type == 2 are metro stops)

# Cleaned intersections data
intersections_sf <- 
  read_parquet("./processed_data/data_final.parquet") |> 
  # converting to sf object
  st_as_sf(
    coords = c("x", "y"), # coords takes longitude first
    # Set our coordinate reference system to EPSG:32188,
    crs = 32188
  )

# Extracting coordinates from sf object and converting to WGS84 
coord_stm <- 
  st_transform(stm, crs = 4326) |> 
  st_coordinates()
coord_inter <- 
  st_transform(intersections_sf, crs = 4326) |> 
  st_coordinates()

# Creating a function to apply over all points
dist_mat <- function(X){
  # X : a matrix of 2 columns (first column is longitude, second column is latitude) 
  # calculates distance between long/lat point and every intersection in dataset
  distGeo(X, coord_inter) 
}

# Creating distance matrix between all intersection, bus stops
inter_stm_dist <- apply(coord_stm, 1, dist_mat)
rownames(inter_stm_dist) <- intersections_sf$int_no
# have to paste because actual id is combination (I think... review dataset)
colnames(inter_stm_dist) <- paste(stm$stop_code, stm$stop_id, sep = "_")

# extracting some information for final dataset
min_dist <- apply(inter_stm_dist, 1, min, simplify = TRUE)
min_index <- apply(inter_stm_dist, 1, which.min, simplify = TRUE)

final_stm <-
  tibble(
    int_no = intersections_sf$int_no,
    stop_code_id = colnames(inter_stm_dist)[min_index],
    min_dist = min_dist,
    within_15m = factor(if_else(floor(min_dist) <= 15, "yes", "no"))
  )

write_parquet(final_stm, "./processed_data/stm_stops.parquet")




