library(tidyverse)
library(arrow)
library(sf)
library(leaflet)

# For cleaning final data in parquet format, see data_cleaning.R script
data <- read_parquet("./data/data_final.parquet")

data_sf <- 
  sf::st_as_sf(
    data,
    # Pretty sure x represents longitude (NOT latitude)
    coords = c("x", "y"), # coords takes longitude first
    # Set our coordinate reference system to EPSG:32188 ,
    # the standard WGS84 geodetic coordinate reference system
    crs = 32188
  )


coordinates_nad <- st_coordinates(data_sf) # extract coordinates

data_sf_wgs <- st_transform(data_sf, crs = 4326)

pal <- colorNumeric(
  palette = "viridis",
  domain = data_sf$acc)
leaflet(data = data_sf_wgs) |> 
  addTiles() |> 
  addProviderTiles("CartoDB.Positron") |> 
  addCircleMarkers(radius = 7,
                   fillColor = ~pal(acc), stroke = FALSE, fillOpacity = 0.8,
                   popup = ~str_glue("<b>Number of accidents: </b>{acc}<br/>
                                       <b>Intersection: </b>{rue_1} & {rue_2}<br/>
                                       <b>Borough: </b>{borough}"), 
                   label = ~str_glue("Intersection number: {int_no}")) %>%
  addLegend(pal = pal, values = ~acc, position = "bottomright", title = "Accidents")

# Some nice map palettes c("Stadia.AlidadeSmooth", "CartoDB.Positron")
