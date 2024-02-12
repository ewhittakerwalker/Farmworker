library(shiny)
library(sf)
library(ggplot2)
library(viridis)
library(leaflet)
library(stringr)


load("/Users/ewanwhittaker-walker/Rshiny/merged_map.rda")

map <- st_transform(df_merge, 4326)

pal <- colorNumeric("YlOrRd", domain = map$voting)

out <- leaflet(map, options = leafletOptions(minZoom = 5.35)) %>%
  addTiles() %>%
  addPolygons(
    color = "white", 
    fillColor = ~ pal(voting),
    fillOpacity = 1, 
    layerId = map$GEOID
  ) %>% 
  addLegend(pal = pal, values = ~voting, opacity = 1) %>% 
  setView(lng = -120.00000, lat = 37.00000, zoom = 5.35) %>%
  setMaxBounds(map.getBounds())

out
