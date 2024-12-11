library(leaflet)
library(sf)
library(RColorBrewer)
library(tigris)
library(dplyr)
library(viridis)
library(htmlwidgets)


# Load PUMA shapefile for Iowa
iowa_pumas <- tigris::pumas(state = "IA", year = 2022, class = "sf")

# Merge shapefiles with ACS data
iowa_map_data <- iowa_pumas %>%
  left_join(acs_data_predict_agg_FSWROUTY, by = c("GEOID20" = "GEOID"))

# Transform to WGS84 for mapping
iowa_map_data <- st_transform(iowa_map_data, 4326)

# Create a color palette for the `proportion_of_population` column
pal <- colorBin(
  palette = viridis(5, direction = -1),  # Reverses the color scale
  domain = iowa_map_data$proportion_of_population,
  bins = 5
)

# Plot with Leaflet
leaflet(iowa_map_data) %>%
  addTiles() %>%  # Add base map tiles
  addPolygons(
    fillColor = ~pal(proportion_of_population),
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    highlight = highlightOptions(
      weight = 3,
      color = "black",
      bringToFront = TRUE
    ),
    label = ~paste0("PUMA: ", NAMELSAD20, 
                    "<br>Proportion: ", round(proportion_of_population, 2)),
    popup = ~paste0("PUMA: ", NAMELSAD20, 
                    "<br>Proportion: ", round(proportion_of_population, 2))
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~proportion_of_population,
    title = "Proportion of Food Insecure Households",
    opacity = 0.7
  )



