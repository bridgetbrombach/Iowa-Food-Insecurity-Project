
# Install and Load Libraries

library(sf)
library(dplyr)
library(leaflet)

#  Get County Boundaries
#https://usa.ipums.org/usa/volii/boundaries.shtml
puma <- st_read("data/ipums_puma_2020.shp")

acs_data_predict_agg_FSSTATUS <- acs_data_predict_agg_FSSTATUS %>% 
  rename(GEOID = PUMA)


# Merge Data
# Merge PUMA shapefile with ACS Predictions data
puma_data <- puma %>%
  left_join(acs_data_predict_agg_FSSTATUS, by = "GEOID")

# Filter for Iowa PUMAs by State Column
iowa_puma <- puma_data %>% filter(State == "Iowa")  

#Manually Assign Coordinate Reference System
st_crs(iowa_puma) <- 26915

#Transform CRS to Lat/Long for use in Map
iowa_puma <- st_transform(iowa_puma, 4326)



# Create quantiles
breaks <- quantile(iowa_puma$meanstuff, probs = seq(0, 1, length.out = 6), na.rm = TRUE)

# Create custom labels based on the quantiles
labels <- sapply(1:(length(breaks) - 1), function(i) {
  paste0(
    "From ", 
    round(breaks[i], 2),  # Lower bound of the quantile
    " to ", 
    round(breaks[i + 1], 2)  # Upper bound of the quantile
  )
})


# Create a Color Palette
pal <- colorBin("YlOrRd", domain = iowa_puma$meanstuff, bins = breaks, na.color = "transparent")

# Create the Map
leaflet(iowa_puma) %>%
  addTiles() %>%  # Add base map tiles
  addPolygons(
    fillColor = ~pal(meanstuff),  # Color scale based on meanstuff
    fillOpacity = 0.7,
    weight = 1,
    color = "white",
    highlight = highlightOptions(
      weight = 3,
      color = "black",
      bringToFront = TRUE
    ),
    label = ~paste0("PUMA Name: ", Name, 
                    "Value: ", meanstuff),  # Popup label showing meanstuff
    popup = ~paste0("PUMA:", Name, 
                    "Value: ", meanstuff)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~meanstuff,
    title = "Proportion of FSSTATUS",
    opacity = 0.7,
    labels = labels

  )


