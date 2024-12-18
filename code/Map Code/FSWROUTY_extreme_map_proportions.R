# Load necessary libraries

library(tigris)       # To get PUMA shapefiles

library(dplyr)        # For data wrangling

library(ggplot2)      # For plotting

library(sf)           # For handling spatial data



# Get Iowa PUMA shapefiles

iowa_pumas <- tigris::pumas(state = "IA", year = 2022, class = "sf")


# Merge shapefiles with data

iowa_map_data <- iowa_pumas %>%
  
  left_join(acs_data_predict_agg_FSWROUTY_extreme, by = c("GEOID20" = "GEOID"))



# Plot the choropleth map

ggplot(data = iowa_map_data) +
  
  geom_sf(aes(fill = proportion_of_population), color = "white", size = 0.2) + # Fill based on 'value'
  
  scale_fill_viridis_c(option = "plasma", name = "Proportion*", direction = -1) + # Viridis color scale
  
  theme_minimal() +
  
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), plot.caption = element_text(hjust = 0.5) ) +
  
  labs(
    
    title = "Predicted Proportion of Food Insecure Seniors by PUMA",
    
    caption = "*Proportion of households with one or more seniors who have
    very often felt at risk of running out of food and not being able to affored more"
    
  )
