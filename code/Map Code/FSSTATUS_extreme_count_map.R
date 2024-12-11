library(tigris)       # To get PUMA shapefiles

library(dplyr)        # For data wrangling

library(ggplot2)      # For plotting

library(sf)           # For handling spatial data



# Get Iowa PUMA shapefiles

iowa_pumas <- tigris::pumas(state = "IA", year = 2022, class = "sf")


# Merge shapefiles with data

iowa_map_data <- iowa_pumas %>%
  
  left_join(acs_data_predict_agg_FSSTATUS_extreme, by = c("GEOID20" = "GEOID"))



# Plot the choropleth map

ggplot(data = iowa_map_data) +
  
  geom_sf(aes(fill = count_of_seniors), color = "white", size = 0.2) + # Fill based on 'value'
  
  scale_fill_viridis_c(option = "plasma", name = "Count*", direction = -1) + # Viridis color scale
  
  theme_minimal() +
  
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), plot.caption = element_text(hjust = 0.5)) +
  
  labs(
    
    title = "Precicted Count of Food Insecure Seniors",
    
    caption = "*Count based on number of seniors who indicate that they are 
    very low food secure"
    
  )

