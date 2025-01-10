# Required packages
library(sf)
library(ggplot2)

# Load the shapefile (adjust the path as needed)
shapefile_path <- "NUTS_RG_20M_2024_3035.shp"
nuts_data <- st_read(shapefile_path)


# Manually set zoomed-in limits for Europe
zoomed_bbox <- c(xmin = 1200000, xmax = 7000000, ymin = 1000000, ymax = 6500000)

# Plot the NUTS2 regions using ggplot2 with regions clearly highlighted and consistent styling
ggplot(data = nuts_data) +
  geom_sf(aes(fill = LEVL_CODE), color = "black", size = 0.1) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  coord_sf(xlim = c(zoomed_bbox["xmin"], zoomed_bbox["xmax"]),
           ylim = c(zoomed_bbox["ymin"], zoomed_bbox["ymax"]),
           crs = st_crs(nuts_data)) +
  labs(title = "NUTS Regions", x = "Longitude", y = "Latitude", fill = "LEVL_CODE") +
  theme_minimal()
