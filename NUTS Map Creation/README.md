
## Introduction 

The file containing the NUTS geographical information was retrieved from <https://ec.europa.eu/eurostat/web/gisco/geodata/statistical-units/territorial-units-statistics>, I will be using the NUTS 2024 file in `shp` file format, you can download from the site. The NUTS legislation is changed periodically so it is possible to have a more recent year depending on when you access the website. 

- NUTS 1: major socio-economic regions
- NUTS 2: basic regions for the application of regional policies
- NUTS 3: small regions for specific diagnoses.

Load necessary libraries and the [NUTS_RG_20M_2024_3035.shp.zip](https://github.com/user-attachments/files/18375882/NUTS_RG_20M_2024_3035.shp.zip). Unzip the ´shp´ file before executing codes, alternatively you can download the map file from the repository.

```{r, warning = FALSE, message = FALSE}
library(sf)
library(ggplot2)

# Load the shapefile (adjust the path as needed)
shapefile_path <- "NUTS_RG_20M_2024_3035.shp"
nuts_data <- st_read(shapefile_path, quiet = TRUE)
```



```{r}
# Manually set zoomed-in limits for Europe
zoomed_bbox <- c(xmin = 1200000, xmax = 7000000, ymin = 1000000, ymax = 6500000)

# Plot the NUTS2 regions using color block
ggplot(data = nuts_data) +
  geom_sf(aes(fill = LEVL_CODE), color = "black", size = 0.1) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  coord_sf(xlim = c(zoomed_bbox["xmin"], zoomed_bbox["xmax"]),
           ylim = c(zoomed_bbox["ymin"], zoomed_bbox["ymax"]),
           crs = st_crs(nuts_data)) +
  labs(title = "NUTS Regions", x = "Longitude", y = "Latitude", fill = "LEVL_CODE") +
  theme_minimal()
```


