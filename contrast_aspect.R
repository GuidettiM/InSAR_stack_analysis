# This R program calculates and plots the Contrast based on the Theory of Evidence of Weights (WOE) by Bonham-Carter (1994), applied to the aspect of several polygons in a large area.
# You only need a shapefile with your desired polygons and an aspect raster of the area (be sure they both have the same cohordinate system).

# Load necessary libraries
library(raster)
library(sf)
library(dplyr)
library(ggplot2)

# Step 1: Load the aspect raster and shapefile
aspect_raster <- raster("C:/Tesi_GIS/statistiche/deformazione_lito/crop_aspect/aspect_5m_merged.tif") # Replace with your raster file path

polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_bl_blp_32N.shp") # Replace with your shapefile path
#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_bp_dm_32N.shp") 
#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_cs_a_32N.shp") 
#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_Da_32N.shp") 
#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_dsc_dol_32N.shp") 

#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/deformazione_32N.shp") # Accorpati


# Normalize aspect values directly on the raster, keeping NA values intact
aspect_values <- values(aspect_raster) # Extract all values, including NA
aspect_values_wrapped <- ifelse(
  !is.na(aspect_values) & aspect_values >= 337.5,  # Normalize only non-NA values >= 337.5
  aspect_values - 360,
  aspect_values
)

# Step 2: Classify all aspect values into 8 classes
aspect_classes <- cut(
  aspect_values_wrapped,
  breaks = c(-22.5, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5),
  labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
  include.lowest = TRUE,
  right = FALSE
)

# Replace raster values with classified aspect values
aspect_raster <- setValues(aspect_raster, as.integer(aspect_classes))

#plot(aspect_raster)

# Step 3: Mask the raster using the shapefile
IDS_1 <- mask(aspect_raster, polygons)             # Areas inside polygons
IDS_0 <- mask(aspect_raster, polygons, inverse = TRUE) # Areas outside polygons

#plot(IDS_1)
#plot(IDS_0)

# Step 4: Count pixels in each class for IDS_1 and IDS_0
# Function to count pixels in each class
count_pixels <- function(raster) {
  freq(raster) %>%
    as.data.frame() %>%
    rename(class = value, count = count)
}

# Count pixels
ids1_counts <- count_pixels(IDS_1)
ids0_counts <- count_pixels(IDS_0)

# Ensure all classes are present, even if counts are 0
all_classes <- data.frame(class = 1:8) # 8 classes (N to NW)
ids1_counts <- merge(all_classes, ids1_counts, by = "class", all.x = TRUE)
ids0_counts <- merge(all_classes, ids0_counts, by = "class", all.x = TRUE)
ids1_counts[is.na(ids1_counts)] <- 0
ids0_counts[is.na(ids0_counts)] <- 0

# Step 5: Calculate Npix values
ids1_counts <- ids1_counts %>%
  mutate(Npix1 = count,
         Npix2 = sum(count) - Npix1)

ids0_counts <- ids0_counts %>%
  mutate(Npix3 = count,
         Npix4 = sum(count) - Npix3)

# Step 6: Calculate weights of evidence and contrast
results <- ids1_counts %>%
  select(class, Npix1, Npix2) %>%
  left_join(ids0_counts %>% select(class, Npix3, Npix4), by = "class") %>%
  mutate(
    W_plus = log((Npix1 / (Npix1 + Npix2)) / (Npix3 / (Npix3 + Npix4))),
    W_minus = log((Npix2 / (Npix1 + Npix2)) / (Npix4 / (Npix3 + Npix4))),
    Contrast = W_plus - W_minus
  )

# Print results
print(results)

# Step 7: plot the results in an histogram

# Add direction labels to the results data frame
direction_labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

# Map numeric class values to direction labels
results_df <- results %>%
  mutate(
    direction = factor(class, levels = 1:8, labels = direction_labels) # Add direction column
  )

# Plot the results with direction labels
ggplot(results_df, aes(x = direction, y = Contrast)) +
  geom_col(fill = "dodgerblue", width = 0.8) + 
  geom_hline(yintercept = 0, color = "black", linetype = "longdash", linewidth = 0.3) + # Add Contrast = 0 line
  scale_y_continuous(limits = c(-1.4, 1.4)) +
  labs(
    title = "Bl-Blp",
    x = "Direzione",
    y = "Contrasto"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    #axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for better readability
  )

# PDF = 4 in x 4.5; JPEG = 500 x 550


# Optional: Export the masked rasters
writeRaster(IDS_1, "IDS_1.tif", overwrite = TRUE)
writeRaster(IDS_0, "IDS_0.tif", overwrite = TRUE)

