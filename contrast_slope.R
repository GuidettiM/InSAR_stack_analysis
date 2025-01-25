# This R program calculates and plots the Contrast based on the Theory of Evidence of Weights (WOE) by Bonham-Carter (1994), applied to the slope of several polygons in a large area.
# You only need a shapefile with your desired polygons and a slope raster of the area (be sure they both have the same cohordinate system)

# Load necessary libraries
library(raster)
library(sf)
library(dplyr)
library(ggplot2)

# Step 1: Load the slope raster and shapefile
slope_raster <- raster("C:/Tesi_GIS/statistiche/deformazione_lito/crop/slope_5m_merged_crop5.tif")# Replace with your slope raster file path

#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_bl_blp_32N.shp") # Replace with your shapefile path
#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_bp_dm_32N.shp") 
#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_cs_a_32N.shp") 
#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_Da_32N.shp") 
#polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/lito_dsc_dol_32N.shp") 

polygons <- st_read("C:/Tesi_GIS/statistiche/deformazione_lito/lito_accorpata/deformazione_32N.shp") # Accorpati


# Step 2: Classify slope values into specified ranges
# Define slope breaks and labels
slope_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
slope_labels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", ">35")

# Extract slope values
slope_values <- values(slope_raster)

# Classify slope values
slope_classes <- cut(
  slope_values,
  breaks = slope_breaks,
  labels = slope_labels,
  include.lowest = TRUE,
  right = FALSE
)

# Replace raster values with classified slope values
slope_raster <- setValues(slope_raster, as.integer(slope_classes))

# Step 3: Mask the raster using the shapefile
IDS_1 <- mask(slope_raster, polygons)             # Areas inside polygons
IDS_0 <- mask(slope_raster, polygons, inverse = TRUE) # Areas outside polygons

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
all_classes <- data.frame(class = 1:length(slope_labels)) # Classes for slope ranges
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

# Step 7: Plot the results in a histogram

# Add slope labels to the results data frame
results_df <- results %>%
  mutate(
    slope_range = factor(class, levels = 1:length(slope_labels), labels = slope_labels) # Map numeric classes to slope ranges
  )

# Plot the results
ggplot(results_df, aes(x = slope_range, y = Contrast)) +
  geom_col(fill = "dodgerblue", width = 0.8) + 
  geom_hline(yintercept = 0, color = "black", linetype = "longdash", linewidth = 0.3) + # Add Contrast = 0 line
  scale_y_continuous(limits = c(-5.7, 1.2)) +
  labs(
    title = "IDS accorpati",
    x = "Pendenza (°)",
    y = "Contrasto"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for better readability
  )

# PDF = 4 in x 4.5; JPEG = 500 x 550
