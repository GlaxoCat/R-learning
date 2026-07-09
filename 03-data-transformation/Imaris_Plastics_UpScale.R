library(ggprism)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(here)
library(ggsignif)
library(ggpubr)

process_image_data <- function(image_prefix) {
  # here() tells R: "Start at the root project directory,
  # look inside 'Imaris_Data', and find this file."
  cluster_center <- read_excel(
    here(
      "Imaris_Data",
      paste0(image_prefix, "_Cluster_Plastics_GFP_CenterIntensity.xlsx")
    ),
    skip = 1
  ) %>%
    select(ID, Center_Intensity = `Intensity Center`)

  cluster_max <- read_excel(
    here(
      "Imaris_Data",
      paste0(image_prefix, "_Cluster_Plastics_GFP_MaxIntensity.xlsx")
    ),
    skip = 1
  ) %>%
    select(ID, Max_Intensity = `Intensity Max`)

  cluster_mean <- read_excel(
    here(
      "Imaris_Data",
      paste0(image_prefix, "_Cluster_Plastics_GFP_MeanIntensity.xlsx")
    ),
    skip = 1
  ) %>%
    select(ID, Mean_Intensity = `Intensity Mean`)

  cluster_voxel <- read_excel(
    here(
      "Imaris_Data",
      paste0(image_prefix, "_Cluster_Plastics_GFP_Voxel.xlsx")
    ),
    skip = 1
  ) %>%
    select(ID, Voxel = `Number of Voxels`)

  cluster_tidy <- cluster_center %>%
    left_join(cluster_max, by = "ID") %>%
    left_join(cluster_mean, by = "ID") %>%
    left_join(cluster_voxel, by = "ID") %>%
    mutate(
      Image_ID = image_prefix,
      Spot_Category = "Cluster"
    )

  # --- Same Process for Non-cluster plastics ---
  noncluster_center <- read_excel(
    here(
      "Imaris_Data",
      paste0(image_prefix, "_NonCluster_Plastics_GFP_CenterIntensity.xlsx")
    ),
    skip = 1
  ) %>%
    select(ID, Center_Intensity = `Intensity Center`)

  noncluster_max <- read_excel(
    here(
      "Imaris_Data",
      paste0(image_prefix, "_NonCluster_Plastics_GFP_MaxIntensity.xlsx")
    ),
    skip = 1
  ) %>%
    select(ID, Max_Intensity = `Intensity Max`)

  noncluster_mean <- read_excel(
    here(
      "Imaris_Data",
      paste0(image_prefix, "_NonCluster_Plastics_GFP_MeanIntensity.xlsx")
    ),
    skip = 1
  ) %>%
    select(ID, Mean_Intensity = `Intensity Mean`)

  noncluster_voxel <- read_excel(
    here(
      "Imaris_Data",
      paste0(image_prefix, "_NonCluster_Plastics_GFP_Voxel.xlsx")
    ),
    skip = 1
  ) %>%
    select(ID, Voxel = `Number of Voxels`)

  noncluster_tidy <- noncluster_center %>%
    left_join(noncluster_max, by = "ID") %>%
    left_join(noncluster_mean, by = "ID") %>%
    left_join(noncluster_voxel, by = "ID") %>%
    mutate(Image_ID = image_prefix, Spot_Category = "Non cluster")

  # # ==========================================
  # # 3. SMASH TOGETHER INTO THE MASTER DATASET
  # # ==========================================
  master_tidy_spots <- bind_rows(cluster_tidy, noncluster_tidy)
}

# Read only the ID and the raw metric from each independent file
# List all your image prefixes
image_list <- c("image1", "image14", "image28")

# Read and combine everything automatically
final_table <- map_df(image_list, process_image_data)

# Save your multi-image master dataset
write_csv(final_table, "all_images_tidy_spots.csv")


# # ==========================================
# # Center Intensity (Quality)
# # ==========================================

ggplot(
  final_table,
  aes(x = Center_Intensity, fill = Spot_Category, shape = Image_ID)
) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Image_ID) +
  labs(
    title = "Center Intensity (Quality) of Cluster vs. NonCluster Plastics",
    x = "Center Intensity (Quality)",
    y = "Freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

# # ==========================================
# # Mean Intensity
# # ==========================================

ggplot(
  final_table,
  aes(x = Mean_Intensity, fill = Spot_Category, shape = Image_ID)
) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Image_ID) +
  labs(
    title = "Mean Intensity of Cluster vs. NonCluster Plastics",
    x = "Mean Intensity",
    y = "Freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

# # ==========================================
# # Max Intensity
# # ==========================================
ggplot(
  final_table,
  aes(x = Max_Intensity, fill = Spot_Category, shape = Image_ID)
) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Image_ID) +
  labs(
    title = "Max Intensity of Cluster vs. NonCluster Plastics",
    x = "Max Intensity",
    y = "Freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

# # ==========================================
# # Voxels
# # ==========================================
ggplot(
  final_table,
  aes(x = Voxel, fill = Spot_Category, shape = Image_ID)
) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Image_ID) +
  labs(
    title = "Voxels of Cluster vs. NonCluster Plastics",
    x = "Voxel",
    y = "Freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

# # ==========================================
# # Comparing Mean Intensities of plastics across different images, grouped by cluster and noncluster
# # ==========================================
ggplot(
  final_table,
  aes(x = Mean_Intensity, fill = Image_ID)
) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Spot_Category) +
  labs(
    title = "Mean Intensity of Spots across images",
    x = "Image",
    y = "Freq",
    color = "Image ID"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

# # ==========================================
# # This comment is a test for commit and ammend
# # ==========================================

# # ==========================================
# # Looking at noncluster plastic distribution only
# # ==========================================

final_table %>%
  filter(Spot_Category == "Non cluster") %>%
  ggplot(aes(x = Mean_Intensity, fill = Image_ID)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Noncluster Plastics Distribution across Images",
    x = "Mean Intensity",
    y = "Freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")


# # ==========================================
# # Looking at noncluster plastic distribution with boxplot
# # ==========================================

final_table %>%
  filter(Spot_Category == "Non cluster") %>%
  ggplot(aes(x = Image_ID, y = Mean_Intensity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  labs(
    title = "Noncluster Plastics Distribution Across Images",
    x = "Mean Intensity",
    y = "Freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

# # ==========================================
# # Looking at noncluster plastic distribution with violin plot
# # ==========================================

final_table %>%
  filter(Spot_Category == "Non cluster") %>%
  ggplot(aes(x = Image_ID, y = Mean_Intensity)) +
  geom_violin(alpha = 0.7, quantile.linetype = 3L) +
  labs(
    title = "Noncluster Plastics Distribution Across Images",
    x = "Mean Intensity",
    y = "Freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

# # ==========================================
# # Looking at cluster plastic distribution with boxplot - center intensity (quality)
# # ==========================================

final_table %>%
  filter(Spot_Category == "Cluster") %>%
  ggplot(aes(
    x = Image_ID,
    y = Center_Intensity,
    fill = Image_ID,
    color = Image_ID
  )) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) + # width = how far apart individual spots are, alpha = how visible, size = how big
  labs(
    title = "Cluster Plastics Distribution across Images",
    x = "Quality",
    y = "freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

# # ==========================================
# # Looking at cluster plastic distribution with boxplot - Voxels
# # ==========================================

final_table %>%
  filter(Spot_Category == "Cluster") %>%
  ggplot(aes(x = Image_ID, y = Voxel, fill = Image_ID, color = Image_ID)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  labs(
    title = "Cluster Plastics Distribution across Images",
    x = "Voxel",
    y = "freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")


final_table %>%
  ggplot(aes(x = Image_ID, y = Voxel, fill = Image_ID, color = Image_ID)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  facet_wrap(~Spot_Category) +
  labs(
    title = "Plastics Distribution across Images",
    x = "Voxel",
    y = "freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")


# # ==========================================
# # Image 1 Voxel Comparison Cluster vs. Noncluster
# # ==========================================

final_table %>%
  filter(Image_ID == "image1") %>%
  ggplot(aes(x = Spot_Category, y = Voxel)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  labs(
    title = "Cluster vs. NonCluster Voxel in Image 1",
    x = "Spot Category",
    y = "Voxel"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")


# # ==========================================
# # Cluster plastics comparison across images WITH SIGNIFICANCE BARS
# # ==========================================

image_comparisons <- list(
  c("image14", "image28"),
  c("image1", "image14"),
  c("image1", "image28")
)

final_table %>%
  filter(Spot_Category == "Cluster") %>%
  ggplot(aes(
    x = Image_ID,
    y = Center_Intensity,
    fill = Image_ID,
    color = Image_ID
  )) +
  #stat_boxplot(geom = "errorbar", width = 0.2, lwd = 0.8, color = "black") +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) + # width = how far apart individual spots are, alpha = how visible, size = how big
  labs(
    title = "Cluster Plastics Distribution across Images",
    x = "Quality",
    y = "freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light") +
  theme(legend.position = "none") + # This removes the legend altogether

  stat_compare_means(
    comparisons = image_comparisons,
    method = "t.test",
    label = "p.signif"
  )
