library(ggprism)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

# ==========================================
# 1. PROCESS THE CLUSTER PLASTICS
# ==========================================

# Read only the ID and the raw metric from each independent file
cluster_center <- read_excel(
  "image14_Cluster_Plastics_GFP_CenterIntensity.xlsx"
) %>%
  select(ID, Center_Intensity = `Intensity Center`)

cluster_max <- read_excel(
  "image14_Cluster_Plastics_GFP_MaxIntensity.xlsx",
  skip = 1
) %>%
  select(ID, Max_Intensity = `Intensity Max`)

cluster_mean <- read_excel(
  "image14_Cluster_Plastics_GFP_MeanIntensity.xlsx",
  skip = 1
) %>%
  select(ID, Mean_Intensity = `Intensity Mean`)

cluster_voxel <- read_excel(
  "image14_Cluster_Plastics_GFP_Voxel.xlsx",
  skip = 1
) %>%
  select(ID, Voxel = `Number of Voxels`)

cluster_tidy <- cluster_center %>%
  left_join(cluster_max, by = "ID") %>%
  left_join(cluster_mean, by = "ID") %>%
  left_join(cluster_voxel, by = "ID") %>%
  mutate(
    Image_ID = "Image_14",
    Spot_Category = "Cluster"
  )

# Same Process for Non-cluster plastics

noncluster_center <- read_excel(
  "image14_NonCluster_Plastics_GFP_CenterIntensity.xlsx",
  skip = 1
) %>%
  select(ID, Center_Intensity = `Intensity Center`)

noncluster_max <- read_excel(
  "image14_NonCluster_Plastics_GFP_MaxIntensity.xlsx",
  skip = 1
) %>%
  select(ID, Max_Intensity = `Intensity Max`)

noncluster_mean <- read_excel(
  "image14_NonCluster_Plastics_GFP_MeanIntensity.xlsx",
  skip = 1
) %>%
  select(ID, Mean_Intensity = `Intensity Mean`)

noncluster_voxel <- read_excel(
  "image14_NonCluster_Plastics_GFP_Voxel.xlsx",
  skip = 1
) %>%
  select(ID, Voxel = `Number of Voxels`)

noncluster_tidy <- noncluster_center %>%
  left_join(noncluster_max, by = "ID") %>%
  left_join(noncluster_mean, by = "ID") %>%
  left_join(noncluster_voxel, by = "ID") %>%
  mutate(
    Image_ID = "Image_14",
    Spot_Category = "Non cluster"
  )

# # ==========================================
# # 3. SMASH TOGETHER INTO THE MASTER DATASET
# # ==========================================
master_tidy_spots <- bind_rows(cluster_tidy, noncluster_tidy)

print(master_tidy_spots)
write_csv(master_tidy_spots, "image14_tidy_spots.csv")

final_table <- read_csv("image14_tidy_spots.csv")

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
  labs(
    title = "Voxels of Cluster vs. NonCluster Plastics",
    x = "Voxel",
    y = "Freq"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

final_table %>%
  filter(Spot_Category == "Cluster") %>%
  filter(Mean_Intensity < 150)

final_table %>%
  filter(Spot_Category == "Non cluster") %>%
  filter(Mean_Intensity < 150)
