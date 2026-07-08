library(tidyverse)
library(palmerpenguins)
library(ggprism)
library(camcorder)
library(readxl)
library(plotly)

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_prism(palette = "magma") + #> Changes discrete color palette
  theme_prism(base_size = 14) + #> Change background to white, add thick black axis lines, shift tick outwards
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper Length (mm)",
    y = "Body mass (g)",
    color = "Species",
    shape = "Species"
  )


#> Break for the second plot
#>
#>

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  labs(
    title = "Bill lengths and depths by species",
    subtitle = "Comparisons of length and depth for Adelie, Chinstrap, and Gentoo Penguins",
    caption = "Data come from the palmerpenguins package",
    x = "Bill Length (mm)",
    y = "Bill Depth (mm)",
    color = "Species",
    shape = "Species"
  ) +
  geom_point(mapping = aes(color = species, shape = species)) +
  scale_color_prism(palette = "office") +
  theme_prism(base_size = 14)


#> Break for the second plot
#>
#>
#>

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  # 1. Put labs() right up front
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, \nand Gentoo Penguins"
  ) +
  geom_point(mapping = aes(color = bill_depth_mm), size = 2) +
  geom_smooth(color = "black", se = FALSE) +
  # 2. Define names inside continuous scales so they aren't erased
  scale_color_continuous(
    palette = "viridis",
    name = "Bill Depth (mm)"
  ) +
  scale_x_continuous(name = "Flipper Length (mm)") +
  scale_y_continuous(name = "Body mass (g)") +
  # 3. Add themes last
  theme_prism(base_size = 14) +
  theme(legend.title = element_text())
ggsave(
  filename = "01-data-visualization/plots/BodyMass_FlipperLength.png",
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

#> Break for the categorical variable blue
#>

catPlot <- ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()
print(catPlot)

catPrismPlot <- ggplot(penguins, aes(x = fct_infreq(species))) +
  labs(
    title = "Frequency of Each Species",
    x = "Species"
  ) +
  geom_bar() +
  scale_color_prism(palette = "office") +
  theme_prism(base_size = 14)
print(catPrismPlot)

#> Histogram Plot
#>

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(mapping = aes(color = species), binwidth = 200) +
  labs(
    title = "Body Mass of Penguins (g)",
    x = "Body Mass (g)",
    y = "Frequency"
  ) +
  theme_prism(base_size = 14)


ggplot(penguins, aes(x = body_mass_g)) +
  geom_density() +
  labs(
    title = "Body Mass of Penguins",
    x = "Body Mass",
    y = "Frequency",
  ) +
  theme_prism(base_size = 14)

ggplot(penguins, aes(y = species)) +
  geom_bar()

boxplot <- ggplot(
  penguins,
  aes(x = species, y = body_mass_g, fill = species, color = species)
) +
  geom_boxplot()

boxplot +
  theme_prism(base_size = 14) +
  theme(legend.title = element_text()) +
  scale_fill_prism(palette = "floral") +
  scale_color_prism(palette = "floral")

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Body Mass and Density of Different Penguin Species",
    x = "Body Mass (g)",
    y = "Density"
  ) +
  theme_prism(base_size = 14) +
  scale_fill_prism(palette = "prism_light") +
  scale_color_prism(palette = "prism_light")

ggsave(
  filename = "01-data-visualization/plots/Mass of Different Penguin Species.png",
  width = 8,
  height = 4,
  units = "in",
  dpi = 600
)

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

#> Facet Wrap
#>

ggplot(
  penguins,
  aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = island)
) +
  geom_point() +
  facet_wrap(~island)


p <- ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm, color = species)
) +
  geom_point()
theme_prism(base_size = 14)

# Convert it to an interactive web widget
ggplotly(p)
