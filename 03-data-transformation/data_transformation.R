library(ggprism)
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(dplyr)

flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

flights |>
  filter(dep_delay > 120) |>
  group_by(year, month, day, origin) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

flights |>
  filter(dep_delay > 120) |>
  group_by(year, month, day, origin) |>
  summarize()

monthly_delays <- flights |>
  filter(dep_delay > 120) |>
  group_by(year, month, origin) |>
  summarize(
    mean_arr_delay = mean(arr_delay, na.rm = TRUE),
    .groups = "drop"
  )

# Perfect structure for a line graph comparison!
ggplot(monthly_delays, aes(x = month, y = mean_arr_delay, color = origin)) +
  # Draw the lines connecting the months
  geom_line(linewidth = 1) +
  # Add individual dots on each month for readability
  geom_point(size = 2.5) +
  # Custom scale formatting
  scale_x_continuous(
    name = "Month of the Year",
    breaks = 1:12,
    labels = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    )
  ) +
  scale_y_continuous(
    name = "Average Arrival Delay (minutes)"
  ) +
  scale_color_brewer(
    name = "Airport",
    palette = "Set1"
  ) +
  # Apply clean title formatting
  labs(
    title = "2013 Monthly Average Arrival Delays",
    subtitle = "Data isolated for flights with departure delays > 120 minutes"
  ) +
  # Apply a clean theme layout
  theme_prism(base_size = 14) +
  theme(legend.title = element_text())


# want to look at the average air speed of flights departing from diff NY airports

plane_traffic_speeds <- flights |>
  filter(air_time > 300) |>
  mutate(speed = distance / air_time * 60) |>
  group_by(year, month, origin) |>
  summarize(
    mean_speed = mean(speed, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plane_traffic_speeds, aes(x = month, y = mean_speed, color = origin)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    name = "Month",
    breaks = 1:12,
    labels = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    )
  ) +
  scale_y_continuous(
    name = "Average Air Speed (MPH)"
  ) +
  labs(
    title = "Long-haul Flight Speeds by Departure Airport",
    subtitle = "Flights with air times > 5 hours"
  ) +
  theme_prism(base_size = 14) +
  theme(legend.title = element_text())
