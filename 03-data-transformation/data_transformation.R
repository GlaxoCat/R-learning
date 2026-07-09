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


# Filtering flights that left on January 1, regardless of the year

flights |>
  filter(month == 1 & day == 1)

# Filtering flights that left either in January or Februrary

flights |>
  filter(month == 1 | month == 2)

# Shorcut when combining |> pipe and OR statement ( | )

flights |>
  filter(month %in% c(1, 2))

jan1_flights <- flights |>
  filter(month == 1 & day == 1)

flights |>
  select(year, month, day, dep_time) |>
  arrange(year, month, day, dep_time)

flights |>
  arrange(year, month, day, dep_time)

# following code sorts by the departure time,
# which is spread over four columns. We get the earliest years first, then within a year, the earliest months, etc.
# Then, this will group the following variables (year, month, day, dep_time) and output

flights |>
  select(year, month, day, dep_delay) |>
  arrange(desc(dep_delay))


flights |>
  arrange(desc(dep_delay)) |>
  group_by(dep_delay) |>
  summarize()

# Find all unique origin and destination pairs
# This will return only columns that are included as arguments in distinct()
# Unless you do .keep_all = TRUE

flights |>
  distinct(origin, dest)


flights |>
  count(origin, dest, sort = TRUE) #having sort set to TRUE will automatically arrange in descending order of the # occurances

#> Exercises
#> 1. In a single pipeline for each condition, find all flights that meet the condition:
# Had an arrival delay of two or more hours
# Flew to Houston (IAH or HOU)
# Were operated by United, American, or Delta
# Departed in summer (July, August, and September)
# Arrived more than two hours late but didn’t leave late
# Were delayed by at least an hour, but made up over 30 minutes in flight

flights |>
  filter(
    arr_delay > 120 |
      dest %in% c("IAH", "HOU") |
      carrier %in% c("UA", "AA", "DL") |
      month %in% c(7, 8, 9) |
      (arr_delay > 120 & dep_delay <= 0) |
      (dep_delay >= 60 & (dep_delay - arr_delay) > 30)
  )


# Sort flights to find flights with longest departure delays. Find flights that left earliest in the morning

flights |>
  arrange(desc(dep_delay))

# # ==========================================
# # Test to work on learning how to properly stage and commit changes this is task 1!
# # ==========================================
