# Import libraries
library(tidyverse)
library(here)
library(ggrepel)

# Load data
df_ow <- read_csv(here("assignment 1/data/ont-sa-characteristic-ow-cma.csv"))
# Check data
glimpse(df_ow)

# Create a date column by doing 1st month+year, so ggplot can read as date
# Keep city as name of the CMA without the code
df <- df_ow %>%
  mutate(
    date = make_date(year, month, 1),
    # The `cma` column looks like "505 - Ottawa--Gatineau".
    # This removes the leading number and dash so we are left with the city name.
    city = str_remove(cma, "^\\d+ - ")
  ) %>%
  # Drop observations before March 2018
  filter(date >= as.Date("2018-03-01"))

# Index each city such that level of December 2019 = 100%
# This makes all cities comparable: we see percent changes, rather than absolute change
# Formula: (cases in a given quarter / cases in Dec 2019) * 100
base_date <- as.Date("2019-12-01")

city_index <- df %>%
  group_by(city) %>% # do the calculation separately per city (each city has different baseline level)
  mutate(index = cases / cases[date == base_date] * 100) %>% #add an index column
  ungroup() #remove grouping to produce the visualization later

# Build pooled line
# Add up cases across all cities for each date, THEN index to Dec 2019.
# (This is different from averaging the city indexes: big cities count more.)
pooled <- df %>%
  group_by(date) %>%
  summarise(cases = sum(cases), .groups = "drop") %>%
  mutate(index = cases / cases[date == base_date] * 100)

# Produce the graph

# Pick which city to highlight. Change this name to highlight a different one.
# (Check the exact spelling with: unique(city_index$city))
highlight_city <- "Toronto"

# Split Toronto off from the other cities so we can draw it in its own colour
toronto <- city_index %>% filter(city == highlight_city)

p <- ggplot() +
  
  # Layer 1: a darker horizontal line at 100 (the December 2019 level)
  geom_hline(yintercept = 100, colour = "grey30", linewidth = 0.6) +
  
  # Layer 2: dashed vertical line marking the start of the pandemic (April 2020)
  geom_vline(xintercept = as.Date("2020-04-01"),
             linetype = "dashed", colour = "grey40") +
  
  # Another line marking the end of emergency benefits
  geom_vline(xintercept = as.Date("2022-05-01"),
             linetype = "dashed", colour = "grey40") +
  
  # Layer 3: one thin grey line per city (group = city = one line each)
  geom_line(data = city_index,
            aes(x = date, y = index, group = city),
            colour = "grey80", linewidth = 0.6) +
  
  # Layer 4: the thick red pooled line
  geom_line(data = pooled,
            aes(x = date, y = index),
            colour = "#C0606F", linewidth = 2) +
  
  # Layer 5: Toronto in blue, drawn last so it sits on top of everything else.
  # It is a bit thinner than the pooled line so both stay visible.
  geom_line(data = toronto,
            aes(x = date, y = index),
            colour = "grey50", linewidth = 1) +
  
  # X axis: label the start of each year, show only the year
  scale_x_date(
    breaks = as.Date(paste0(2019:2026, "-01-01")),
    date_labels = "%Y"
  ) +
  
  # Y axis: gridlines/labels at 80, 100, 120
  scale_y_continuous(breaks = c(80, 100, 120)) +
  
  # Title, axis labels and caption
  labs(
    title   = "How did COVID-19 change Ontario Works caseloads?",
    x       = NULL,                                    # no x-axis title
    y       = "Ontario Works cases (Dec 2019 = 100)",
    caption = "Source: Ontario Social Assistance Case Characteristics by CMA (data.ontario.ca). A case is one person or one family unit."
  ) +
  
  # Clean theme with only horizontal gridlines
  theme_minimal(base_size = 15) +
  theme(
    plot.title.position   = "plot",                    # align title with left edge of the image
    plot.caption.position = "plot",
    plot.title    = element_text(face = "bold", size = 22),
    plot.caption  = element_text(colour = "grey45", size = 10, hjust = 0),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.8),
    axis.text = element_text(colour = "grey30")
  )

# Show the chart in the RStudio Plots pane
print(p)

# Save as a PNG into the folder
ggsave(here("assignment 1/paper/ontario_works_line_chart.png"),
       p, width = 10, height = 6.5, dpi = 300, bg = "white")



