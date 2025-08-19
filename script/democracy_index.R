# Project Information =========================================================
# Project:       Democracy
# Script:        03_Demokratie.R
# Description:   Analysis of the democracy index from The Economist
# Author:        Burak Bozkurt
# Date created:  22 July 2024
# Last modified: 19 August 2025

# 1 PREPARATION ===============================================================

## 1.1 Load Libraries ==========================================================
library(tidyverse)
# library(rvest) # For exctracting elements from html
# library(xml2) # For reading html
library(readxl)
library(janitor)
library(maps)
library(countrycode)

## 1.2 Define Functions ========================================================

# Theme setting for world map
world_map_theme <- theme_minimal() +
    theme(
      text = element_text(family = "serif"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    )

# Theme setting for panel graphs
panel_theme <- theme_bw() +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    panel.grid.minor = element_blank() # removes unnecessary grids between the years
  )

# Creates panel graphs
panel_graph <- function(country) {
  
  if (!is_character({{ country }})) {
    stop("\n Input has to be a country from the democracy index!")
  }
  
  dem.index.long |> 
    filter(country %in% {{ country }}) |> 
    mutate(
      country = fct_reorder(country, score, .desc = TRUE) # sort factor levels of region by score
    ) |> 
    ggplot(aes(x = year, y = score, col = country, linetype = country, shape = country)) +
    geom_rect(
      data = data.frame(
        xmin = -Inf, 
        xmax = Inf,
        ymin = c(0, 4, 6, 8),
        ymax = c(4, 6, 8, 10),
        regime = c(
          "Authoritarian" = "red", 
          "Hybrid regime" = "orange", 
          "Flawed democracy" = "green", 
          "Full democracy" = "darkgreen"
        )
      ),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = regime), alpha = .15,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    scale_fill_identity( # necessary for using the colors defined above
      guide = "legend",
      labels = c("Full democracy", "Flawed democracy", "Hybrid regime", "Authoritarian")
    ) + 
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    panel_theme +
    geom_hline(
      yintercept = c(4, 6, 8), # for including the dotted horizontal lines
      linetype = "dotted"
    ) +
    labs(
      x = "Year", 
      y = "Score", 
      title = paste0("Democracy Index - Scores (2006 - ", most_recent_year, ")"),
      caption = "Source: The Economist"
    ) +
    scale_y_continuous(
      breaks = seq(0, 10, 1), 
      expand = c(0,0) # y axis strictly begins at 0 and ends at 10
    ) +
    scale_x_continuous(breaks = c(2006, 2008, 2010:most_recent_year))
}

## 1.3 Scrape and save data ====================================================

# wikipedia.html <- read_html("https://en.wikipedia.org/wiki/The_Economist_Democracy_Index#List_by_country") 

# dem.index.total <- wikipedia.html |> 
#  html_elements(".wikitable") |> # read table from Wikipedia
#  html_table() # save all tables as lists

# dem.index <- as.data.frame(dem.index.total[4])  # select the only relevant table from the list

# writexl::write_xlsx(dem.index, "democracy_index.xlsx")


## 1.4 Read data ===============================================================

### 1.4.1 Read democracy data ==================================================

dem.index <- read_excel("data/democracy_index.xlsx")

### 1.4.2 Load world map data ==================================================

world_map <- map_data("world")

## 1.5 Tidy up data ============================================================

### 1.5.1 Tidy up democracy data ===============================================

# get rid of rank column (useless information)
dem.index <- dem.index |> select(!contains("rank"))

# clean up column names with janitor package
dem.index <- clean_names(dem.index)

# convert character variables into factors
dem.index <- dem.index |> 
  mutate(
    country = fct(country),
    region = fct(region),
    regime_type = fct(regime_type)
  )

# Replace country names by official names (makes it easier to visualize data in the world map)
dem.index$country <- dem.index$country |> 
  countrycode(origin = "country.name", destination = "country.name.en")

### 1.5.2 Tidy up world map data ===============================================

# Add column
world_map$region2 <- world_map$region

# Replace country names in the world map data set with names from dem.index
replacements <- c(
  "USA"                          = "United States",
  "Puerto Rico"                  = "United States",
  "Republic of Congo"            = "Congo - Brazzaville",
  "Democratic Republic of the Congo" = "Congo - Kinshasa",
  "UK"                           = "United Kingdom",
  "Swaziland"                    = "Eswatini",
  "French Guiana"                = "France",
  "Czech Republic"               = "Czechia",
  "Ivory Coast"                  = "Côte d’Ivoire",
  "Myanmar"                      = "Myanmar (Burma)",
  "Bosnia and Herzegovina"       = "Bosnia & Herzegovina",
  "Palestine"                    = "Palestinian Territories",
  "Trinidad"                     = "Trinidad & Tobago"
)

world_map$region2 <- str_replace_all(world_map$region2, replacements)

# 2 WORLD MAP VISUALIZATIONS ===================================================

# Join world_map and dem.index
world_map <- full_join(world_map, dem.index, join_by(region2 == country))

## 2.1 Countries by region =====================================================

# Plot world map (Region)
ggplot(world_map, aes(x = long, y = lat, group = group, fill = region.y)) +
  geom_polygon(color = "black") +
  labs(
    title = "World Regions according to the Economist",
    caption = "Source: The Economist"
  ) +
  world_map_theme

## 2.2 Countries by regime type ================================================

# extract the most recent year for which we have observations
most_recent_year <- dem.index |> select(starts_with("x")) |> colnames() |> sub(pattern = "x", replacement = "") |> first()

# Plot World Map (regime type)
ggplot(world_map, aes(x = long, y = lat, group = group, fill = regime_type)) +
  geom_polygon(color = "black") +
  labs(
    title = paste0("Countries by Regime Type (", most_recent_year, ")"),
    caption = "Source: The Economist"
    ) +
  world_map_theme +
  scale_fill_manual(values = c("darkgreen", "green", "orange", "red"))

# 3 DEMOCRACY DEVELOPMENT BY COUNTRY (2006 - 2024) =============================

## 3.1 Transform table =========================================================

dem.index.long <- dem.index |> 
  pivot_longer(                   # convert columns (years) into rows
    cols = starts_with("x"),      # select all columns starting with "x" (x2023, x2022, ...)
    names_to = "year",            # new column "year" should contain all years
    values_to = "score")         # values in cells should appear in the column "score"

dem.index.long <- dem.index.long |> 
  mutate(
    year = parse_number(year),    # extracts numbers only from the column "year" (the "x" are excluded)
    regime_type = case_when(      # regime type is adapted to the number of score a country received
      score > 8 ~ "Full democracy",                  
      score > 6 & score <= 8 ~ "Flawed democracy",
      score > 4 & score <= 6 ~ "Hybrid regime",
      score <= 4 ~ "Authoritarian"
    )
  )

## 3.2 Average democracy scores by region ====================================== 

avg.score.region <- dem.index.long |> 
  group_by(region) |> 
  group_by(year, .add = TRUE) |>
  summarize(
    avg.score = mean(score)
  )

avg.score.region <- avg.score.region |> 
  ungroup() |> # necessary for reordering the levels of region
  mutate(
    region = fct_reorder(region, avg.score, .desc = TRUE) # reorder levels of region by avg.score
  )

avg.score.region |> 
  ggplot(aes(
    x = year, 
    y = avg.score, 
    col = region)
  ) +
  geom_rect(
    data = data.frame(
      xmin = -Inf, 
      xmax = Inf,
      ymin = c(0, 4, 6, 8),
      ymax = c(4, 6, 8, 10),
      regime = c(
        "Authoritarian" = "red", 
        "Hybrid regime" = "orange", 
        "Flawed democracy" = "green", 
        "Full democracy" = "darkgreen"
      )
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = regime), alpha = .15,
    inherit.aes = FALSE,
  ) +
  scale_fill_identity( # necessary for using the colors defined above
    guide = "legend",
    labels = c("Full democracy", "Flawed democracy", "Hybrid regime", "Authoritarian")
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Democracy Index - Average Score by Region",
    caption = "Source: The Economist"
  ) +
  panel_theme +
  geom_hline(
    yintercept = c(4, 6, 8), # for including the dotted horizontal lines
    linetype = "dotted"
  ) +
  scale_x_continuous(breaks = c(2006, 2008, 2010:most_recent_year)) +
  scale_y_continuous(
    breaks = seq(0, 10, 1), 
    expand = c(0,0) # y axis strictly begins at 0 and ends at 10
  )

## 3.3 Development of democracy in specific countries ==========================

# Countries with positive trends
panel_graph(country = c("Afghanistan", "Mali", "Nicaragua", "Russia", "Venezuela")) +
  scale_color_brewer(palette = "Dark2")

# Countries with negative trends
panel_graph(country = c("Angola", "Bhutan", "Malaysia", "Armenia")) +
  scale_color_brewer(palette = "Set1")
