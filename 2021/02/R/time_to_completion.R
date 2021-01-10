library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)
library(colorspace)
library(extrafont)
library(rvest)

loadfonts(quiet = TRUE)


#' Source & dataset description: 
#' https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-01-05

tuesdata <- tidytuesdayR::tt_load(2021, week = 2)
transit_cost <- tuesdata$transit_cost
glimpse(transit_cost)

## DATA PREPARATION ===============================================

df <- transit_cost %>% 
  # drop non-relevant rows
  filter(!is.na(e)) %>% 
  mutate(country_name = countrycode::countrycode(country, origin = "ecb", destination = "country.name"),
         country_name = ifelse(country == "UK", "United Kingdom", country_name),
         end_year = na_if(end_year, "X"),
         end_year = as.numeric(end_year),
         # this causes a few NAs since the column contains text for future projects
         start_year = as.numeric(start_year),
         real_cost = as.numeric(real_cost),
         tunnel_per = str_remove_all(tunnel_per, "%") %>% as.numeric / 100,
         duration = end_year - start_year
  ) %>% 
  mutate(continent = countrycode::countrycode(country_name, 
                                              origin = "country.name", 
                                              destination = "continent")) 


count(df, country_name, sort = TRUE)
count(df, desc(end_year))


## DATA VIZ ====================================================

## set custom ggplot theme
custom_theme <- function() {
  theme_minimal(base_family = "Barlow") +
    theme(panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = 0.1, color = "grey89"),
          text = element_text(color = "grey30", lineheight = 1.2),
          plot.title = element_markdown(family = "Source Sans Pro SemiBold", 
                                        color = "black", 
                                        lineheight = 1.2),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 6, lineheight = 1.1),
          axis.title.x = element_text(hjust = 0),
          axis.text.y = element_markdown(size = 7),
          plot.background = element_rect(color = NA, fill = "grey98"),
          plot.margin = margin(t = 8, l = 8, r = 8, b = 8),
          legend.position = "top",
          legend.justification = "left")
}
# set as default theme
theme_set(custom_theme())

# load color palettes
source("../../color_palettes.R")


df %>% 
  # discard projects with missing duration and incomplete projects
  filter(!is.na(duration) & end_year <= 2020) %>% 
  # select top 10 projects based on duration
  slice_max(duration, n = 10, with_ties = FALSE) %>%
  select(country_name, city, continent, line, start_year, end_year, duration) %>% 
  # create a label for the project
  mutate(line_label = str_to_upper(paste(city, line))) %>% 
  ggplot(aes(reorder(line_label, duration))) +
  # colored point elements in the background 
  geom_point(aes(y = start_year, col = continent), size = 5, show.legend = FALSE) +
  geom_point(aes(y = end_year, col = continent), size = 5, show.legend = FALSE) +
  # colored segments to create background lines for GANTT chart
  geom_segment(aes(xend = line_label, y = start_year, yend = end_year, col = continent), 
               size = 4) +
  # overlay with white segments of same length
  geom_segment(aes(xend = line_label, y = start_year, yend = end_year), 
               col = "white", size = 2) +
  # overlay with white points
  geom_point(aes(y = start_year), size = 3, col = "white") +
  geom_point(aes(y = end_year), size = 3, col = "white") +
  # add labels
  geom_text(aes(y = 2025, label = paste(duration, "yrs")), 
            color = "grey54", family = "Barlow") +
  scale_y_continuous() +
  scale_color_beatles_lego() +
  coord_flip() +
  labs(title = "Rail infrastructure projects with the longest time to completion",
       subtitle = "",
       caption = "Projects completed until 2020.<br>
       Visualization: @4nsgarW. Data: Transit Costs Project",
       x = NULL, y = NULL, col = NULL) +
  theme(axis.text.y = element_markdown(size = 12, hjust = 0, family = "Barlow SemiBold", color = "grey47"),
        legend.position = "bottom",
        legend.justification = "right",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, color = "grey89"),
        plot.title = element_markdown(size = 14),
        plot.caption = element_markdown(hjust = 1),
        plot.margin = margin(t = 12, l = 12, r = 8, b = 8))

ggsave("plots/construction_duration.png", type = "cairo", dpi = 200, width = 8, height = 6)
