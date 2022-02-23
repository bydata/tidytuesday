library(tidyverse)
library(ggtext)
library(here)
library(geofacet)
library(lubridate)

base_path <- here::here("2022", "08")
tuesdata <- tidytuesdayR::tt_load("2022-02-22")
freedom <- tuesdata$freedom

# Remove UK from EU28 grid
eu27_grid <- subset(eu_grid1, name != "United Kingdom")
grid_preview(eu27_grid)

# Freedom categories
count(freedom, CL)
count(freedom, PR)

# List of EU country names 
eu_countries <- eu27_grid$name

## Geo-facetted plot -----------------------------------------------------------
p <- freedom %>% 
  # align country names
  mutate(country = ifelse(country == "Czechia", "Czech Republic", country),
         date = as_date(paste0(year, "-01-01"))) %>% 
  pivot_longer(cols = c(CL, PR), names_to = "indicator") %>% 
  filter(country %in% eu_countries)  %>% 
  ggplot(aes(year, value)) +
  geom_line(aes(col = indicator), alpha = 0.7, size = 1, show.legend = FALSE) +
  
  # Year labels only in one facet
  geom_text(
    data = data.frame(
      x = c(1995, 2020),
      y = c(5, 5),
      country = rep("Sweden", 2)
    ),
    aes(x, y, label = x),
    angle = 90, hjust = 0.5, size = 2.25, 
    col = "grey79", family = "Raleway", inherit.aes = FALSE
  ) + 
  # Reading help for values
  geom_text(
    data = data.frame(
      x = 1995,
      y = c(2.5, 6),
      label = c("\U2191 More freedom", "\U2193 Less freedom"),
      country = rep("Finland", 2)
    ),
    aes(x, y, label = label),
    hjust = 0, size = 2.25, col = "grey79", family = "Raleway",
    inherit.aes = FALSE
  ) + 
  
  scale_x_continuous(breaks = c(1995, 2020)) +
  scale_y_reverse(limits = c(7, 1)) +
  scale_color_manual(values = c("CL" = "yellow2", "PR" = "skyblue")) +
  facet_geo(~country, grid = eu27_grid, move_axes = FALSE) +
  labs(
    title = "Freedom in Europe",
    subtitle = "Freedom House <b style='color:yellow2'>Civil Liberties</b> and 
    <b style='color:skyblue'>Political Rights</b> indicators from 1995 to 2020",
    caption = "**Source:** Freedom House, geo facets: {geofacet}, shapefile: Natural Earth #TidyTuesday | **Visualization:** Ansgar Wolsing"
  ) +
  theme_bw(base_family = "Raleway") +
  theme(
    # plot.background = element_rect(color = NA, fill = "grey8"),
    plot.background = element_rect(color = NA, fill = NA),
    panel.background = element_rect(color = NA, fill = alpha("grey12", 0.7)),
    panel.border = element_blank(),
    strip.background = element_rect(color = NA, fill = "grey28"),
    strip.text = element_text(color = "white", size = 6),
    axis.text = element_blank(),
    # axis.text.x = element_text(color = "grey82"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey57", size = 0.1, linetype = "dotted"),
    axis.ticks = element_blank(),
    text = element_text(color = "grey89"),
    legend.background = element_rect(color = NA, fill = NA),
    plot.title = element_markdown(family = "Roboto Condensed", face = "bold", size = 24),
    plot.subtitle = element_markdown(margin = margin(b = 16)),
    plot.caption = element_markdown(size = 7),
    plot.margin = margin(6, 6, 6, 6)
  )
ggsave(here(base_path, "plots", "map_freedom_eu.png"), dpi = 300,
        width = 6, height = 6.5)


## EU shape for background effect ----------------------------------------------
eu <- rnaturalearth::ne_countries(scale = 110, country = eu_countries, returnclass = "sf")
eu_bg <- ggplot(eu) + 
  geom_sf(color = "grey2", size = 0.2, fill = "grey4") +
  coord_sf(xlim = c(-10, 30), ylim = c(32, 70)) +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "grey8")
  )
ggsave(here(base_path, "plots", "shape_eu.png"), dpi = 300,
      width = 6, height = 6.5)

## COMBINE PLOTS ---------------------------------------------------------------
library(magick)
img_shape_eu <- image_read(here(base_path, "plots", "shape_eu.png"))
img_plot <- image_read(here(base_path, "plots", "map_freedom_eu.png"))

img_combined <- image_composite(img_shape_eu, img_plot) 
image_write(img_combined, here(base_path, "plots", "map_freedom_eu_with_bg.png"))  

