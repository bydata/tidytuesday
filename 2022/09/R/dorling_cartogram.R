library(tidyverse)
library(ggtext)
library(here)
library(sf)
library(cartogram)
# devtools::install_github("aljrico/gameofthrones")
library(gameofthrones) # color palette

base_path <- here::here("2022", "09")
tuesdata <- tidytuesdayR::tt_load("2022-03-01")
stations <- tuesdata$stations
colnames(stations) <- tolower(colnames(stations))
glimpse(stations)

#' Building upon the cartogram created by Benjamin Nowak
#' https://twitter.com/BjnNowak/status/1498658998341537796

#' Data Dictionary:
#' https://afdc.energy.gov/data_download/alt_fuel_stations_format

#' State motor vehicle registrations:
#' h/t https://twitter.com/jim_gruman/status/1498614682504077312
#' https://www.fhwa.dot.gov/policyinformation/statistics/2017/mv1.cfm

# Scrape the registration data
library(rvest)
url <- "https://www.fhwa.dot.gov/policyinformation/statistics/2017/mv1.cfm"
page <- read_html(url)
table_raw <- html_node(page, css = "table") %>% html_table() 
vehicles_states <- table_raw %>% 
  select(state = 1, total_auto = 4) %>% 
  filter(state != "STATE") %>% 
  mutate(total_auto = as.numeric(str_remove_all(total_auto, ",")),
         state = ifelse(state == "Dist. of Col.",
                        "District of Columbia",
                        str_remove(state, " \\(\\d+\\)")))


# Fuel type code
count(stations, fuel_type_code)
elec_stations <- filter(stations, fuel_type_code == "ELEC") 
dim(elec_stations)
elec_stations_states <- count(elec_stations, state, name = "elec_n")


# Get states geometries
states_sf <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
states_sf <- states_sf %>% 
  select(name, postal, geometry)

# Join geometry and stations dataframe
elec_stations_states_sf <- elec_stations_states %>% 
  inner_join(states_sf, by = c("state" = "postal")) %>% 
  st_as_sf() %>% 
  st_transform(crs = "epsg:3395")

elec_stations_states_vehicles_sf <- 
  elec_stations_states_sf %>% 
  inner_join(vehicles_states, by = c("name" = "state")) %>% 
  mutate(stations_per_100k_vehicles = elec_n / total_auto * 10^5)


# Create the geometry for the Dorling cartogram
dorling <- elec_stations_states_vehicles_sf %>% 
  filter(!name %in% c("Alaska", "Hawaii")) %>% 
  cartogram_dorling("stations_per_100k_vehicles")


## Annotations 

# Create labels for Alaska and Hawaii
labels_ak_hi <- elec_stations_states_vehicles_sf %>% 
  filter(name %in% c("Alaska", "Hawaii")) %>% 
  st_drop_geometry() %>% 
  mutate(label = glue::glue("\U2022 {state}: {round(stations_per_100k_vehicles, 1)}"))

# Total number of stations
total_stations_n <- nrow(elec_stations)
total_stations_n_fmt <- scales::number(total_stations_n, big.mark = ",")

plot_titles <- list(
  title = "Electric vehicle charging stations in the U.S.",
  subtitle = glue::glue("{total_stations_n_fmt} electric charging stations are available 
                        in the U.S. as of 2022. Vermont has the highest number of 
                        electric charging stations per 100,000 cars (any type of engine), with 148."),
  caption = "**Source:** U.S. Department of Transportation | 
  **Visualization**: Ansgar Wolsing | #TidyTuesday",
  fill = "Number of electric charging stations<br>per 100,000 cars")


ragg::agg_png(here(base_path, "plots", "stations-dorling-cartogram.png"), width = 6, height = 5, 
              units = "in", res = 200)
dorling %>% 
  ggplot() +
  geom_sf(aes(fill = stations_per_100k_vehicles),
          col = NA) +
  geom_sf_text(aes(label = state, size = stations_per_100k_vehicles), 
               col = "grey98", family = "Raleway ExtraBold",
               show.legend = FALSE) +
  # Text-only annotation for Alaska and Hawaii
  annotate("text", 
           label = paste(labels_ak_hi$label, collapse = "\n"),
           x = raster::extent(dorling)@xmin,
           y = raster::extent(dorling)@ymin,
           hjust = 0, vjust = 0, size = 3, family = "Raleway Medium") +
  scale_fill_got(option = "Greyjoy", breaks = c(40, 80, 120)) +
  scale_size_continuous(range = c(1.5, 7)) +
  guides(fill = guide_colorsteps(title.position = "top", barwidth = unit(5, "cm"),
                                 barheight = unit(0.5, "cm"))) +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption,
    fill = plot_titles$fill
  ) +
  theme_void(base_family = "Raleway", base_size = 10) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    plot.margin = margin(2, 6, 6, 6),
    plot.title = element_markdown(family = "Raleway SemiBold", size = 16),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 12, b = 8)
    ),
    plot.caption = element_markdown(hjust = 0, color = "grey18",
                                    margin = margin(t = 8)),
    legend.position = "bottom",
    legend.title = element_markdown(hjust = 0),
    legend.background = element_blank()
  )
invisible(dev.off())

