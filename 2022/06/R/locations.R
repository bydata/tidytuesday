library(tidyverse)
library(ggtext)

base_path <- here::here("2022", "06")
tuesdata <- tidytuesdayR::tt_load("2022-02-08")

aerial_victories <- tuesdata$airmen %>% 
  select(aerial_victory_credits) %>% 
  separate_rows(aerial_victory_credits, sep = "; ") %>% 
  na.omit() %>% 
  separate(aerial_victory_credits, #
           into = c("aerial_victory_credits", "aerial_victory_date"),
           sep = " on ", remove = TRUE) %>%
  mutate(aerial_victory_credits = str_extract(aerial_victory_credits, "\\d+ (1/2)?") %>% 
           str_replace(" 1/2", ".5") %>% 
           as.numeric(),
         aerial_victory_date = lubridate::mdy(aerial_victory_date)
  ) %>% 
  arrange(aerial_victory_date)


# Load tsv file with location data and join with aerial victories dataset:

locations <- read_tsv(here::here(base_path, "data", "tuskegee_airmen_victory_locations.tsv")) %>% 
  separate(lat_lon, into = c("lat", "lon"), sep = ",") %>% 
  mutate(across(c(lat, lon), as.numeric))

aerial_victories_locations <- aerial_victories %>% 
  inner_join(locations, by = c("aerial_victory_date" = "date")) %>% 
  group_by(approx_location, lat, lon) %>% 
  summarize(aerial_victory_credits = sum(aerial_victory_credits), .groups = "drop") %>% 
  mutate(country = str_match(approx_location, ", (.+)")[, 2])


## Aerial victory locations on a map

europe <- rnaturalearth::ne_countries(scale = 50, continent = "Europe", 
                                      returnclass = "sf")

#' Color palette from W.E.B. Du Bois's work
#' Source: https://origins.osu.edu/sites/default/files/migrated_files/WEBDuBois%2011.jpg
dubois_palette <- c(
  "bg" = "#DDCDBF",
  "border" = "#CBB6A4",
  "text" = "#3A3432",
  "#CA3D55", "#CBB6A4", "#727CA6", "#E8B95F", "#505D52", "#201E1A", "#D79792", "#505D52", "#201E1A"
)



ggplot() +
  geom_sf(
    data = europe, col = NA, # dubois_palette["text"], 
    fill = dubois_palette[5], size = 0.1) +
  geom_point(
    data = na.omit(aerial_victories_locations),
             aes(x = lon, y = lat, size = aerial_victory_credits,
                 fill = country),
             shape = 21, col = "white", alpha = 1, stroke = 0.2,
    #          fill = dubois_palette[6]
    ) +
  scale_fill_manual(values = unname(dubois_palette[c(4, 6:10)])) +
  scale_size_area(max_size = 8) +
  coord_sf(xlim = c(-4, 20), ylim = c(36, 58)) +
  guides(size = guide_legend(title.position = "top"),
         fill = guide_legend(title.position = "top", override.aes = list(size = 4),
                             direction = "horizontal", byrow = TRUE),
  ) +
  labs(
    title = toupper("TUSKEGEE AIRMEN AERIAL VICTORIES"),
    subtitle = toupper("Locations of the aerial victories by the Tuskegee Airmen<br>between 1943 and 1945"),
    caption = toupper("Source: Tuskegee Airmen Challenge, Tuskegee Airmen Chronology"),
    size = toupper("Aerial victory credits"),
    fill = toupper("Country of location (2022)")) +
  cowplot::theme_map() +
  theme(
    plot.background = element_rect(
      # color = dubois_palette["border"], 
      color = NA,
      fill = dubois_palette["bg"]),
    panel.background = element_rect(color = dubois_palette["border"], fill = NA,
                                    size = 1),
    legend.position = "bottom",
    legend.justification = "center",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    text = element_text(family = "Noto Sans Mono", face = "plain", 
                        color = colorspace::lighten(dubois_palette["text"], 0.3)),
    plot.title = element_text(hjust = 0.5, family = "Odibee Sans",
                              face = "plain", size = 24),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 6, b = 12),
      hjust = 0.5, halign = 0.5),
      plot.margin = margin(12, 20, 6, 20),
    plot.caption = element_markdown(size = 7, hjust = 0.5)
  )
ggsave(here(base_path, "plots", "aerial_victories_locations.png"),
       dpi = 300, width = 5, height = 6.8)



# https://flourish.studio/blog/masters-web-dubois/
