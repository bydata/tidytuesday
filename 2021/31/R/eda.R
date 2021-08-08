library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)
library(colorspace)
library(wesanderson)
library(extrafont)
library(ggraph)
library(tidygraph)
library(patchwork)

# font_import(pattern = "Barlow|Source Sans|OpenSans|Inconsolata", prompt = FALSE)
loadfonts(quiet = TRUE)

# font size for text geoms
geom_text_font_size <- 3

# custom ggplot2 theme
theme_custom <- function(dark = FALSE, base_family = "Lato", ...) {
  if (dark) {
    bg_color <- "#212121"
    text_color <- "grey97"
    text_color_light <- "grey90"
    line_color <- "grey50"
  } else {
    bg_color <- "grey98"
    text_color <- "grey25"
    text_color_light <- "grey45"
    line_color <- "grey89"
  }
  
  theme_minimal(base_family = base_family, ...) +
    theme(
      plot.background = element_rect(
        fill = bg_color,
        color = NA,
        size = 0.5
      ),
      plot.title = element_markdown(
        family = "Georgia",
        size = 20,
        margin = margin(t = 16, b = 10),
        lineheight = 1.3
      ),
      plot.subtitle = element_markdown(
        # family = "Georgia",
        size = 10,
        margin = margin(b = 8),
        lineheight = 1.25
      ),
      plot.caption = element_markdown(
        hjust = 0,
        margin = margin(t = 10, b = 6),
        color = text_color_light,
        lineheight = 1.2,
        size = 8
      ),
      strip.text = element_text(family = "Source Sans Pro", 
                                face = "bold",
                                margin = margin(t = 12, b = 2),
                                color = text_color_light),
      text = element_text(color = text_color),
      axis.title = element_text(size = 7),
      axis.text = element_text(color = text_color_light, size = 7),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(size = 0.1, color = line_color),
      plot.margin = margin(l = 12, r = 12, b = 6),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}


theme_set(theme_custom(dark = FALSE))

source("../../color_palettes.R")

#' Source & dataset description: 
#' https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-02-02

filepath_data <- file.path("data", "tuesdata.rds")

if (!file.exists(filepath_data)) {
  tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
  write_rds(tuesdata, filepath_data)
} else {
  tuesdata <- read_rds(filepath_data)
}

str(tuesdata)

oly <- tuesdata$olympics

# Add Singapore (SGP) to the regions
regions <- tuesdata$regions %>% 
  bind_rows(list(NOC = "SGP", region = "Singapore", notes = NA))


oly <- 
  oly %>%  
  # recode equestrian events 1956 Olympics from Stockholm to Melbourne
  mutate(city = ifelse(games == "1956 Summer" & city == "Stockholm", "Melbourne", city)) %>% 
  inner_join(regions, by = c("noc" = "NOC")) %>%  
  # exclude 1906 Intercalated Games (not official Olympics)
  filter(year != 1906) %>% 
  mutate(medal = factor(medal, levels = c("Gold", "Silver", "Bronze")),
         season = factor(season),
         sex = factor(sex))


oly %>% 
  filter(season == "Summer") %>%
  count(year, games, city) %>% 
  ggplot(aes(year, n)) +
  geom_col() +
  geom_text(aes(label = paste(games, city)), 
            angle = 90, size = 2, col = "grey50",
            hjust = 0, vjust = 0.25)



# Countries of host cities =======

distinct(oly, city) %>% as.data.frame()

host_cities_countries <- tribble(
  ~city, ~host_country,
  "Barcelona", "Spain",
  "London", "UK",
  "Antwerpen", "Belgium",
  "Paris", "France",
  "Calgary", "Canada",
  "Albertville", "France",
  "Lillehammer", "Norway",
  "Los Angeles", "USA",
  "Salt Lake City", "USA",
  "Helsinki", "Finland",
  "Lake Placid",  "USA",
  "Sydney", "Australia",
  "Atlanta", "USA",
  "Stockholm", "Sweden",
  "Sochi", "Russia",
  "Nagano", "Japan",
  "Torino", "Italy",
  "Beijing", "China",
  "Rio de Janeiro", "Brazil",
  "Athina", "Greece",
  "Squaw Valley",  "USA",
  "Innsbruck", "Austria",
  "Sarajevo", "Bosnia-Herzegovina",
  "Mexico City", "Mexico",
  "Munich", "Germany",
  "Seoul", "South Korea",
  "Berlin", "Germany",
  "Oslo", "Norway",
  "Cortina d'Ampezzo", "Italy",
  "Melbourne", "Australia",
  "Roma", "Italy",
  "Amsterdam", "Netherlands",
  "Montreal", "Canada",
  "Moskva", "Russia",
  "Tokyo", "Japan",
  "Vancouver", "Canada",
  "Grenoble", "France",
  "Sapporo", "Japan",
  "Chamonix", "France",
  "St. Louis", "USA",
  "Sankt Moritz", "Switzerland",
  "Garmisch-Partenkirchen", "Germany"
)



# ========

oly_medals <- oly %>% 
  filter(season == "Summer") %>% 
  filter(!is.na(medal)) %>% 
  distinct(games, year, city, region, sport, event, medal) %>% 
  count(games, year, city, region, medal) %>% 
  group_by(games, year, city, medal) %>% 
  mutate(total_medals_games = sum(n),
         share_medal = n / total_medals_games) %>% 
  ungroup() %>% 
  inner_join(host_cities_countries, by = "city")


host_countries_medals <- oly_medals %>% 
  mutate(is_host_country = region == host_country) %>% 
  group_by(region, city, games, year, is_host_country, medal) %>% 
  summarize(total_medals_country = sum(n),
            total_medals_games = max(total_medals_games),
            .groups = "drop") %>% 
  arrange(games, region, medal) %>% 
  mutate(share_medals = total_medals_country / total_medals_games) %>% 
  # keep only nations which have hosted the Olympics
  semi_join(host_cities_countries, by = c("region" = "host_country"))
  

plot_country_medals <- function(x, which_medal = "Gold") {
  host_countries_medals %>% 
    filter(region == x, medal == which_medal) %>% 
    ggplot(aes(year, share_medals)) +
    geom_col(aes(fill = is_host_country)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
}

plot_country_medals("UK")
plot_country_medals("China")
plot_country_medals("Germany")
plot_country_medals("USA")
plot_country_medals("Australia")
plot_country_medals("Japan")
plot_country_medals("Russia")
plot_country_medals("Brazil")
plot_country_medals("Netherlands")






start_year <- 1948

host_countries_medals %>% 
  filter(year >= start_year, medal == "Gold") %>% 
  semi_join(host_cities_countries, by = c("region" = "host_country")) %>% 
  group_by(region) %>% 
  filter(any(is_host_country)) %>% 
  arrange(year, .by_group = TRUE) %>% 
  mutate(medals_share_roll_mean = zoo::rollmean(share_medals, k = 3, align = "right", fill = NA)) %>% 
  filter(!is.na(medals_share_roll_mean)) %>% 
  ungroup() %>% 
  # filter(region == "UK") %>% 
  ggplot(aes(year, medals_share_roll_mean)) +
  geom_step() +
  geom_point(data = . %>% filter(is_host_country),
             aes(y = share_medals)) +
  scale_x_continuous(breaks = seq(1896, 2016, 2 * 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(vars(region), scales = "free_y")


# How many medals per Summer Games?
medals_games <- oly %>% 
  filter(season == "Summer") %>% 
  distinct(games, year, city, sport, event, medal) %>% 
  count(games, year, city, medal) %>% 
  mutate(n = ifelse(is.na(medal), 0, n)) %>%
  pivot_wider(id_cols = c(games, year, city), names_from = "medal", values_from = "n") %>% 
  mutate(total = Gold + Silver + Bronze) %>% 
  pivot_longer(cols = -c(games, year, city), names_to = "medal", values_to = "total_medals_games")
  

olympic_colors <- c(
  "blue" = rgb(8, 133, 194, maxColorValue = 255),
  "yellow" = rgb(251, 177, 50, maxColorValue = 255),
  "green" = rgb(28, 139, 60, maxColorValue = 255),
  "red" = rgb(237, 51, 78, maxColorValue = 255),
  "white" = "white",
  "black" = "black"
)





start_year <- 1948
rollmean_k <- 3

plot <- oly %>% 
  filter(season == "Summer") %>% 
  filter(year >= start_year) %>% 
  distinct(games, year, city, region, sport, event, medal) %>% 
  count(games, year, city, region, medal) %>% 
  # keep medals NA, but set n to zero (in order to keep information about countries which participated)
  mutate(n = ifelse(is.na(medal), 0, n)) %>% 
  group_by(games, year, city, region, medal) %>% 
  # summarize(total_medals = sum(n)) %>% 
  inner_join(host_cities_countries, by = "city") %>% 
  mutate(is_host_country = region == host_country) %>% 
  ungroup() %>% 
  left_join(medals_games) %>% 
  mutate(total_medals_games = replace_na(total_medals_games, 0),
         medal_share = n / total_medals_games,
         medal_share = replace_na(medal_share, 0)) %>% 
  group_by(games, year, city, region) %>% 
  mutate(total_medals_country = sum(n),
         total_medals_country_share = total_medals_country / sum(total_medals_games, na.rm = TRUE),
         total_medals_country_share = replace_na(total_medals_country_share, 0)) %>% 
  ungroup() %>%
  arrange(games, region, medal) %>% 
  semi_join(host_cities_countries, by = c("region" = "host_country")) %>% 
  distinct(games, year, city, region, is_host_country, total_medals_country_share) %>% 
  group_by(region) %>% 
  # filter(any(is_host_country)) %>% 
  arrange(year, .by_group = TRUE) %>%
  # mutate(total_medals_share_roll_mean = zoo::rollmean(total_medals_country_share, k = 3, align = "right", fill = NA)) %>% 
  mutate(total_medals_share_roll_mean_lagged = zoo::rollmean(lag(total_medals_country_share, 1), 
                                                             k = rollmean_k, align = "right", fill = 0)) %>% 
  # filter(!is.na(total_medals_share_roll_mean_lagged)) %>%
  filter(year >= start_year + rollmean_k * 4) %>% 
  filter(any(is_host_country)) %>% 
  # exclude Germany 1972 (only data for East and West combined in dataset)
  filter(!region %in% c("Germany", "Russia")) %>% 
  ungroup() %>%
  ggplot(aes(year, total_medals_share_roll_mean_lagged)) +
  geom_line(aes(col = "Rolling average of the previous 3 Olympics")) +
  geom_ribbon(aes(ymin = 0, ymax = total_medals_share_roll_mean_lagged), 
              fill = olympic_colors["blue"], alpha = 0.05) +
  geom_segment(data = . %>% filter(is_host_country),
               aes(xend = year, y = total_medals_share_roll_mean_lagged, yend = total_medals_country_share),
               lty = "dotted", color = olympic_colors["black"]) +
  geom_point(data = . %>% filter(is_host_country),
             aes(y = total_medals_country_share, fill = "Medals won as host"), shape = 21, 
             color = olympic_colors["black"]) +
  scale_x_continuous(breaks = seq(1896, 2016, 3 * 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = unname(olympic_colors["blue"])) +
  scale_fill_manual(values = unname(olympic_colors["black"])) +
  coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(vars(region), scales = "free_y", labeller = as_labeller(function(x) str_to_upper(x))) +
  labs(title = "Preparation, Investments or<br>Home Court Advantage?",
       subtitle = glue::glue("Nations tend to exceed their previous medal achievements when<br>
       <b style='color:black'>hosting the Summer Olympics</b>.
       For context, the area graphs indicate the<br>
       <b style='color:{olympic_colors[\"blue\"]}'>rolling average of medals won at the previous 3 Summer Olympics</b>.<br>
       <br>
       To cope with the shifting number of competitions between games, the plots indicate<br>
       the share of medals won by each country.
       Note the different scales across nations<br>to emphasize the relative change in medals won per country."),
       caption = "Excluding host countries Germany (with Munich hosting the Summer Olympics 1972 and two German states competing) and<br>
       Soviet Union (Moscow 1980, with the Soviet Union dissolved in 1991).
       <br>
       <br>
       Data: <b>Kaggle</b> | 
       Visualization: <b>Ansgar Wolsing</b> (@_ansgar)",
       x = NULL, y = NULL,
       fill = NULL, color = NULL)
# ggsave("plots/SummerOlympics_host-countries_medals.png", type = "cairo", width = 8, height = 6)


# https://github.com/wurli/tidy-tuesday/blob/master/2021-07-27-olympics/2021-07-27-olympics.R
olympics_logo <- magick::image_read_svg(
  "https://upload.wikimedia.org/wikipedia/commons/5/5c/Olympic_rings_without_rims.svg"
)
olympics_logo

# see https://stackoverflow.com/questions/63548647/using-extrafont-with-cowplot-font-width-unknown-for-character-error
# cowplot::set_null_device("png")
ragg::agg_png("plots/SummerOlympics_host-countries_medals_rings.png", 
              res = 300, width = 8, height = 8, units = "in")
plot + inset_element(
  grid::rasterGrob(olympics_logo), 
  left = 0.75, 
  top = 1.025, 
  right = 1, 
  bottom = 0.80,
  align_to = "full"
)
invisible(dev.off())

