library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)
library(colorspace)
library(extrafont)

# font_import(pattern = "Barlow|Source Sans|OpenSans|Inconsolata", prompt = FALSE)
loadfonts(quiet = TRUE)

# font size for text geoms
geom_text_font_size <- 3

# custom ggplot2 theme
theme_custom <- function(dark = FALSE, base_family = "Barlow", ...) {
  if (dark) {
    bg_color <- "#212121"
    text_color <- "grey97"
    text_color_light <- "grey90"
    line_color <- "grey50"
  } else {
    bg_color <- "grey98"
    text_color <- "grey25"
    text_color_light <- "grey35"
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
        family = "Source Sans Pro SemiBold",
        size = 14,
        margin = margin(t = 16, b = 10),
        lineheight = 1.2
      ),
      plot.subtitle = element_markdown(
        size = 8,
        margin = margin(b = 8),
        lineheight = 1.2
      ),
      plot.caption = element_markdown(
        hjust = 0,
        margin = margin(t = 10, b = 6),
        color = text_color_light,
        lineheight = 1,
        size = 7
      ),
      strip.text = element_text(family = "Source Sans Pro SemiBold", 
                                margin = margin(t = 12, b = 2),
                                color = "grey90"),
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

theme_set(theme_custom(dark = TRUE))

source("../../color_palettes.R")

#' Source & dataset description: 
#' https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-02-02

filepath_data <- file.path("data", "tuesdata.rds")

if (!file.exists(filepath_data)) {
  tuesdata <- tidytuesdayR::tt_load(2021, week = 7)
  str(tuesdata)
  write_rds(tuesdata, filepath_data)
} else {
  tuesdata <- read_rds(filepath_data)
}


income_distribution <- tuesdata$income_distribution
glimpse(income_distribution)

count(income_distribution, race)
summarize(income_distribution, range = range(year))

income_distribution %>% 
  filter(year == min(year)) %>% 
  count(race)

# first year available by race
income_distribution %>% 
  group_by(race) %>% 
  summarize(min_year = min(year))

# latest year available by race
income_distribution %>% 
  group_by(race) %>% 
  summarize(latest = max(year))


#' Table H-17. Households by Total Money Income, Race, and Hispanic Origin of Householder: 1967 to 2019															
#' (Income in current and 2019 CPI-U-RS adjusted dollars (28). Households as of March of the following year)															

is_median_bracket <- function(median, bracket) {
  bracket <- str_remove_all(bracket, "[\\$,]|to") %>% 
    str_split_fixed(" ", n = 2) %>% 
    str_trim() %>% 
    as.numeric()
  bracket_min <- bracket[1]
  bracket_max <- bracket[2]
  
  median >= bracket_min & median <= bracket_max
}
is_median_bracket(60000, "$50,000 to $74,999")

selected_races <- c("White Alone, Not Hispanic", "Black Alone")
color_palette <- "Cyan-Magenta"
colors <- diverging_hcl(2, palette = color_palette)
income_distribution %>% 
  filter(year %in% c(1972, max(year)),
         race %in% selected_races) %>% 
  mutate(income_distribution2 = ifelse(race == min(race), -income_distribution, income_distribution) / 100,
         income_bracket = fct_inorder(income_bracket),
         is_median_bracket = map2_lgl(income_median, income_bracket, is_median_bracket)) %>% 
  ggplot(aes(income_bracket, income_distribution2, fill = race)) +
  geom_col(alpha = 0.7, width = 0.8, show.legend = FALSE) +
  geom_col(data = . %>% filter(is_median_bracket),
           aes(group = race, y = 0.95 * income_distribution2), 
           alpha = 0.8, width = 0.05, fill = "black") +
  coord_flip() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x))) +
  scale_color_discrete_diverging(palette = color_palette) +
  scale_fill_discrete_diverging(palette = color_palette) +
  facet_wrap(vars(factor(year))) +
  labs(title = "Income inequality in the U.S.",
       subtitle = glue::glue("To this day, <b style='color:{colors[1]}'>households headed by a Black person</b>
                          have a lower income<br>compared to households headed by
                          a <b  style='color:{colors[2]}'>white (non-hispanic) person</b>.<br>
                          Household income in USD adjusted to 2019 prices. 
                          Median of distributions highlighted.
                          "),
       caption = "Source: U.S. Census Bureau, Current Population Survey, Annual Social and Economic Supplements (CPS ASEC). Visualization: @_ansgar",
       x = NULL, y = NULL)

ggsave("plots/income_distribution.png", type = "cairo", dpi = 200, width = 6, height = 4)
