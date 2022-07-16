library(tidyverse)
library(ggtext)
library(here)
library(lubridate)

base_path <- here::here("2022", "28")

# tuesdata <- tidytuesdayR::tt_load(2022, week = 28)
# flights <- tuesdata$flights
#' Download Eurocontrol dataset from https://ansperformance.eu/data/
library(readxl)
url_data <- "https://ansperformance.eu/download/xls/Airport_Traffic.xlsx"
filepath <- "Airport_Traffic.xlsx"
download.file(url_data, destfile = filepath)
flights <- read_xlsx(filepath, sheet = "DATA")

# Color palette
color_pal = c("#E6A850", "#325163", "#6C97B0", "#C1CCBC")

flights %>% 
  filter(STATE_NAME == "Germany", APT_NAME == "Cologne-Bonn") %>% 
  mutate(day = yday(FLT_DATE)) %>%
  count(YEAR, day, APT_NAME, wt = FLT_TOT_1, name = "total_flights") %>%
  ggplot(aes(day, total_flights, group = YEAR)) +
  geom_smooth(
    data = ~subset(., YEAR < 2020),
    color = "grey50", se = FALSE, span = 0.2, size = 0.3) +
  geom_smooth(
    data = ~subset(., YEAR == 2021),
    aes(color = factor(YEAR)), se = FALSE, span = 0.2, size = 0.7) +
  geom_smooth(
    data = ~subset(., YEAR == 2022),
    aes(color = factor(YEAR)), se = FALSE, span = 0.33, size = 0.7) +
  geom_smooth(
    data = ~subset(., YEAR == 2020),
    aes(color = factor(YEAR)), se = FALSE, span = 0.2, size = 0.7) +
  annotate(
    "text", x = c(305, 120, 220, 150), y = c(420, 100, 310, 350),
    color = c("grey50", color_pal[1:3]),
    label = c("2016-2019", "2020", "2021", "2022"),
    hjust = 0, family = "Noto Sans", size = 3
  ) +
  scale_x_continuous(breaks = c(1, 91, 182, 274), labels = c("Jan", "Apr", "Jul", "Oct")) +
  scale_color_manual(values = color_pal[1:3]) + 
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  guides(col = "none") +
  labs(
    title = sprintf(
    "Pandemic hit flight numbers in <span style='color:%s'>2020</span> at Cologne-Bonn Airport,<br>
    approaching pre-pandemic level in mid-<span style='color:%s'>2022</span>", 
    color_pal[1], colorspace::lighten(color_pal[3], 0.1)),
    subtitle = "Arrivals and departures at Cologne-Bonn Airport, Germany",
    caption = "Daily flight numbers smoothed with LOWESS (bw=0.2, except 2022 bw=0.33).
    Source: Eurocontrol. Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Flights per day (smoothed)") +
  theme_minimal(base_family = "Noto Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey12"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey39", size = 0.1),
    text = element_text(color = "grey70"),
    axis.text = element_text(color = "grey55", lineheight = 1.1),
    plot.title = element_markdown(family = "Noto Serif", color = "grey90", 
                                  size = 16, lineheight = 1.25),
    plot.title.position = "plot",
    plot.caption = element_markdown(size = 7, hjust = 0)
  )
ggsave(here(base_path, "plots", "CGN-airport-years.png"), dpi = 400,
      width = 7, height = 5)
