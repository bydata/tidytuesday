pacman::p_load("tidyverse",
"tidytuesdayR",
"lubridate", "ggtext", "ggfx",
"colorspace",
"here",
"glue")


filepath_data <- here("2021", "38", "data", "tuesdata.rds")

if (!file.exists(filepath_data)) {
  tuesdata <- tidytuesdayR::tt_load(2021, week = 38)
  write_rds(tuesdata, filepath_data)
} else {
  tuesdata <- read_rds(filepath_data)
}

str(tuesdata)

## Prepare Billboard dataset =======
billboard <- tuesdata$billboard %>% 
  mutate(week = mdy(week_id)) %>% 
  select(-c(url, week_id))

glimpse(billboard)

## Manually add 2021 hot 100
lastchristmas_2021 <- tribble(
  ~week, ~week_position,
  "2021-12-04", 40,
  "2021-12-11", 15,
  "2021-12-18", 13,
  "2021-12-25", 9,
  
) %>% 
  mutate(week = as_date(week),
         song = "Last Christmas", 
         performer = "Wham!")


lastchristmas <- billboard %>% 
  filter(song == "Last Christmas", performer == "Wham!") %>% 
  arrange(week) %>% 
  select(week, week_position, song, performer) %>% 
  bind_rows(lastchristmas_2021)





p <- lastchristmas %>% 
  mutate(year = year(week),
         season = ifelse(month(week) == 12, year, year - 1),
         season_str = paste(season, season + 1, sep = "/"),
         week_normed = paste(str_pad(month(week), 2, side = "left", "0"), 
                             str_pad(mday(week), 2, side = "left", "0"),
                             sep = "-"),
         week_normed_date = paste(ifelse(month(week) > 10, 2020, 2021),
                                  week_normed, sep = "-"),
         week_normed_date = ymd(week_normed_date)) %>% 
  group_by(season) %>% 
  mutate(is_last = week == max(week),
         is_peak = week_position == min(week_position)) %>% 
  ungroup() %>% #View()
  filter(season > 2015) %>% 
  ggplot(aes(week_normed_date, week_position)) +
  geom_vline(aes(xintercept = as_date("2020-12-25")),
             lty = "dotted", size = 0.8, color = "#FFFFFF66"
             ) +
  geom_line(aes(group = season_str), color = "grey80") +
  with_inner_glow(
    geom_point(
      aes(fill = is_peak, size = is_peak),
      color = "white", shape = 21, # size = 7, 
      show.legend = FALSE),
    color = "grey60", sigma = 4, expand = 2
    ) +
  geom_text(data = . %>% filter(is_peak),
            aes(label = week_position),
            color = "#FFD700DD", size = 2.5, family = "Noto Serif", fontface = "bold") +
  scale_x_date(breaks = as_date("2020-12-25"), date_labels = "%b %d") +
  scale_y_reverse(breaks = seq(10, 100, 10)) +
  scale_size_manual(values = c(4, 7)) +
  scale_fill_manual(values = c("FALSE" = "grey80", "TRUE" = "darkred")) +
  coord_cartesian(xlim = c(as_date("2020-12-01"), as_date("2021-01-18")), 
                  ylim = c(NA, 1), clip = "off") +
  facet_wrap(vars(season_str), nrow = 1) +
  labs(
    title = "**Last Christmas**",
    subtitle = "has made the **Billboard Hot 100** each holiday season since
    2016/2017",
    caption = "Source: **Billboard Hot 100**, **Kaggle** |
    Visualization: **Ansgar Wolsing**",
    x = NULL,
    y = "Chart position"
  ) +
  theme_minimal(base_family = "Noto Serif", base_size = 10) +
  theme(
    plot.background = element_rect(color = NA, fill = desaturate("darkred", 0.3)),
    plot.margin = margin(t = 8, l = 8, r = 8, b = 4),
    strip.text = element_text(family = "Mountains of Christmas", face = "bold",
                              color = "#FFFFFFCC", size = 14),
    strip.background = element_rect(color = "#FFFFFF99",  size = 0, fill = "#013220"),
    panel.background = element_rect(color = "#FFFFFF99",  size = 0, 
                                    fill = "#FFFFFF44"),
    panel.grid = element_blank(),
    text = element_text(color = "grey88"),
    axis.title = element_text(color = "#FFFFFFCC"),
    axis.text = element_text(color = "#FFFFFFAA"),
    plot.title = element_markdown(size = 18, color = "white"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      size = 11, margin = margin(t = 0, b = 12)),
    plot.caption = element_textbox_simple(
      color = "#FFFFFFAA", size = 7, margin = margin(t = 12, b = 2)
    )
  )
ggsave(here("2021", "38", "plots", "lastchristmas.png"), dpi = 300,
       width = 6.5, height = 5)
