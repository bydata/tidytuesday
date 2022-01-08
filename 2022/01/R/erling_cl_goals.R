# devtools::install_github("JaseZiv/worldfootballR")
pacman::p_load("tidyverse", "lubridate", "here", "ggtext", "glue", "ggforce",
               "worldfootballR")

fb_player_match_logs("https://fbref.com/en/players/1f44ac21/Erling-Haaland", 
                     stat_type = "summary", season_end_year = 2021)

fb_player_match_logs_possibly <- possibly(fb_player_match_logs, otherwise = NULL)

player_urls <- c("Lionel Messi" = "https://fbref.com/en/players/d70ce98e/Lionel-Messi", 
                 "Cristiano Ronaldo" = "https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo",
                 "Erling Haaland" = "https://fbref.com/en/players/1f44ac21/Erling-Haaland",
                 "Kylian Mbappé" = "https://fbref.com/en/players/42fd9c7f/Kylian-Mbappe",
                 "Robert Lewandowski" = "https://fbref.com/en/players/8d78e732/Robert-Lewandowski",
                 "Karim Benzema" = "https://fbref.com/en/players/70d74ece/Karim-Benzema",
                 "Raúl" = "https://fbref.com/en/players/2b81295d/Raul")

years <- 1995:2022
player_urls_years <- expand.grid(player_url = player_urls, season_end_year = years)

player_data <- list()
years <- as.character(years)
for (player_url in player_urls) {
  message(glue("Player: {player_url}"))
  for (year in years) {
    message(glue("|___ {year}"))
    player_data[[player_url]][[year]] <- 
      fb_player_match_logs_possibly(player_url, year, stat_type = "summary")
  }
}

df <- map(player_data, bind_rows) %>% 
  bind_rows() %>% 
  tibble()


player_birthdates <- tibble(
  name = c("Lionel Messi",
           "Cristiano Ronaldo",
           "Erling Haaland",
           "Kylian Mbappé",
           "Robert Lewandowski",
           "Karim Benzema",
           "Raúl"
           ),
  birthday = mdy(c("June 24, 1987",
                   "February 5, 1985",
                 "July 21, 2000",
                 "December 20, 1998",
                 "August 21, 1988",
                 "December 19, 1987",
                 "June 27, 1977")
  ))


df_plot <- df %>% 
  filter(Comp == "Champions Lg") %>%
  # remove matches when player was in squad but didn't play
  filter(Pos != "On matchday squad, but did not play") %>%
  inner_join(player_birthdates, by = c("Player" = "name")) %>% 
  arrange(Date) %>% 
  mutate(Date = as_date(Date),
         matchday_age = interval(birthday, Date),
         matchday_age = as.period(matchday_age, "years"),
         matchday_age2 = year(matchday_age) + 
           month(matchday_age) / 12 + 
           day(matchday_age) / 365) %>%
  group_by(Player) %>%
  mutate(Gls_cumul = cumsum(Gls),
         match_count = row_number()) %>% 
  ungroup() %>% 
  mutate(Player = factor(Player, levels  = c("Kylian Mbappé", "Raúl", "Karim Benzema", 
                                             "Robert Lewandowski",
                                             "Lionel Messi", "Cristiano Ronaldo",
                                             "Erling Haaland"
                                             ))) %>% 
  arrange(Player, Date) %>% 
  # create a new row index for transition_reveal() 
  select(Player, Date, match_count, Gls_cumul) %>% 
  #add_row(Player = NA_character_, Date = NA, match_count = 0, Gls_cumul = 0) %>% 
  mutate(row_index = row_number())
         

# df_annotations <- df_plot %>% 
#   group_by(Player) %>% 
#   slice_max(order_by = Gls_cumul, n = 1, with_ties = FALSE) %>% 
#   ungroup() %>% 
#   select(Player, Date, matchday_age, matchday_age2, match_count, Gls_cumul)

colors <- c("grey50", "grey60", "grey70", "grey80", "grey90", "white", "yellow")

p <- df_plot %>% 
  ggplot(aes(match_count, Gls_cumul)) +
  geom_path(aes(col = fct_inorder(Player)), show.legend = FALSE,
            lineend = "round", linejoin = "round") +
  scale_color_manual(values = colors) +
  labs(
    title = "Top Goal Scorers in the UEFA Champions League",
    caption = "Source: **FBRef** | Visualization: **Ansgar Wolsing**",
    x = "CL appearances",
    y = "Goals scored"
  ) +
  theme_minimal(base_family = "Helvetica Neue", base_size = 10) +
  theme(
    text = element_text(color = "grey69"),
    plot.background = element_rect(color = NA, fill = "grey4"),
    panel.grid.major = element_line(size = 0.2, color = "grey26"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(size = 0.1, color = "grey19"),
    plot.title = element_text(color = "white", family = "Helvetica Neue Medium",
                              size = 14),
    plot.title.position = "plot",
    plot.margin = margin(12, 8, 8, 8),
    plot.caption = element_markdown(size = 7, margin = margin(t = 12, b = 4))
  )
p
ggsave(here("2022", "01", "plots", "strikers_goals.png"), dpi = 200, width = 6, height = 4)

library(gganimate)

p_anim <- p + geom_label(aes(
  # label = Player,
  label = str_pad(toupper(Player), 20, "both", " "), 
                             x = 5, y = 115, color = Player),
              stat = "unique", show.legend = FALSE, hjust = 0, 
              family = "Inconsolata",
              label.padding = unit(2, "mm"),
              fill = "grey4",
              label.size = 0.2) +
  geom_point(aes(col = fct_inorder(Player)), show.legend = FALSE,
             size = 0.75) +
  transition_reveal(row_index) +
  ease_aes("cubic-in")
animate(p_anim, end_pause = 24, fps = 24, nframes = 24 * 8,
        res = 300, width = 1500, height = 1200, device = "ragg_png")
anim_save(here("2022", "01", "plots", "animated.gif"))

