library(tidyverse)
library(ggtext)
library(here)
library(lubridate)

base_path <- here::here("2022", "12")


## DATA PREP -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2022-03-22")
babynames <- tuesdata$babynames

glimpse(babynames)


## EDA  ------------------------------------------------------------------------

babynames %>% 
  group_by(year, sex) %>% 
  summarize(min_n = min(n), .groups = "drop") %>% 
  arrange(desc(min_n))
# ==> names seems to be capped at a minimum value of 5


## BASKETBALL PLAYERS ----------------------------------------------------------

basketball_players <- tribble(
  ~name,      ~full_name,              ~draft_year, ~career_end, ~birth_year,
# "Michael",   "Michael Jordan",        1984,       2003,         1963,
  "Lebron",    "LeBron James",          2003,         NA,         1984, 
  "Kobe",      "Kobe Bryant",           1996,       2016,         1978,
  "Hakeem",    "Hakeem Olajuwon",       1984,       2002,         1963,
  "Kareem",    "Kareem Abdul-Jabbar",   1969,       1989,         1947, 
  "Shaquille", "Shaquille O'Neal",      1992,       2011,         1972,
# "Dirk",      "Dirk Nowitzki",         1997,       2019,         1978,
  "Carmelo",   "Carmelo Anthony",       2003,         NA,         1984,
  "Dominique", "Dominique Wilkins",     1982,       1999,         1960,
  "Klay",      "Klay Thompson",         2011,         NA,         1990,
# "Isiah",     "Isiah Thomas",          1981,       1994,         1961,
  "Kyrie",     "Kyrie Irving",          2011,         NA,         1992
) 



# Annotations
plot_titles <- list(
  title = "Popularity of NBA Player Names",
  subtitle = "The graph shows the number of children (identified as male) born in 
  the U.S. with the name of the respective NBA professional by year.
  The colored area marks the active career of the players. 
  Note the different scaling of the y-axis.
  And please remember: correlation does not imply causation.",
  caption = "**Source:** {babynames} R package, Hadley Wickham | 
  **Visualization:** Ansgar Wolsing | <br>
  **Image credit:** Reisio, Public domain, via Wikimedia Commons"
)

babynames %>% 
  filter(name %in% basketball_players$name, sex == "M") %>% 
  inner_join(basketball_players, by = "name") %>% 
  mutate(career_end = replace_na(career_end, 2022)) %>% 
  # calculate name maximum
  group_by(name) %>% 
  mutate(name_max_n = max(n)) %>% 
  ungroup() %>% 
  mutate(
    name_label = paste(
      name, 
      glue::glue("<span style='font-family:Helvetica;font-size:8pt'>{full_name}</span>"), 
      sep = "<br>"), 
    name_label = fct_reorder(name_label, -name_max_n)) %>% 
  filter(year >= 1950) %>% 
  ggplot(aes(year, n)) +
  # basketball background
  geom_richtext(
    aes(x = min(year), y = 0, 
        label = glue::glue("<img src='{here(base_path, \"Basketball.png\")}'>")), 
    stat = "unique") + 
  # area for active player career
  geom_rect(aes(xmin = draft_year, xmax = career_end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            stat = "unique", fill = "white", alpha = 0.4) +
  # line: name popularity
  geom_line(size = 0.8, color = "white") +
  scale_y_continuous() +
  coord_cartesian(xlim = c(NA, max(babynames$year))) +
  facet_wrap(vars(name_label), scales = "free_y", nrow = 3) +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption
  ) + 
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor.x = element_line(color = "white", size = 0.075),
    panel.grid.minor.y = element_blank(),
    strip.text = element_markdown(
      family = "Bangers", size = 12, hjust = 0, lineheight = 0.8, 
      margin = margin(t = 12, b = 4)),
    plot.title = element_text(face = "bold", size = 18),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      size = 9, margin = margin(t = 4, b = 8)
    ),
    plot.caption = element_markdown(
      hjust = 0, margin = margin(t = 8), size = 7, color = "grey30"),
    axis.title = element_blank(),
    text = element_text(color = "grey4", lineheight = 1.25),
    plot.margin = margin(t = 8, b = 4, l = 8, r = 18)
  )
ggsave(here(base_path, "plots", "nba-names.png"), width = 6, height = 6)


#' Image credit:
#' Reisio, Public domain, via Wikimedia Commons
#' https://commons.wikimedia.org/wiki/File:Basketball.png
