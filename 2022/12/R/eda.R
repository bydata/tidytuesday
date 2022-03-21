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


# count of different babynames
babynames %>% 
  count(year, sex, name = "n_distinct") %>% 
  ggplot(aes(year, n_distinct, col = sex)) +
  geom_line()



# "Trending names"  ---------
trends <- babynames %>% 
  group_by(name, sex) %>% 
  arrange(year, .by_group = TRUE) %>% 
  mutate(change = prop / lag(prop) - 1) %>% 
  filter(prop > 0.005)


## BEATLES ----------------------

#' On 7 February 1964, the Beatles arrived at John F Kennedy airport in New York, 
#' greeted by thousands of screaming fans. This Daily Mirror article documents 
#' Beatlemania crossing the Atlantic, as the band dubbed the Fab Four arrived to 
#' play their first concerts in America.

beatles_names <- c("John", "Paul", "George", "Ringo")

babynames %>% 
  filter(name %in% beatles_names, sex == "M") %>% 
  filter(year >= 1930) %>% 
  ggplot(aes(year, prop, col = name)) +
   geom_line() +
  geom_vline(xintercept = 1964)

## BEATLES ----------------------

# draft year in comments
basketball_names <- c(
  "Michael",   # 1984
  "Lebron",    # 2003
  "Kobe",      # 1996
  "Hakeem",    # 1984
  "Kareem",    # 1969
  "Shaquille",  # 1992
  "Julius"     # 1972
                      )


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
  "Kyrie",     "Kyrie",                 2011,         NA,         1992
) 


# babynames %>% 
#   filter(name %in% basketball_names, sex == "M") %>% 
#   filter(year >= 1930) %>% 
#   ggplot(aes(year, prop, col = name)) +
#   geom_line() +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 0.001)) +
#   facet_wrap(vars(name), scales = "free_y")

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
  # area for active player career
  geom_rect(aes(xmin = draft_year, xmax = career_end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            stat = "unique", fill = "#AD3F24", alpha = 0.4) +
  # line: name popularity
  geom_line() +
  # geom_point(aes(x = birth_year, y = 0), 
  #            stat = "unique", size = 4, shape = 21, color = "white", fill = "#AD3F24") + 
  # scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous() +
  coord_cartesian(xlim = c(NA, max(babynames$year))) +
  facet_wrap(vars(name_label), scales = "free_y", nrow = 3) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.background = element_rect(color = NA, fill = "#D9C48B"),
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor.x = element_line(color = "white", size = 0.075),
    panel.grid.minor.y = element_blank(),
    strip.text = element_markdown(family = "Bangers", size = 12, hjust = 0,
                              margin = margin(t = 12, b = 4)),
    axis.title = element_blank(),
    text = element_text(color = "grey4"),
    plot.margin = margin(t = 4, b = 4, l = 8, r = 18)
  )
ggsave(here(base_path, "plots", "nba-names.png"), width = 7, height = 7)



#' Image credit:
#' Reisio, Public domain, via Wikimedia Commons
#' https://commons.wikimedia.org/wiki/File:Basketball.png

