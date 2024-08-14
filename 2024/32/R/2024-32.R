library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "32")

df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-06/olympics.csv')

# Medals by discipline by country
medals_by_country <- df |> 
  filter(!is.na(medal), season == "Summer") |> 
  distinct(year, games, team, event, sex, medal) |> 
  arrange(year, games, team)

#' Since 2020 Tokyo is not covered in the dataset, use another dataset from 
#' Kaggle: https://www.kaggle.com/datasets/ramontanoeiro/summer-olympic-medals-1986-2020
#' (Download manually.)
df_medals <- read_csv(here(base_path, "data", "Summer_olympic_Medals.csv"))

df_medal_counts_per_year <- df_medals |> 
  group_by(Year) |> 
  summarize(across(c(Gold, Silver, Bronze), sum)) |> 
  mutate(total_medals = Gold + Silver + Bronze, .by = Year)

# Check for the different country names for Germany
germany_country_names <- df_medals |> 
  filter(str_detect(Country_Name, "Germany")) |> 
  distinct(Country_Name) |> 
  pull(Country_Name)
df_medals |> 
  filter(str_detect(Country_Name, "Germany")) |> 
  group_by(Country_Name) |> 
  summarize(min(Year), max(Year))

df_medals_germany <- df_medals |> 
  filter(Country_Name %in% germany_country_names) |> 
  group_by(Year) |> 
  summarize(across(c(Gold, Silver, Bronze), sum)) |> 
  mutate(total_medals = Gold + Silver + Bronze) |> 
  inner_join(df_medal_counts_per_year, by = "Year", suffix = c(".country", ".total")) |> 
  mutate(
    gold_share = Gold.country / Gold.total,
    medals_share = total_medals.country / total_medals.total
  )


bg_color <- "#00143A" # "#001841" # "#091a3d"
df_annotations <- data.frame(
  Year = c(1936, 1976, 1984, 1992, 2024),
  label_vjust = c(-0.3, -0.3, 1.2, -0.3, 1.2),
  label = c(
    "NS-Propaganda-Spiele\nin Berlin 1936",
    "Hochphase des Dopings\nin DDR und Bundesrepublik",
    "1980 Boykott\nder Spiele\ndurch die\nBundesrepublik,\n1984 durch die DDR",
    "Erste Spiele nach der\nWiedervereinigung",
    "\nParis\n2024"
  )
)

df_medals_germany |> 
  select(Year, gold_share, medals_share) |> 
  add_row(Year = 2024, gold_share = 0.0366, medals_share = 0.0318) |> 
  pivot_longer(cols = c(gold_share, medals_share)) |> 
  mutate(
    name = ifelse(name == "gold_share", "Goldmedaillen %", "Medaillen insgesamt %"),
    name = factor(name, levels = c("Medaillen insgesamt %", "Goldmedaillen %"))) |> 
  left_join(df_annotations, by = "Year") |> 
  ggplot(aes(Year, value, col = name)) +
  geom_line(
    data = ~filter(., Year <= 1936),
    linewidth = 0.8) +
  geom_line(
    data = ~filter(., Year >= 1952),
    linewidth = 0.8) +
  geom_point(
    data = ~filter(., Year %in% c(1936, 1976, 1980, 1984, 1992, 2000, 2020, 2024)),
    shape = 21, fill = bg_color) +
  # geom_label(aes(label = Year), size = 2) +
  geom_text(
    data = ~filter(., Year == 1952),
    aes(label = str_wrap(name, 18)),
    hjust = 1, vjust = 0, family = "Roboto", fontface = "bold", nudge_x = -0.5, 
    size = 3, nudge_y = 0, lineheight = 0.9, show.legend = FALSE
  ) +
  geom_text(
    data = ~filter(., name == "Goldmedaillen %"),
    aes(Year, value, label = label, vjust = label_vjust),
    inherit.aes = FALSE, stat = "unique", hjust = 0, lineheight = 0.9, 
    nudge_x = -0.5, family = "Roboto Condensed", size = 2.5, color = "white"
  ) +
  scale_x_continuous(breaks = seq(1896, 2024, 8)) +
  scale_y_continuous(
    breaks = seq(0, 0.25, 0.05),
    minor_breaks = seq(0, 1, 0.01),
    labels = scales::label_percent(), expand = expansion(mult = c(0, 0.1))) +
  scale_color_manual(values = c("turquoise", "gold")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Deutschlands Medaillenerfolg bei den Olympischen Sommerspielen im Zeitverlauf",
    subtitle = "Anteil an den vergebenen Goldmedaillen und den Medaillen insgesamt",
    caption = "**Hinweis:** 1896 bis 1936 Deutsches Reich,
    1952 bis 1960 Gesamtdeutsche Mannschaft (Bundesrepublik, DDR, 1956 Saarland), 
    1964 bis 1988 Medaillen von Bundesrepublik und DDR kombiniert, 
    ab 1992 wiedervereinigtes Deutschland.
    **Daten:** IOC, Kaggle. **Visualisierung:** Ansgar Wolsing",
    x = NULL,
    y = "Anteil in %",
    color = NULL
  ) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    legend.position = "bottom",
    text = element_text(color = "white", lineheight = 1),
    axis.text = element_text(color = "white"),
    plot.title = element_text(color = "white", family = "Roboto", face = "bold"),
    plot.title.position = "plot",
    plot.caption = element_textbox(width = 1, hjust = 0, size = 6),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(
      color = "white", linewidth = 0.075, linetype = "solid"),
    panel.grid.minor.y = element_line(
      color = "white", linewidth = 0.025, linetype = "solid")
  )
ggsave(here(base_path, "plots", "medals-share-germany.png"),
       width = 6, height = 4, scale = 1.2)



# Ranking position since 1992
library(ggbump)

df_medals |> 
  filter(Year >= 1992) |> 
  arrange(Year, -Gold, -Silver, -Bronze) |> 
  group_by(Year) |> 
  mutate(rank = row_number()) |> 
  ungroup() |> 
  filter(Country_Name == "Germany") |> 
  ggplot(aes(Year, rank)) +
  ggbump::geom_bump(
    linewidth = 1
  ) +
  geom_label(
    aes(label = rank),
    label.size = 0
  ) +
  scale_x_continuous(breaks = seq(1992, 2024, 4)) +
  scale_y_reverse() +
  coord_cartesian(ylim = c(NA, 1)) +
  theme_minimal()
