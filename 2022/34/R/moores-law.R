library(tidyverse)
library(ggtext)
library(lubridate)
library(here)

base_path <- here::here("2022", "34")

tuesdata <- tidytuesdayR::tt_load(2022, week = 34)

#' Download the data from the original source:
#' https://chip-dataset.vercel.app/

df <- read_csv(here(base_path, "data", "chip_dataset.csv"), name_repair = janitor::make_clean_names)
df <- df %>% 
  mutate(release_date = as_date(release_date)) %>% 
  filter(!is.na(release_date))
skimr::skim(df)

df %>% 
  filter(!is.na(transistors_million)) %>% 
  ggplot(aes(release_date, transistors_million, color = type)) +
  geom_smooth(method = "gam") +
  geom_point(aes(color = stage(type, after_scale = colorspace::desaturate(color, 0.75))), 
                 size = 0.2, alpha = 0.5) +
  scale_y_log10() +
  scale_color_manual(values = c("#EEC11A", "#03FD00")) +
  facet_wrap(vars(type)) +
  guides(color = "none") +
  labs(
    title = "Moore's Law Still Holds, Especially in <span style='color:#03FD00'>GPU</span>",
    subtitle = "Moore's law refers to the observation made by 
    Gordon Moore in 1965 that the number of transistors in a dense integrated 
    circuit doubles about every two years.
    The lines indicate overall trends of advancements in CPU and GPU, the dots show
    the number of transistors of individual products.",
    x = "Release date",
    y = "Transistors (in millions, log scale)",
    caption = "Smoothed lines using an generalized additive model.<br>
    Data: Chip Dataset CC BY-ND 4.0). Visualization: Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Cabinet Grotesk", base_size = 8) +
  theme(
    plot.background = element_rect(color = NA, fill = "grey8"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "grey90"),
    axis.title = element_text(face = "bold", color = "grey90"),
    text = element_text(color = "grey90"),
    strip.text = element_text(family = "Tabular Semibold", size = 9, color = "grey90"),
    strip.background = element_rect(color = NA, fill = "grey20"),
    panel.background = element_rect(color = NA, fill = "grey30"),
    plot.title = element_markdown(face = "bold", color = "grey97", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      lineheight = 1.2, size = 8,
      width = 0.95, margin = margin(t = 2, b = 8)),
    plot.caption = element_markdown()
      
  )
ggsave(here(base_path, "plots", "moores-law.png"), dpi = 500, width = 6, height = 4)
