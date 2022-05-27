library(tidyverse)
library(ggtext)
library(here)
library(glue)
library(tidygraph)
library(ggraph)

base_path <- here::here("2022", "21")


## DATA PREP -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 21)
sevens <- tuesdata$sevens
fifteens <- tuesdata$fifteens

unique(sevens$tournament)
unique(fifteens$tournament)


## WORLD SERIES PLUS/MINUS -----------------------------------------------------

p <- sevens %>% 
  filter(tournament == "World Series") %>% 
  select(team_1, team_2, score_1, score_2) %>% 
  pivot_longer(cols = c(team_1, team_2), values_to = "team") %>% 
  transmute(team, score = ifelse(name == "team_1", score_1, score_2),
         score_opponent = ifelse(name == "team_1", score_2, score_1),
         across(c(score, score_opponent), as.numeric)) %>% 
  group_by(team) %>% 
  summarize(total_score = sum(score), total_score_opponent = sum(score_opponent)) %>% 
  mutate(total_score_opponent_minus = -total_score_opponent) %>% 
  ggplot(aes(fct_reorder(team, total_score))) +
  geom_col(aes(y = total_score, fill = "score")) +
  geom_col(aes(y = total_score_opponent_minus, fill = "score opponent")) +
  geom_segment(
    aes(xend = after_stat(x), y = 0, yend = total_score - total_score_opponent,
        col = total_score < total_score_opponent),
    # col = "grey20", 
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  geom_text(aes(y = total_score, label = team),
            hjust = 0, nudge_y = 50, family = "Fira Sans Light", col = "grey4", size = 3) +
  scale_y_continuous(position = "right", breaks = seq(-10000, 10000, 2500),
                     labels = abs) +
  scale_fill_manual(values = c("#497135", "#97C684")) +
  scale_color_manual(values = c("#97C684", "#497135")) +
  coord_flip(clip = "off", ylim = c(NA, 9000)) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Total <b style='color:#497135'>points scored</b>
    and <b style='color:#97C684'>conceded</b> in World Rugby Women's Sevens Series",
    caption = "**Sources:** ScrumQueens. Wikipedia. **Visualization:** Ansgar Wolsing"
  ) +
  theme_void(base_family = "Fira Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    axis.text.x.top = element_text(size = 6, color = "grey24"),
    axis.ticks.x.top = element_line(size = 0.2, color = "grey24"),
    axis.ticks.length.x.top = unit(1, "mm"),
    legend.position = "top",
    text = element_text(color = "grey24"),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_markdown(
      family = "Noto Serif", margin = margin(t = 4, b = 12)),
    plot.caption = element_markdown(color = "grey38")
  )

# add subtitle inside the plot
p + 
  annotate("richtext", 
           x = 15, y = -5000, hjust = 0,
           label = "<b style='color:#97C684'> points conceded</b>",
           label.colour = "grey80") + 
  annotate("richtext", 
           x = 19, y = 3500, hjust = 0,
           label = "<b style='color:#497135'>points scored</b>",
           label.colour = "grey80") + 
  annotate(GeomTextBox,
    x = 3, y = 3500, label = "The **World Rugby Women's Sevens Series** is a series 
    of international rugby sevens tournaments for women's national teams run by World Rugby. 
    The inaugural series was held in 2012–2013.<br><br>
    The **bars** indicate the total <b style='color:#497135'>points scored</b>
    and the points <b style='color:#97C684'>conceded</b> by each team which 
    participated in at least one event. The **arrows** show the difference between
    points scored and conceded. Arrows pointing to the right indicate a positive,
    arrows to pointing to the left a negative score difference.
    ",
    hjust = 0, vjust = 0, box.size = 0, fill = NA, color = "grey38", width = 0.39, size = 3,
    family = "Fira Sans") 
  
ggsave(here(base_path, "plots", "world_series_plus_minus.png"), dpi = 400, width = 7, height = 6)


