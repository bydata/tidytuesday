# devtools::install_github("thebioengineer/tidytuesdayR")
pacman::p_load("tidyverse", "ggraph", "tidygraph", "ggtext")
tuesdata <- tidytuesdayR::tt_load("2022-01-25")

ratings <- tuesdata$ratings
details <- tuesdata$details


ratings %>% 
  filter(bayes_average > 0) %>% 
  ggplot(aes(bayes_average)) +
  stat_dots(quantiles = 100)

ratings %>% 
  filter(average > 0) %>% 
  ggplot(aes(average)) +
  stat_dots(quantiles = 100)


## graph of mechanics -------------------


mechanics <- details %>% 
  select(id, primary, boardgamemechanic) %>% 
  mutate(boardgamemechanic = str_remove_all(boardgamemechanic, "[\\[\\]']")) %>% 
  separate_rows(boardgamemechanic, sep = ", ")

count(mechanics, boardgamemechanic, sort = TRUE)

graph <- mechanics %>% 
  widyr::pairwise_count(boardgamemechanic, id, upper = FALSE) %>%  
  as_tbl_graph(directed = FALSE) %>% 
  activate(nodes) %>% 
  inner_join(count(mechanics, boardgamemechanic), by = c("name" = "boardgamemechanic"))


set.seed(123)
p <- graph %>% 
  activate(nodes) %>% 
  filter(n >= 1000) %>% 
  ggraph(layout = "gem") +
  geom_edge_fan(aes(edge_width = n),
                color = "#5B3B13", alpha = 0.8) +
  geom_node_point(aes(size = n),
                  shape = 23, fill = "#CEAF50", col = "white", stroke = 1)+
  geom_node_label(aes(label = name, size = n),
                  fill = alpha("#073453", 0.7), color = "white",
                  label.size = NA, repel = TRUE, family = "Bangers",
                  show.legend = FALSE) +
  scale_edge_width(range = c(0.05, 4), n.breaks = 6) +
  scale_size_continuous(range = c(3, 12)) +
  guides(
    edge_width = guide_legend(title.position = "top"),
    size = guide_legend(title.position = "top")
  ) +
  labs(
    title = "Board Game Mechanics",
    subtitle = "A board game mechanic refers to a specific element or type of game play.
    A board game might combine multiple game mechanics.
    The thicker an <b style='color:#5B3B13'>edge</b> in this graph, 
    the more board games share the two mechanics connected by this particular edge. 
    The size of the <b style='color:#CEAF50'>nodes</b> indicates how many board games use this mechanic.",
    caption = "Source: **Board Games Geek** | Visualization: **Ansgar Wolsing**",
    edge_width = "Number of games sharing these two mechanics",
    size = "Number of games"
  ) +
  theme_graph(base_family = "Raleway") +
  theme(
    legend.position = "bottom",
    legend.title = element_markdown(),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "left",
    legend.justification = "center",
    legend.title.align = 0,
    legend.text.align = 0,
    text = element_text(color = "grey35"),
    plot.title = element_markdown(family = "Bangers", hjust = 0.5, face = "plain",
                                  color = "#073453", size = 32),
    plot.subtitle = element_textbox(
      margin = margin(t = 6, b = 16), hjust = 0.5, lineheight = 1.3,
      width = unit(1, "npc")
    ),
    plot.caption = element_markdown()
  )
ggsave(here::here("2022", "04", "plots", "graph_boardgame-mechanics.png"),
       dpi = 600, width = 9, height = 9)
