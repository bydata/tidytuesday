pacman::p_load("tidyverse", "ggtext", "glue", "tidycensus", "ggraph", "tidygraph")

tuesdata <- tidytuesdayR::tt_load("2022-01-18")
chocolate <- tuesdata$chocolate


characteristics <- chocolate %>% 
  mutate(id = row_number()) %>% 
  separate_rows(most_memorable_characteristics, sep = ", ")  %>% 
  select(id, rating, characteristic = most_memorable_characteristics)

c1 <- characteristics %>% 
  widyr::pairwise_count(item = characteristic, feature = id, upper = FALSE) %>% 
  arrange(-n)

c2 <- characteristics %>% 
  widyr::pairwise_count(item = characteristic, feature = id, wt = rating, upper = FALSE) %>% 
  arrange(-n) %>% 
  rename(wt = n)

inner_join(c1, c2, by = c("item1", "item2")) %>% 
  mutate(avg_rating = wt / n)

c_count <- characteristics %>% 
  count(characteristic, name = "total") %>% 
  arrange(-total)

graph <- c1 %>% 
  filter(n >= 10) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  activate(nodes) %>% 
  inner_join(c_count, by = c("name" = "characteristic"))

# color palette: https://colors.dopely.top/palettes/gZKp0nH2lcx

plot_titles <- list(
  title = "Chocolate Characteristics",
  subtitle = "Most memorable characteristics which are frequently mentioned 
  together in reviews (at least 10 chocolate bars). The width of the bars 
  indicates the number of times two characteristics were stated jointly for a 
  chocolate bar.",
  caption = "Source: **Flavors of Cacao** | Visualization: **Ansgar Wolsing**"
)

p <- graph %>%
  ggraph(layout = "circle") +
  geom_edge_arc(aes(edge_width = n),
                alpha = 0.6,
                 edge_colour = "#896B60") +
  geom_node_point(aes(size = total), shape = 21, fill = "#F5F2EA", col = "#4A332D") +
  geom_node_text(aes(label = name, size = total),
                  repel = TRUE, family = "Amiri", color = "#3b2824",
                 show.legend = FALSE) +
  scale_edge_width(range = c(0.1, 4)) +
  scale_size_area(max_size = 12) +
  guides(size = guide_legend(title.position = "top"),
         edge_width = guide_legend(title.position = "top")) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption,
       size = "Chocolate bars with this characteristic", 
       edge_width = "Chocolate bars which share these two characteristics") +
  theme_graph(base_family = "Amiri") +
  theme(plot.title = element_text(
    family = "Fredericka The Great", face = "plain", size = 28, hjust = 0, 
    color = "#3b2824"),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 0, b = 12)
    ),
    plot.caption = element_markdown(
      margin = margin(t = 8, b = 2),
      hjust = 1
    ),
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "vertical",
    legend.box.margin = margin(t = 12, 0, 0, 0),
    legend.box.spacing = unit(0, "mm"),
    legend.box.just = "left",
    # legend.box.background = element_rect(color = "green"),
    legend.spacing.y = unit(-1, "mm")
    )
ggsave(here::here("2022", "03", "plots", "network_characteristics.png"),
       dpi = 600, width = 6, height = 7.5)

