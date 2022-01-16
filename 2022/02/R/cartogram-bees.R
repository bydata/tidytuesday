pacman::p_load("tidyverse", "ggtext", "glue", "here", "gganimate",
               "cartogram", "sf")


tuesdata <- tidytuesdayR::tt_load("2022-01-11")
colony <- tuesdata$colony

unique(colony$state)
unique(colony$months)

colony_prep <- colony %>% 
  # filter(state != "United States") %>% 
  mutate(months = factor(months, levels = c("January-March", "April-June", 
                                            "July-September", "October-December")),
         quarter = paste0("Q", as.numeric(months)),
         year_quarter = paste(year, quarter))

colony_2021q2 <- colony_prep %>% 
  filter(year == 2021, quarter == "Q2")


state_shapes <- rnaturalearth::ne_states(iso_a2 = "US", returnclass = "sf")
st_crs(state_shapes)

shape_colonies <- state_shapes %>% 
  inner_join(colony_2021q2, by = c("name" = "state")) %>% 
  st_transform(crs = "+proj=moll")
st_crs(shape_colonies)

shape_colonies_cartogram <- shape_colonies %>%
  filter(name != "Hawaii") %>% 
  cartogram_cont(weight = "colony_n")


shape_colonies_dcartogram <- shape_colonies %>%
  filter(name != "Hawaii") %>% 
  cartogram_dorling(weight = "colony_n", k = 10)


ragg::agg_png(here("2022", "02", "plots", "bee_cartogram.png"),
              res = 300, width = 5.25, height = 3.5, units = "in")
ggplot(shape_colonies_cartogram) + 
  geom_sf(aes(fill = colony_n),
          color = NA, size = 0.2) +
  geom_sf_label(data = subset(shape_colonies_cartogram, 
                             name %in% c("California", "Texas", "Florida")),
               aes(label = name),
               family = "Roboto", size = 2,
               fill = "grey8", alpha = 0.7, color = "white", label.size = 0,
               label.r = unit(0.4, "mm")) + 
  # scale_fill_continuous(trans = "pseudo_log") +
  scale_fill_gradient2(low = "grey12", mid = "white", high = "#f5ad05",
                       midpoint = log(100000), 
                       trans = "pseudo_log",
                       labels = scales::number_format(scale = 0.001, suffix = "k"),
                       breaks = c(10^3, 10^4, 10^5, 10^6)) +
  guides(fill = guide_colorbar()) +
  labs(title = "Where my Bees at?",
       subtitle = "The shapes of the state have been altered to be proportional
       to the number of bee colonies (as of 2021 Q2, excl. Hawaii)",
       caption = "Source: **U.S. Department of Agriculture** |
       Visualization: **Ansgar Wolsing**",
       fill = "Number of colonies"
  ) +
  cowplot::theme_map(font_family = "Roboto", font_size = 9) +
  theme(
    plot.background = element_rect(color = NA, fill = "#67c27f"),
    # panel.background = element_rect(fill = "white"),
    text = element_text(color = "grey29"),
    legend.position = c(0.8, 0.33),
    legend.key.width = unit(2, "mm"),
    legend.key.height = unit(5, "mm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    plot.title = element_markdown(family = "Neonderthaw", size = 28, 
                                  color = "grey2", face = "plain"),
    plot.subtitle = element_textbox_simple(),
    plot.caption = element_markdown(size = 6)
  )
invisible(dev.off())


ragg::agg_png(here("2022", "02", "plots", "bee_cartogram-dorling.png"),
              res = 300, width = 5.25, height = 3.5, units = "in")
ggplot(shape_colonies_dcartogram) + 
  geom_sf(aes(fill = colony_n),
          color = "white", size = 0.1) +
  geom_sf_label(data = subset(shape_colonies_dcartogram, 
                              name %in% c("California", "Texas", "Florida")),
                aes(label = name),
                family = "Roboto", size = 2,
                fill = "grey8", alpha = 0.7, color = "white", label.size = 0,
                label.r = unit(0.4, "mm")) + 
  # scale_fill_continuous(trans = "pseudo_log") +
  scale_fill_gradient2(low = "grey12", mid = "white", high = "#f5ad05",
                       midpoint = log(100000), 
                       trans = "pseudo_log",
                       labels = scales::number_format(scale = 0.001, suffix = "k"),
                       breaks = c(10^3, 10^4, 10^5, 10^6)) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = "Where my Bees at?",
       subtitle = "Each state is represented with a dot.
       The size of each state is proportional
       to the number of bee colonies (as of 2021 Q2, excl. Hawaii)",
       caption = "Source: **U.S. Department of Agriculture** |
       Visualization: **Ansgar Wolsing**",
       fill = "Number of colonies"
  ) +
  cowplot::theme_map(font_family = "Roboto", font_size = 9) +
  theme(
    plot.background = element_rect(color = NA, fill = "#67c27f"),
    # panel.background = element_rect(fill = "white"),
    text = element_text(color = "grey29"),
    legend.position = c(0.725, 0.125),
    legend.direction = "horizontal",
    legend.key.width = unit(6, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    plot.title = element_markdown(family = "Neonderthaw", size = 28, 
                                  color = "grey2", face = "plain"),
    plot.subtitle = element_textbox_simple(),
    plot.caption = element_markdown(size = 6)
  )
invisible(dev.off())

