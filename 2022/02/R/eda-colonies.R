pacman::p_load("tidyverse", "ggtext", "glue", "here", "gganimate")


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

colony_prep %>% 
  filter(state != "United States") %>% 
  ggplot(aes(colony_n, colony_lost)) +
  geom_point() +
  facet_wrap(vars(year_quarter))


colony_prep %>% 
  filter(state != "United States") %>% 
  group_by(year) %>% 
  summarize(n_distinct(quarter))

colony_prep %>% 
  filter(state != "United States") %>% 
  na.omit() %>% 
  distinct(year, quarter) %>% View()


p <- colony_prep %>% 
  filter(state != "United States") %>% 
  ggplot(aes(quarter, colony_n)) +
  geom_boxplot(outlier.colour = NA, fill = "grey19", alpha = 0.25) +
  geom_point(
    aes(group = state),
    shape = 21, size = 3,col = "grey12", stroke = 0.2, fill = "#fab920",
    alpha = 0.6) +
  scale_y_log10() +
  theme_minimal()
p

p_anim <- p +
  labs(subtitle = "{closest_state}") +
  transition_states(factor(year))

anim <- animate(p_anim, duration = 20, res = 200)
anim_save(here("2022", "02", "plots", "animated_bees-1.gif"))


p <- colony_prep %>% 
  filter(state == "United States") %>% 
  ggplot(aes(colony_added, colony_lost)) +
  # geom_segment(
  #   aes(x = lag(colony_added), xend = colony_added,
  #       y = lag(colony_lost), yend = colony_lost)
  # ) +
  geom_point(aes(group = year_quarter, size = colony_n),
             shape = 21, size = 3,col = "grey12", stroke = 0.2, fill = "#fab920"
  ) +
  scale_x_continuous(labels = scales::number_format(trim = FALSE)) +
  scale_y_continuous(labels = scales::number_format(trim = FALSE)) +
  scale_size_area() +
  labs(title = "Bee colonies added and bee colonies lost in the United States",
       x = "Colonies added", y = "Colonies lost") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    plot.title.position = "plot"
    )
p
p_anim <- p +
  labs(subtitle = "{closest_state}") +
  transition_states(year_quarter) +
  shadow_mark(alpha = 0.2, colour = "grey40")
anim <- animate(p_anim, duration = 20, res = 200)
anim_save(here("2022", "02", "plots", "animated_bees-2.gif"))




states_top10 <- colony_prep %>% 
  filter(!state %in% c("United States", "Other states")) %>% 
  filter(!is.na(colony_n)) %>% 
  group_by(state) %>% 
  summarize(max_colony_n = max(colony_n)) %>% 
  slice_max(max_colony_n, n = 10) %>% 
  pull(state)


p <- colony_prep %>% 
  # filter(!state %in% c("United States", "Other states")) %>% 
  filter(state %in% states_top10) %>% 
  # data is missing for Q2 2019
  filter(year != 2019 & quarter != "Q2") %>% 
  select(year, quarter, year_quarter, state, colony_added, colony_lost, colony_n) %>% 
  pivot_longer(cols = c(colony_lost, colony_added)) %>% 
  mutate(value = ifelse(name == "colony_lost", -value, value),
         state = fct_reorder(state, colony_n)) %>% 
  # filter(year == 2015, quarter == "Q1") %>% 
  ggplot(aes(state, value)) +
  geom_col(aes(fill = name), show.legend = FALSE) +
  # scale_y_continuous(labels = scales::number_format(trim = FALSE)) +
  scale_y_continuous(labels = scales::number_format(scale = 0.001, suffix = "k")) +
  scale_fill_manual(values = c("colony_lost" = "grey56", "colony_added" = "#fab920")) +
  coord_flip() +
  labs(title = "Bee Colonies <b style='color:grey56'>Lost</b>
       and Bee Colonies <b style='color:#fab920'>Added</b>",
       caption = "Source: **U.S. Department of Agriculture** |
       Visualization: **Ansgar Wolsing**",
       x = NULL,
       y = NULL) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    text = element_text(color = "grey48"),
    plot.title = element_markdown(family = "Georgia", color = "grey2"),
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(),
    plot.title.position = "plot",
    panel.ontop = TRUE,
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "grey87", size = 0.3),
    panel.grid.minor.x = element_line(color = "grey87", size = 0.1))
p
p_anim <- p +
  labs(subtitle = "United States, selected states<br><br><br>
  <b style='color:grey60;font-size:22pt'>{closest_state}</b>") +
  transition_states(year_quarter) +
  shadow_mark(alpha = 0.1, fill = "grey60")
anim <- animate(p_anim, duration = 20, res = 200, unit = "in", width = 5.5, height = 5)
anim_save(here("2022", "02", "plots", "animated_bees-3.gif"))
