pacman::p_load("tidyverse", "ggtext", "glue", "tidycensus")


tuesdata <- tidytuesdayR::tt_load("2022-01-11")
colony <- tuesdata$colony

# Census population data 2020
url <- "https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/apportionment-2020-table01.xlsx"
filepath <- here::here("2022", "02", "data", "apportionment-2020-table01.xlsx")
download.file(url, destfile = filepath)

population <- readxl::read_xlsx(
  filepath, skip = 4, 
  col_names = c("state", "pop2020"),
  col_types = c("text", "numeric", "skip", "skip"))

colony %>% 
  anti_join(population) %>% 
  distinct(state)

df_plot <- colony %>% 
  inner_join(population) %>% 
  filter(year == "2020", months == "January-March") %>% 
  select(state, colony_n, pop2020) 

selected_states <- c("California", "Texas", "Florida")

df_plot %>% 
  ggplot(aes(pop2020, colony_n)) +
  geom_smooth(method = "lm", color = "grey43", fill = "grey74") +
  geom_point(
    aes(fill = state %in% selected_states),  
    color = "white", size = 3, shape = 21, show.legend = FALSE) +
  ggrepel::geom_text_repel(
    data = subset(df_plot, state %in% selected_states),
    aes(label = state),
    color = "grey40", size = 2.5, family = "Montserrat"
  ) + 
  scale_x_log10(labels = scales::number_format(trim = FALSE)) +
  scale_y_log10(labels = scales::number_format(trim = FALSE)) +
  scale_fill_manual(values = c("FALSE" = "grey40", "TRUE" = "#f5ad05")) +
  labs(
    title = "",
    caption = "Sources: **U.S. Department of Agriculture**, **U.S. Census 2020** |
       Visualization: **Ansgar Wolsing**",
    x = "Population",
    y = "Number of bee colonies"
  ) + 
  theme_minimal(base_family = "Montserrat", base_size = 9) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    plot.caption = element_markdown(),
    text = element_text(color = "grey35"),
    plot.title = element_text(color = "grey4"),
    plot.title.position = "plot"
  )
ggsave(here::here("2022", "02", "plots", "scatter_colony_n-population.png"),
  dpi = 300, width = 5, height = 3.5)

with(df_plot, cor(pop2020, colony_n))
