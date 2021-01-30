library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)
library(colorspace)
library(extrafont)
library(ggforce)

loadfonts(quiet = TRUE)

source("../../color_palettes.R")

#' Source & dataset description: 
#' https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-01-26

filepath_data <- file.path("data", "tuesdata.rds")

if (!file.exists(filepath_data)) {
  tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
  str(tuesdata)
  write_rds(tuesdata, filepath_data)
} else {
  tuesdata <- read_rds(filepath_data)
}

plastics <- tuesdata$plastics
glimpse(plastics)

plastics <- plastics %>% 
  mutate(parent_company = na_if(str_to_lower(parent_company), "null"))

plastics_2020 <- filter(plastics, year == 2020)

count(plastics_2020, parent_company, sort = TRUE)
count(plastics_2020, country, sort = TRUE)


top_companies <- plastics_2020 %>% 
  filter(!is.na(parent_company) & str_to_lower(parent_company) != "unbranded") %>% 
  group_by(parent_company) %>% 
  summarize(n = n(), sum = sum(grand_total, na.rm = TRUE)) %>% 
  slice_max(order_by = n, n = 4)

df <- plastics_2020 %>% 
  inner_join(top_companies) %>% 
  select(parent_company, country, grand_total) %>% 
  mutate(parent_company = glue::glue("<img src='input/companies/{parent_company}.png' height='21'>")) %>% 
  group_by(country) %>% 
  filter(sum(grand_total) >= 250) %>% 
  ungroup() %>% 
  gather_set_data(x = 1:2, id_name = "id") %>% 
  mutate(x = fct_inorder(x))
  

plot_subtitle <- "Each year, Break Free From Plastic gather to reveal the top plastic polluters.
With brand audits, people collect plastic waste and document the brand on each item.
Not surprisingly, they represent some of the FMCG companies with the highest market share."

g <- df %>% 
ggplot(aes(x, id = id, split = y, value = grand_total)) +
  geom_parallel_sets(aes(fill = parent_company), 
                     alpha = 0.5, axis.width = 0.1,
                     color = NA,
                     show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(color = NA, angle = 0, hjust = 0,
                            family = "Montserrat", size = 2.5) +
  scale_fill_discrete_sequential(palette  = "Blues 2")

gg <- ggplot_build(g)

g +
  # left hand side: Company logos
  geom_tile(data = filter(gg[["data"]][[3]], 
                              str_detect(label, paste(top_companies$parent_company, collapse = "|"))),
                aes(x - 0.275, y, width = 0.425, height = 5200),
                inherit.aes = FALSE,
                color = "grey80",
                fill = "white") +
  
  # left hand side: Company logos
  geom_richtext(data = filter(gg[["data"]][[3]], 
                              str_detect(label, paste(top_companies$parent_company, collapse = "|"))),
                aes(x, y, label = label),
                inherit.aes = FALSE,
                hjust = 1, 
                stat = "unique",
                # label.color = "grey80",
                label.color = NA,
                fill = "white",
                nudge_x = -0.05,
                family = "Montserrat") +
  
  # right hand side: Countries
  geom_richtext(data = filter(gg[["data"]][[3]], 
                              !str_detect(label, paste(top_companies$parent_company, collapse = "|"))),
                aes(x, y, label = str_to_upper(label)),
                inherit.aes = FALSE,
                hjust = 0, 
                stat = "unique",
                label.color = NA,
                fill = NA,
                nudge_x = 0.05,
                color = "grey30", size = 2,
                family = "Montserrat") +
  
  # labels & theme
  labs(title = str_to_upper("Whose litter was collected where?"),
       subtitle = plot_subtitle,
       caption = "Source: Break Free From Plastic Brand Audit 2020 | Visualization: @_ansgar") +
  theme_void(base_family = "Barlow") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(color = "grey30"),
        plot.title = element_markdown(family = "Barlow SemiBold", color = "black", size = 18),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_markdown(hjust = 0),
        plot.background = element_rect(color = NA, fill = "grey98"),
        plot.margin = margin(t = 12, l = 8, r = 8, b = 8))

ggsave(file.path("plots", "sankey.png"), type = "cairo", dpi = 200, width = 6, height = 6.5)

