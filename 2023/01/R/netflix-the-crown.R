library(tidyverse)
library(ggtext)
library(here)
library(geofacet)

base_path <- here("2023", "01")

# Load the data scraped from Netflix website
retrieved_data <- read_rds(here(base_path, "data", "netflix-data.rds"))

# Add show names to top 10
add_show_names <- function(top10, show_names) {
  inner_join(top10, show_names, by = c("id", "showId"))
}

get_tv_ranking <- function(week_date, data = retrieved_data) {
  top10_tv <- data[[week_date]]$top10 %>% 
    add_show_names(retrieved_data[[week_date]]$show_names) %>%
    filter(category == "tv")
  top10_tv
}

# Prepares the data for plotting (filter for The Crown, add/translate country codes etc.)
prepare_crown_data <- function(df) {
  df %>% 
    filter(showName == "The Crown") %>% 
    mutate(rank = as.numeric(rank)) %>% 
    select(iso, showName, seasonName, week, rank, weeksInTopTen) %>% 
    # na.omit() %>%
    # add countries
    bind_rows(
      data.frame(
        iso = unique(df$iso),
        showName = NA,
        seasonName = NA,
        week = unique(df$week),
        rank = 99, 
        weeksInTopTen = NA
      )
    ) %>% 
    mutate(
      country = countrycode::countrycode(iso, origin = "iso2c", destination = "country.name"),
      code_iso_3166_2 = paste0("ISO 3166-2:", iso)) %>% 
    # handle case when multiple seasons of The Crown are in the top 10
    group_by(week, iso) %>% 
    slice_min(order_by = rank) %>% 
    ungroup() %>% 
    # add the grid 
    right_join(world_countries_grid2, by = "code_iso_3166_2") %>% 
    mutate(week = replace_na(week, min(week, na.rm = TRUE)))
  
}


top10_20220904_tv <- get_tv_ranking("20220904")
top10_20220911_tv <- get_tv_ranking("20220911")
top10_20220918_tv <- get_tv_ranking("20220918")
top10_20220925_tv <- get_tv_ranking("20220925")

# number of countries
length(unique(top10_20220904_tv$iso)) - 1
length(unique(top10_20220911_tv$iso)) - 1
length(unique(top10_20220918_tv$iso)) - 1



## Create geofacetted map ======================================================

grid_preview(world_countries_grid1) +
  theme_void()

# check overlaps between Netflix countries and grid countries
top10_20220918_tv %>% 
  distinct(iso) %>% 
  mutate(country = countrycode::countrycode(iso, origin = "iso2c", destination = "country.name"),
         code_iso_3166_2 = paste0("ISO 3166-2:", iso)) %>% 
  anti_join(world_countries_grid1)

## the grid is lacking Taiwan, let's add it below South Korea
head(world_countries_grid1)
world_countries_grid2 <- world_countries_grid1 %>% 
  add_row(name = "Taiwan", code_alpha3 = "TWN", code_country = "158", 
          code_iso_3166_2 = "ISO 3166-2:TW", col = 25, row = 8)

grid_preview(world_countries_grid2) +
  theme_void()


top10_20220918_crown <- prepare_crown_data(top10_20220918_tv) 
top10_20220904_crown <- prepare_crown_data(top10_20220904_tv)
top10_20220925_crown <- prepare_crown_data(top10_20220925_tv)

df_plot <- bind_rows(top10_20220904_crown, top10_20220918_crown) %>% 
  mutate(
    rank = as.numeric(rank),
    # rank2 = na_if(rank, 99),
    rank_grp = case_when(
      rank == 1 ~ "1",
      rank == 2 ~ "2",
      rank <= 5 ~ "3-5",
      rank <= 10 ~ "6-10",
      rank == 99 ~ "Not in Top 10",
      is.na(rank) ~ "No data"
    ),
    rank_grp = factor(rank_grp, levels = c("1", "2", "3-5", "6-10", "Not in Top 10", "No data"))) %>% 
  mutate(week = case_when(
    week == "20220904" ~ "Aug 29 to Sep 04, 2022",
    week == "20220918" ~ "Sep 12 to Sep 18, 2022"
  )) %>% 
  filter(name != "Antarctica")

# Check regions not shown on map
top10_20220904_tv %>% 
  anti_join(top10_20220904_crown, by = "iso") %>% 
  select(iso) %>% 
  unique()



## Plot ========================================================================

### Dark version -------------

color_palette <- c("#E60715", 
                   colorspace::lighten("#E60715", 0.25), 
                   colorspace::lighten("#E60715", 0.5), 
                   colorspace::lighten("#E60715", 0.75), 
                   "grey80", "grey24")

p <- df_plot %>% 
  ggplot(aes(col, -row)) +
  geom_tile(
    aes(fill = rank_grp, width = ifelse(is.na(rank), 0.6, 1),
        height = ifelse(is.na(rank), 0.6, 1)),
    color = alpha("white", 0.2)) +
  geom_tile(
    data = ~filter(., rank == 99),
    fill = "grey80",
    color = "white") +
  geom_text(aes(label = iso, color = rank > 2), 
            size = 2.5, family = "Bebas Neue") +
  # colorspace::scale_fill_discrete_sequential(palette = "Purples", rev = FALSE) +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = c("FALSE" = "white", "TRUE" = "grey12")) +
  coord_fixed() +
  facet_wrap(vars(week)) +
  guides(
    fill = guide_legend(
      title = "Country rank", title.position = "left",
      nrow = 1, override.aes = list(color = "grey83")),
    color = "none"
  ) +
  labs(
    title = "When the Queen died, people started to watch The Crown",
    subtitle = "In the week before Queen Elizabeth II. died, the show *The Crown* was not in 
    the Top 10 of Netflix in any country.
    In the week after the Queen's death,
    *The Crown* had entered the Top 10 in 82 of 93 countries.",
    caption = "If more than one season of The Crown was in the Top 10, the best rank is shown.<br>
    Source: Netflix. Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Helvetica Neue", base_size = 10) +
  theme(
    plot.background = element_rect(color = "grey6", fill = "grey6"),
    legend.position = "bottom",
    legend.key.size = unit(3, "mm"),
    panel.background = element_rect(
      color = "grey12", size = 0.2, fill = "grey12", linetype = "dashed"),
    plot.margin = margin(2, 2, 2, 2),
    text = element_text(color = "grey97", lineheight = 1.2),
    strip.text = element_text(
      size = 12, margin = margin(t = 4, b = 6), family = "Bebas Neue"),
    plot.title = element_text(
      family = "Bebas Neue", size = 24, hjust = 0.5, color = "white"),
    plot.subtitle = element_textbox(
      width = 0.75, margin = margin(t = 8, b = 12), 
      hjust = 0.5, halign = 0.5),
    plot.caption = element_markdown(
      hjust = 0.5, margin = margin(t = 12))
  )
ggsave(here(base_path, "plots", "netflix-the-crown-dark.png"), width = 7.5, height = 5)




### Light version --------

color_palette <- c("#E60715", 
                   colorspace::lighten("#E60715", 0.25), 
                   colorspace::lighten("#E60715", 0.5), 
                   colorspace::lighten("#E60715", 0.75), 
                   "grey80", "grey70")

p <- df_plot %>% 
  ggplot(aes(col, -row)) +
  geom_tile(
    aes(fill = rank_grp, width = ifelse(is.na(rank), 0.6, 1),
        height = ifelse(is.na(rank), 0.6, 1)),
    color = alpha("white", 0.2)) +
  geom_tile(
    data = ~filter(., rank == 99),
    fill = "grey80",
    color = "white") +
  geom_text(aes(label = iso, color = rank > 2), 
            size = 2.5, family = "Bebas Neue") +
  # colorspace::scale_fill_discrete_sequential(palette = "Purples", rev = FALSE) +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = c("FALSE" = "white", "TRUE" = "grey12")) +
  coord_fixed() +
  facet_wrap(vars(week)) +
  guides(
    fill = guide_legend(
      title = "Country rank", title.position = "left",
      nrow = 1, override.aes = list(color = "grey83")),
    color = "none"
  ) +
  labs(
    title = "When the Queen died, Netflix users started to watch The Crown",
    subtitle = "In the week before Queen Elizabeth II. died, the show *The Crown* was not in 
    the Top 10 of Netflix in any country.
    In the week after the Queen's death,
    *The Crown* had entered the Top 10 in 82 of 88 countries.",
    caption = "If more than one season of The Crown was in the Top 10, the best rank is shown.
    Not shown on the map: The Crown also entered the top 10 in Hong Kong, Réunion, 
    New Caledonia, Martinique, and Guadeloupe.
    Source: Netflix. Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Helvetica Neue", base_size = 10) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    legend.key.size = unit(3, "mm"),
    panel.background = element_rect(
      color = "grey95", size = 0.2, fill = "grey98", linetype = "dashed"),
    plot.margin = margin(2, 2, 2, 2),
    text = element_text(color = "grey24", lineheight = 1.2),
    strip.text = element_text(
      size = 12, margin = margin(t = 4, b = 6), family = "Bebas Neue"),
    plot.title = element_text(
      family = "Bebas Neue", size = 24, hjust = 0.5, color = "grey2"),
    plot.subtitle = element_textbox(
      width = 0.9, margin = margin(t = 8, b = 12), 
      hjust = 0.5, halign = 0.5),
    plot.caption = element_textbox(
      width = 0.95, hjust = 0.5, halign = 0.5, margin = margin(t = 12))
  )
ggsave(here(base_path, "plots", "netflix-the-crown-light.png"), width = 7.5, height = 5)
