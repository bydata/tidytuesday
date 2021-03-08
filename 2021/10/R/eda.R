library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)
library(colorspace)
library(extrafont)

# font_import(pattern = "Barlow|Source Sans|OpenSans|Inconsolata", prompt = FALSE)
loadfonts(quiet = TRUE)

# font size for text geoms
geom_text_font_size <- 3

# custom ggplot2 theme
theme_custom <- function(dark = FALSE, base_family = "Barlow", ...) {
  if (dark) {
    bg_color <- "#212121"
    text_color <- "grey97"
    text_color_light <- "grey90"
    line_color <- "grey50"
  } else {
    bg_color <- "grey98"
    text_color <- "grey25"
    text_color_light <- "grey35"
    line_color <- "grey89"
  }
  
  theme_minimal(base_family = base_family, ...) +
    theme(
      plot.background = element_rect(
        fill = bg_color,
        color = NA,
        size = 0.5
      ),
      plot.title = element_markdown(
        family = "Source Sans Pro SemiBold",
        size = 14,
        margin = margin(t = 16, b = 10),
        lineheight = 1.2
      ),
      plot.subtitle = element_markdown(
        size = 8,
        margin = margin(b = 8),
        lineheight = 1.2
      ),
      plot.caption = element_markdown(
        hjust = 0,
        margin = margin(t = 10, b = 6),
        color = text_color_light,
        lineheight = 1.2,
        size = 6
      ),
      strip.text = element_text(family = "Source Sans Pro SemiBold", 
                                margin = margin(t = 12, b = 2),
                                color = text_color_light),
      text = element_text(color = text_color),
      axis.title = element_text(size = 7),
      axis.text = element_text(color = text_color_light, size = 7),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(size = 0.1, color = line_color),
      plot.margin = margin(l = 12, r = 12, b = 6),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}

theme_set(theme_custom(dark = FALSE))

source("../../color_palettes.R")

#' Source & dataset description: 
#' https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-02-02

filepath_data <- file.path("data", "tuesdata.rds")

if (!file.exists(filepath_data)) {
  tuesdata <- tidytuesdayR::tt_load(2021, week = 10)
  write_rds(tuesdata, filepath_data)
} else {
  tuesdata <- read_rds(filepath_data)
}

str(tuesdata)

raw_df <- tuesdata$youtube

glimpse(raw_df)
count(raw_df, brand)

raw_df %>% 
  count(brand, year) %>% 
  ggplot(aes(year, n)) +
  geom_line(aes(col = brand)) +
  facet_wrap(vars(brand))

raw_df %>% 
  pivot_longer(cols = c(funny:use_sex), names_to = "content", values_to = "is_present") %>% 
  group_by(year, content) %>% 
  summarize(share = mean(is_present),
            n_present = sum(is_present),
            total_ads = n(), .groups = "drop") %>% 
  ggplot(aes(year, share)) +
  geom_line(aes(group = content)) +
  facet_wrap(vars(content))


## CORRELATION MATRIX: Which features are present =======================================

cor_df <- 
  raw_df %>% 
  # exclude show_product quickly
  select(-show_product_quickly) %>% 
  select(funny:use_sex) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  cor() %>% 
  as_tibble() 

colnames(cor_df) <- str_replace_all(colnames(cor_df), "_", " ") %>% 
  str_to_title() 

cor_df <- cor_df %>% 
  mutate(var1 = colnames(.)) %>% 
  select(var1, everything()) %>% 
  pivot_longer(cols = -var1, names_to = "var2", values_to = "corr") %>% 
  mutate(corr = ifelse(var1 == var2, NA, corr))

ad_features_wide <- 
  raw_df %>% 
  # exclude show_product quickly
  select(-show_product_quickly) %>% 
  select(funny:use_sex) %>% 
  mutate(ad_id = row_number()) %>% 
  pivot_longer(cols = funny:use_sex, names_to = "feature", values_to = "is_present") %>% 
  filter(is_present) 


# how many ads contain none of the features?
n_ads_no_features <- raw_df %>% 
  # exclude show_product quickly
  select(-show_product_quickly) %>% 
  select(funny:use_sex) %>% 
  mutate(ad_id = row_number()) %>% 
  filter(pmax(funny, patriotic, celebrity, danger, animals, use_sex) == FALSE) %>% 
  count()
n_ads_no_features

pairs <- ad_features_wide %>% 
  # count the occurences of each pair of features
  widyr::pairwise_count(feature, ad_id)
head(pairs)

unique_features <- count(ad_features_wide, ad_id) %>% 
  filter(n == 1) %>% 
  inner_join(ad_features_wide, by = "ad_id") %>% 
  select(item1 = feature) %>% 
  mutate(item2 = item1) %>% 
  count(item1, item2)

n_to_group <- 5
viz_cols <- 5
pairs <- bind_rows(pairs, unique_features) %>% 
  mutate(n = n %/% n_to_group) %>% 
  # uncount duplicates rows according to weight variable (n)
  uncount(n) %>% 
  mutate(across(c(item1, item2), ~str_replace_all(., "_", " ") %>% str_to_title()))


pairs_coords <- pairs %>% 
  mutate(pair = str_c(item1, item2, sep = ",")) %>% 
  # nest(data = c(item1, item2)) %>% 
  # mutate(n = map_int(data, nrow)) %>% 
  # unnest(cols = c(data)) %>% 
  mutate(across(c(item1, item2), factor)) %>% 
  select(-pair) %>% 
  arrange(item1, item2) %>% 
  group_by(item1, item2) %>% 
  mutate(row_of_pair = row_number()) %>%
  mutate(x = as.numeric(item1) + (row_of_pair - 1) %% viz_cols  / (viz_cols * 1.25) - 0.3,
         y = length(levels(item2)) + 1 - as.numeric(item2) + (row_of_pair - 1) %/% viz_cols / (viz_cols * 1.25) - 0.3) %>% 
  ungroup() 
  

divergingx_palettes()
palette <- "Purple-Green"
color_pal <- diverge_hcl(5, palette  = palette)
football_color <- "#904F4C"
football_unicode <- "\Uf44e"

plot_subtitle <- glue::glue("The darker the color, the <b style='color:{color_pal[1]}'>less often</b> or 
                            <b style='color:{color_pal[5]}'>more often</b> the two specific characteristics appear<br>together in an ad.
                            Each football represents 5 ads.")

plot_caption <- "Based on 233 ads from the 10 brands that had the most advertisements in Super Bowls from 2000 to 2020.<br>
Source: FiveThirtyEight. Visualization: @_ansgar"


cor_df %>% 
  ggplot(aes(var1, fct_rev(var2))) +
  # tiles indication correlations
  geom_tile(aes(fill = corr), col = "grey98", size = 2) +
  # add icons for each n ads that contained crossed features
  geom_text(data = pairs_coords, 
            aes(x, y),
            label = "\Uf44e", 
            family = "Font Awesome 5 Free Solid",
            col = "grey25", 
            size = 2.5, alpha = 0.9,
            # position = position_jitter(width = 0.33, height = 0.33)
            ) +
  scale_x_discrete(position = "top") +
  scale_fill_continuous_diverging(palette = palette, na.value = "grey98") +
  coord_equal() +
  labs(title = "Which contents go together in Super Bowl ads?",
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = NULL, y = NULL,
       fill = "Correlation") +
  theme(legend.position = "bottom",
        legend.key.height = unit(2, "mm"),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5))

ggsave("plots/ads_corrmatrix.png", type = "cairo", dpi = 200, width = 6, height = 6)

