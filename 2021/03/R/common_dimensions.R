library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)
library(colorspace)
library(extrafont)
library(tidytext)

loadfonts(quiet = TRUE)

source("../../color_palettes.R")

#' Source & dataset description: 
#' https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-01-12

filepath_data <- file.path("data", "tuesdata.rds")

if (!file.exists(filepath_data)) {
  tuesdata <- tidytuesdayR::tt_load(2021, week = 3)
  str(tuesdata)
  write_rds(tuesdata, filepath_data)
} else {
  tuesdata <- read_rds(filepath_data)
}

artists <- tuesdata$artists
artwork <- tuesdata$artwork

glimpse(artists)
glimpse(artwork)

## CLEANING DIMENSIONS ============================

# extract width or height from character vector
extract_dimensions <- function(x, dim = c("width", "height")) {
  if (missing(dim) | length(dim) != 1) {
    dim <- "width"
  }
  x <- ifelse(str_detect(x, "mm"), x, NA)
  d <- str_match(x, "(\\d+)\\s?x\\s?(\\d+)")
  colnames(d) <- c("original", "width", "height")
  d <- d[, dim]
  d <- as.numeric(d)
  d
}

artwork <- artwork %>% 
  mutate(width2 = extract_dimensions(dimensions, "width"),
         height2 = extract_dimensions(dimensions, "height"),
         dim2 = ifelse(!is.na(width2) & !is.na(height2), paste(width2, "x", height, "mm"), NA))

summary(artwork)

count(artwork, dim2, sort = TRUE)

artwork %>% 
  filter(!is.na(width2)) %>% 
  count(width2, height2) %>% 
  ggplot(aes(width2, height2)) +
  geom_point(aes(size = n))



offset <- 10
show_dimensions <- 10

artwork_dimensions <- artwork %>% 
  filter(!is.na(width2) & !is.na(height2) & !is.na(year)) %>%
  # # exclude artworks by Joseph Mallord William Turner
  # filter(artist != "Turner, Joseph Mallord William") %>% 
  arrange(year) %>% 
  mutate(century = ceiling(year / 100),
         century2 = case_when(
           century < 19  ~ "Before 19<sup>th</sup>",
           century == 21 ~ "21<sup>st</sup>",
           TRUE          ~ str_c(century, "<sup>th</sup>")
         ),
         century2 = paste(century2, "century"),
         century2 = fct_inorder(century2)) %>% 
  # round dimensions to cm
  mutate(across(c(width2, height2), function(x) round(x / 10, 0))) %>% 
  group_by(century2) %>% 
  count(width2, height2, dim2) %>% 
  mutate(share = n / sum(n),
         dim2 = str_remove(dim2, " mm")) %>% 
  slice_max(order_by = n, n = show_dimensions, with_ties = FALSE) %>% 
  ungroup() 

label_before_19th <- factor(levels(artwork_dimensions$century2)[1],
                            levels = levels(artwork_dimensions$century2))

# bar chart
artwork_dimensions %>% 
 ggplot(aes(reorder(dim2, share), share)) +
  geom_col() +
  coord_flip() +
  facet_wrap(vars(century2), scales = "free_y")

artwork_dimensions %>% 
  group_by(century2) %>% 
  summarize(across(c(width2, height2), 
                   .fns = list(mean = mean, median = median, sd = sd)))

artwork_dimensions %>% 
  ggplot(aes(xmin = offset, ymin = 0, xmax = offset + width2, ymax = height2)) +
  geom_rect(aes(alpha = share, fill = century2), 
            col = "white", size = 0.25, 
            ) +
  geom_text(data = tibble(century2 = label_before_19th),  
            aes(x = 26.5, y = 33), 
            stat = "unique", 
            label = "43 cm", 
            hjust = 0.25,
            vjust = 0,
            size = 1.5,
            col = "grey50",
            family = "Open Sans",
            inherit.aes = FALSE) +
  geom_segment(data = tibble(century2 = label_before_19th),  
               aes(x = 10, xend = 53, y = 31, yend = 31), 
               stat = "unique", inherit.aes = FALSE,
               col = "grey30",
               size = 0.15,
               lineend = "square",
               linetype  = "dashed"
  ) +
  geom_point(data = tibble(century2 = rep(label_before_19th, 2), width2 = c(10, 53), height2 = c(31, 31)), 
             aes(x = width2, y = height2),
             stat = "unique", inherit.aes = FALSE,
             col = "grey30", size = 0.8,
             shape = "|"
  ) +
  geom_text(data = tibble(century2 = label_before_19th),  
            aes(x = 59, y = 13.5), 
            stat = "unique", 
            label = "27 cm", 
            hjust = 0.5,
            vjust = 1,
            size = 1.5,
            col = "grey50",
            family = "Open Sans",
            inherit.aes = FALSE,
            angle = 90) +
  geom_segment(data = tibble(century2 = label_before_19th),  
               aes(x = 57, xend = 57, y = 0, yend = 27), 
               stat = "unique", inherit.aes = FALSE,
               col = "grey30",
               size = 0.15,
               lineend = "square",
               linetype  = "dashed"
  ) +
  geom_point(data = tibble(century2 = rep(label_before_19th, 2), width2 = c(57, 57), height2 = c(0, 27)), 
             aes(x = width2, y = height2),
             stat = "unique", inherit.aes = FALSE,
             col = "grey30", size = 0.8,
             shape = "_"
  ) +
  scale_color_tate(aesthetics = c("fill", "color")) +
  scale_alpha_continuous(range = c(0.075, 0.25), labels = scales::percent_format(accuracy = 1)) +
  coord_equal(xlim = c(0, 120)) +
  facet_wrap(vars(century2), nrow = 1) +
  guides(fill = FALSE) +
  labs(title = str_to_upper("Most common<br>artwork dimensions<br>in the Tate Collection"),
       caption = glue::glue("The {show_dimensions} most common combinations of width and height per period are displayed. 
       Dimensions rounded to full centimeters.<br>Source: The Tate Collection | Visualization: @4nsgarW"),
       x = NULL, y = NULL, alpha = "% of artwork within period") +
  theme_minimal(base_family = "Open Sans") +
  theme(panel.grid = element_blank(),
        text = element_text(color = "grey30", lineheight = 1.2),
        plot.title = element_markdown(family = "Quattrocento", hjust = 0.5, size = 18,
                                      color = "black", lineheight = 1.2),
        plot.title.position = "plot",
        axis.title.x = element_text(hjust = 0),
        axis.text = element_blank(),
        plot.margin = margin(t = 12, l = 20, r = 20, b = 8),
        plot.background = element_rect(color = NA, fill = "#fffffa"),
        panel.background = element_rect(color = NA, 
                                        fill = darken("#fffffa", 0.0125), size = 0.1),
        plot.caption = element_markdown(family = "Open Sans Light", hjust = 0, size = 6, 
                                        lineheight = 1.2),
        strip.text = element_markdown(size = 12, family = "Oswald SemiBold", 
                                      color = "grey50", margin = margin(t = 20, b = 6)),
        legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_text(size = 7, family = "Open Sans Light"),
        legend.text = element_text(size = 7, family = "Open Sans Light"),
        legend.key.height = unit(1, "mm")
  )

ggsave("plots/artwork_dimensions.png", type = "cairo", dpi = 200, width = 6, height = 4.5)
 
