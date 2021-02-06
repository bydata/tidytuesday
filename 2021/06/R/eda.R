library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)
library(colorspace)
library(extrafont)

# font_import(pattern = "Barlow|Source Sans|OpenSans|Inconsolata", prompt = FALSE)
loadfonts(quiet = TRUE)
base_font_family <- "Barlow"

# font size for text geoms
geom_text_font_size <- 3

# custom ggplot2 theme
theme_custom <- function(dark = FALSE) {
  if (dark) {
    bg_color = "#414141"
    text_color = "grey97"
    text_color_light = "grey90"
    line_color = "grey50"
  } else {
    bg_color = "grey98"
    text_color = "grey25"
    text_color_light = "grey35"
    line_color = "grey89"
  }
  
  theme_minimal(base_family = base_font_family) +
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
        size = 10,
        margin = margin(b = 16),
        lineheight = 1.2
      ),
      plot.caption = element_markdown(
        hjust = 0,
        margin = margin(t = 10, b = 6),
        color = text_color_light,
        lineheight = 1,
        size = 7
      ),
      strip.text = element_text(family = "Source Sans Pro SemiBold", 
                                margin = margin(t = 12, b = 2)),
      text = element_text(color = text_color),
      axis.title = element_text(size = 7),
      axis.text = element_text(color = text_color_light),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(size = 0.1, color = line_color),
      plot.margin = margin(l = 12, r = 12, b = 6),
      plot.title.position = "plot"
    )
}

theme_set(theme_custom(dark = TRUE))

source("../../color_palettes.R")

#' Source & dataset description: 
#' https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-02-02

filepath_data <- file.path("data", "tuesdata.rds")

if (!file.exists(filepath_data)) {
  tuesdata <- tidytuesdayR::tt_load(2021, week = 6)
  str(tuesdata)
  write_rds(tuesdata, filepath_data)
} else {
  tuesdata <- read_rds(filepath_data)
}


# prepare dataset with graduates
prepare_grad_data <- function(df, long_format = TRUE) {
  df <- df %>% 
    select(year = Total, white = White1, black = Black1) %>% 
    mutate(across(everything(), as.numeric)) %>% 
    na.omit() 
  if (long_format) {
    df <- df %>% 
      pivot_longer(cols = -year, names_to = "race_ethnicity", values_to = "share")  
  }
  df
}


# WHITE & BLACK STUDENTS WHO GRADUATED HS
hs_students <- prepare_grad_data(tuesdata$hs_students)
hs_students

hs_students %>% 
  ggplot(aes(year, share, col = race_ethnicity)) +
  geom_line()


# WHITE & BLACK STUDENTS WHO GRADUATED COLLEGE WITH BACHELOR'S DEGREE
bach_students <- prepare_grad_data(tuesdata$bach_students)
bach_students

bach_students %>% 
  ggplot(aes(year, share, col = race_ethnicity)) +
  geom_line()


graduates <- tuesdata[c("bach_students", "hs_students")] %>% 
  map_dfr(prepare_grad_data, long_format = FALSE, .id = "level_edu") %>% 
  mutate(level_edu = str_remove(level_edu, "_students"),
         level_edu = case_when(
           level_edu == "bach" ~ "Bachelor",
           level_edu == "hs" ~ "High School"
         ),
         ratio_w_b = white/black)

colors <- c("Bachelor" = "#36BFA4", "High School" = "#ADA3E5")
smooth_bw <- 0.4

plot_titles <- list(
  title = glue::glue(
    "It took until the 2010s to achieve <i>nearly</i> similar levels of<br>
                     <span style=\"color:{colors['High School']}\">high school</span> education among Black and white persons"
  ),
  subtitle = glue::glue("Trend lines indicate how much more likely a white person in the U.S. is to have completed<br>
  <span style=\"color:{colors['High School']}\">high school</span> or graduated from college with <span style=\"color:{colors['Bachelor']}\">Bachelor's degree</span>
                      compared to a Black person"),
  caption = glue::glue("Shares of levels of education for population of 25 years or older. Ratios smoothed with Loess (bw = {smooth_bw}).<br>Data: Data.World / NCES | Visualization: @_ansgar"))

# Custom annotate function for consistent annotation styles
annotate2 <- function(...) {
  annotate(
    ...,
    col = "grey98",
    family = "Inconsolata",
    size = 2.25,
    lineheight = 1.2,
    label.color = NA,
    fill = NA,
    hjust = 0
  )
}

# Custom geom_curve function
geom_curve2 <- function(...) {
  geom_curve(
    ...,
    stat = "unique",
    col = "grey70",
    curvature = -0.3,
    size = 0.1,
    arrow = arrow(
      angle = 25,
      type = "closed",
      length = unit(1, "mm")
    )
  )
}

graduates %>% 
  ggplot(aes(year, ratio_w_b, col = level_edu)) +
  geom_smooth(size = 1, span = smooth_bw, se = FALSE, show.legend = FALSE) +
  
  # Annotation Bachelor
  annotate2("richtext", x = 2000, y = 2.3,
           label = glue::glue("Way to go for<br><b style=\"color:{colors['Bachelor']}\">Bachelor's degree</b>,<br>though")) +
  geom_curve2(aes(x = 2008, xend = 2009, y = 2.2, yend = 1.7)) +
  
  # Annotation high school
  annotate2("richtext", x = 1938, y = 2.2,
           label = glue::glue("By 1940,<br>
           a white person<br>
           was 3.4 times<br>more likely to have<br>
           completed <b style=\"color:{colors['High School']}\">high school</b><br>
          than a Black person")) +
  geom_curve2(aes(x = 1941, xend = 1941, y = 2.7, yend = 3.2)) +
  
  scale_color_manual(values = colors) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption,
       x = NULL, y = "Ratio share of white / Black graduates"
       )

ggsave(file.path("plots", "ratio_level_education.png"), type = "cairo", dpi = 200, width = 6, height = 4)
