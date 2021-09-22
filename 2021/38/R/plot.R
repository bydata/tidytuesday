pacman::p_load("tidyverse",
"tidytuesdayR",
"lubridate",
"ggtext",
"patchwork",
"here",
"glue")

# font size for text geoms
geom_text_font_size <- 3

# custom ggplot2 theme
theme_custom <- function(dark = FALSE, base_family = "Lato", ...) {
  if (dark) {
    bg_color <- "#212121"
    text_color <- "grey97"
    text_color_light <- "grey90"
    line_color <- "grey50"
  } else {
    bg_color <- "grey98"
    text_color <- "grey25"
    text_color_light <- "grey45"
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
        family = "Noto Serif", face = "bold",
        size = 16, 
        margin = margin(t = 16, b = 10),
        lineheight = 1.3
      ),
      plot.subtitle = element_textbox_simple(
        family = "Lato Light",
        size = 6.5, color = "grey10",
        margin = margin(b = 8),
        lineheight = 1.3
      ),
      plot.caption = element_markdown(
        hjust = 0,
        margin = margin(t = 10, b = 6),
        color = text_color_light,
        lineheight = 1.2,
        size = 5
      ),
      strip.text = element_text(family = "Source Sans Pro", 
                                face = "bold",
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
      plot.margin = margin(l = 6, r = 6, b = 6),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}


theme_set(theme_custom(dark = FALSE))


filepath_data <- here("2021", "38", "data", "tuesdata.rds")

if (!file.exists(filepath_data)) {
  tuesdata <- tidytuesdayR::tt_load(2021, week = 38)
  write_rds(tuesdata, filepath_data)
} else {
  tuesdata <- read_rds(filepath_data)
}

str(tuesdata)

## Prepare Billboard dataset =======
billboard <- tuesdata$billboard %>% 
  mutate(week = mdy(week_id)) %>% 
  select(-c(url, week_id))


## LONGEST TIME UNTIL RETURN TO BILLBOARD ========

billboard %>% 
  filter(performer == "Whitney Houston", song == "I Will Always Love You") %>% 
  select(week, week_position) %>% arrange(week) %>% 
  View()

bb_time <- billboard %>% 
  group_by(performer, song) %>% 
  # exclude songs which entered the charts only once
  filter(n() > 1) %>% 
  arrange(week, .by_group = TRUE) %>% 
  mutate(previous_week_on_bb = lag(week),
         days_since_last_entry = week - previous_week_on_bb) %>% 
  ungroup() %>% 
  select(song, performer, week, previous_week_on_bb, days_since_last_entry) %>% 
  # only keep songs which re-entered the charts at least one year later
  filter(days_since_last_entry > 365)


bb_time %>% 
  count(performer, song, sort = TRUE)

plot_palette <- c("Death of Artist" = "grey10", "Christmas" = "#BB2528",
                  "Movie" = "darkgreen", "Viral" = "#6E3D99", 
                  "Other" = "#C66644")
titles <- list(
  "title" = "Late return to the Billboard Hot 100",
  "subtitle" = glue(
    "These songs re-entered the charts long after they had dropped out of the 
    Billboard Hot 100.
     <br><br>
    The bars represent the period between the previous last appearance and 
    the return to the Billboard Hot 100.
    <br><br>
      At the top is Chuck Berry's \"Run Rudolph Run,\" which returned to the 
      Billboard Hot 100 after an incredible 60 years.
    <br><br>
    The main reasons that a song re-enters after such a long time are newfound 
    popularity during the 
    <b style='color:{plot_palette[\"Christmas\"]}'>Christmas season</b>,
    <b style='color:{plot_palette[\"Death of Artist\"]}'>the death of an artist</b>, 
    or use in a <b style='color:{plot_palette[\"Movie\"]}'>movie soundtrack</b>.
    <br><br>
    \"Bohemian Rhapsody\" even managed to do this twice: 
    in 1992 thanks to \"Wayne's World\" and in 2018 because of the same-titled 
    movie.
    <br><br>
    \"Dreams\", \"Billie Jean\", and \"Livin' On A Prayer\" made a comeback to 
    the U.S. singles charts thanks to their appearance in 
    <b style='color:{plot_palette[\"Viral\"]}'>
    viral videos</b> (TikTok, YouTube).
    <br><br>
    Finally, Michael Jackson's \"Thriller\" gained new popularity during
    <b style='color:{plot_palette[\"Other\"]}'>
    Halloween</b> and made the Top 100 in 2016.
    "
  ),
  "caption" = "Data: **Data.World**, **Billboard.com** | Visualization: **@_ansgar**"
)

df_plot <- bb_time %>% 
  slice_max(order_by = days_since_last_entry, n = 25) %>% 
  bind_rows(filter(bb_time, song == "Bohemian Rhapsody" & performer == "Queen")) %>% 
  distinct() %>% 
  mutate(years_since_last_entry = interval(previous_week_on_bb, week) / years(1),
         years_since_last_entry = floor(years_since_last_entry),
         performer = case_when(
           performer == "Prince And The Revolution" ~ "Prince",
           performer == "Wham! Featuring George Michael" ~ "Wham!",
           TRUE ~ performer
         ),
         # label = glue("{performer} | {song} | {years_since_last_entry} years"),
         label = glue("{performer} \U2022 {song}"),
         label = str_to_upper(label),
         label = fct_reorder(label, days_since_last_entry, .fun = max),
         label_pos_date = previous_week_on_bb + floor(days_since_last_entry / 2),
         reason = case_when(
           performer %in% c("Whitney Houston", "Wham!", 
                            "Prince And The Revolution", "Prince") | 
             str_detect(performer, "David Bowie|George Michael") 
           ~ "Death of Artist",
           song %in% c("White Christmas", "Jingle Bell Rock", "Run Rudolph Run") |
             str_detect(song, "Christmas") ~ "Christmas",
           song %in% c("Bohemian Rhapsody", "Do You Love Me") ~ "Movie",
           # https://www.youtube.com/watch?v=mOHkRk00iI8
           song == "Livin' On A Prayer" ~ "Viral", 
           # https://studybreaks.com/culture/music/fleetwood-mac-tiktok/
           song == "Dreams" & performer == "Fleetwood Mac" ~ "Viral",
           # https://www.billboard.com/articles/columns/chart-beat/6099413/michael-jackson-billie-jean-hot-100-return-xscape
           song == "Billie Jean" & performer == "Michael Jackson" ~ "Viral",
           TRUE ~ "Other"
         )) 


# The subtitle to be plotted on the left-hand side
p_subtitle <- 
  ggplot() +
  labs(subtitle = titles$subtitle)


# The actual plot
p <- df_plot %>% 
  ggplot() +
  geom_segment(data = . %>% filter(!(song == "Bohemian Rhapsody" & 
                                       week == as_date("1992-03-21"))),
               aes(x = label, xend = label, 
                   y = previous_week_on_bb, yend = week,
                   col = reason),
               size = 0.25, alpha = 0.75, show.legend = FALSE) +
  geom_point(data = . %>% filter(!(song == "Bohemian Rhapsody" & 
                                     week == as_date("1992-03-21"))),
             aes(x = label, y = previous_week_on_bb, fill = reason),
             shape = 21, col = "white", show.legend = FALSE) +
  geom_point(data = . %>% filter(!(song == "Bohemian Rhapsody" & 
                                     week == as_date("1992-03-21"))),
             aes(x = label, y = week, fill = reason),
             shape = 21, col = "white", show.legend = FALSE) +
  geom_text(data = . %>% filter(!(song == "Bohemian Rhapsody" & 
                                    week == as_date("1992-03-21"))),
            aes(label, label_pos_date, label = label),
            size = 1.5, col = "grey15", nudge_x = 0.3, family = "Lato") +
  # 1st appearance of Bohemian Rhapsody
  geom_segment(data = . %>% filter(song == "Bohemian Rhapsody" & 
                                     week == as_date("1992-03-21")),
               aes(x = label, xend = label, 
                   y = previous_week_on_bb, yend = week,
                   col = reason),
               size = 0.25, alpha = 0.2, show.legend = FALSE) +
  geom_point(data = . %>% filter(song == "Bohemian Rhapsody" & 
                                     week == as_date("1992-03-21")),
             aes(x = label, y = previous_week_on_bb, fill = reason),
             shape = 21, col = "white", alpha = 0.2, show.legend = FALSE) +
  scale_color_manual(values = plot_palette, aesthetics = c("color", "fill")) +
  coord_flip() +
  labs(subtitle = NULL) +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank())


# Plot layout for patchwork
plot_design <- "
  122
"

ragg::agg_png(here("2021", "38", "plots", "billboard_reentry.png"),
             width = 2000, height = 1200, res = 300, units = "px")
p_subtitle + p +
  plot_layout(design = plot_design) +
  plot_annotation(title = titles$title,
                  subtitle = NULL,
                  caption = titles$caption) &
  theme(plot.subtitle = element_textbox_simple(margin = margin(t = 5, b = -200)))
invisible(dev.off())

