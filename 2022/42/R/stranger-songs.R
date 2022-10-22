library(tidyverse)
library(ggstream)
library(ggtext)
library(lubridate)
library(here)
library(colorspace)

base_path <- here::here("2022", "42")

tuesdata <- tidytuesdayR::tt_load(2022, week = 42)
episodes <- tuesdata$episodes
glimpse(episodes)

dialogues <- tuesdata$stranger_things_all_dialogue
head(dialogues)


# Prepare raw text so that each stage direction constitutes a new line
# This will help assigning speech to a character in the next step.
dialogues_prep <- dialogues %>% 
  select(season, episode, line, raw_text, start_time, end_time) %>% 
  mutate(raw_text = str_replace_all(raw_text, "\\[", "|[")) %>% 
  separate_rows(raw_text, sep = "\\|") %>% 
  filter(str_length(raw_text) > 0) %>% 
  mutate(stage_direction = str_extract(raw_text, "\\[.+\\]"),
         stage_direction = str_remove_all(stage_direction, "[\\[\\]]"),
         speech_text = str_remove_all(raw_text, "\\[.+\\]") %>% 
           str_squish()) 

# Find MUSIC playing
songs <- dialogues_prep %>% 
  select(season, episode, line, start_time, end_time, stage_direction) %>% 
  filter(str_detect(stage_direction, "play(ing|s)")) %>% # View()
  # count(stage_direction, sort = TRUE) %>% 
  # song titles are written in quotation marks
  filter(str_detect(stage_direction, "\"")) %>% 
  mutate(
    # song = str_remove_all(stage_direction, "play(ing|s)|(on stereo)|continues"),
    song = str_extract(stage_direction, "\".+\""),
    song = str_remove_all(song, "\""),
    # make consistent spelling
    song = str_to_title(song),
    song = ifelse(song == "Dream A Little Dream", "Dream A Little Dream Of Me", song),
    song = ifelse(song == "Should I Stay Or Should I Go?", "Should I Stay Or Should I Go", song),
    artist = str_extract(stage_direction, "by .+"),
    artist = str_remove_all(artist, "by|play(ing|s)"),
    artist = str_squish(artist)) %>% 
  select(-stage_direction)
  

# "Repair" missing artist names
songs_artists <- songs %>% 
  filter(!is.na(artist)) %>% 
  distinct(song, artist)

songs <- songs %>% 
  left_join(songs_artists, by = "song") %>% 
  mutate(artist = ifelse(is.na(artist.x), artist.y, artist.x)) %>% 
  select(-c(artist.x, artist.y))


songs %>% 
  count(song, artist, sort = TRUE)

# aggregate the same playback of a song
threshold_seconds <- 4 * 60
songs_playbacks <- songs %>% 
  arrange(season, episode, start_time) %>% 
  group_by(season, episode, song, artist) %>% 
  mutate(time_diff_to_prev_line = (start_time - lag(end_time)) %>% as.numeric(),
         time_diff_to_prev_line = replace_na(time_diff_to_prev_line, 9999)) %>% 
  filter(time_diff_to_prev_line >= threshold_seconds) %>% 
  ungroup() %>% 
  select(-time_diff_to_prev_line)

bg_color <- "grey8"
main_color <- "#B1281E"
red_palette <- c(
  main_color,
  lighten(main_color, seq(0.2, 0.8, 0.3))
)

ragg::agg_png(here(base_path, "plots", "stranger-songs.png"), res = 300, 
              width = 6, height = 7.5, units = "in")
  songs_playbacks %>% 
    count(season, episode, song, artist, sort = TRUE) %>% 
    add_count(song, artist, wt = n, name = "total") %>% 
    filter(total > 1) %>% 
    mutate(song = fct_reorder(song, total)) %>% 
    ggplot(aes(song, n)) +
    ggfx::with_blur(
      ggchicklet::geom_chicklet(
        aes(group = paste(season, episode), fill = factor(season)), 
        col = bg_color, size = 1, width = 0.4,
        radius = unit(3, "pt")),
      sigma = 2, color = "white"
      ) +
    geom_text(aes(label = song, x = as.numeric(song) + 0.4, y = 0), 
              hjust = 0, stat = "unique", family = "Avenir", color = "white") +
    geom_text(
      data = ~distinct(., song, artist, total),
      aes(label = total, y = total), 
      stat = "unique", color = "grey80", hjust = 0, nudge_y = 0.15, family = "Avenir"
    ) +
    # custom title & subtitle
    ggfx::with_inner_glow(
      shadowtext::geom_shadowtext(
        data = NULL,
        aes(x = 13.5, y = 4.5, label = "STRANGER\nSONGS"), 
        # aes(x = 1, y = 0, label = "STRANGER\nSONGS"),
        family = "Benguiat", color = bg_color, bg.color = main_color, size = 12,
        hjust = 0.5, vjust = 1, inherit.aes = FALSE, lineheight = 0.8),
      sigma = 3, expand = 2, color = main_color
    ) +
    annotate(
      GeomTextBox,
      x = 11.5, y = 4.5, 
      label = sprintf("Music is an essential feature in Stranger Things to create the 1980s mood. 
      In <b style='color:%s'>season 4</b>, Kate Bush's **Running Up That Hill**
      becomes an integrated part of the plot.
      Songs which appear more than once in season 1 to 4 are shown.
      Each rectangle represents an episode which features the song.                 
                      ", red_palette[4]),
      hjust = 0.5, halign = 0, vjust = 1, family = "Avenir", color = "grey90", 
      fill = NA, box.size = 0, width = 0.85) +
    scale_y_continuous(breaks = seq(2, 20, 2)) +
    scale_fill_manual(values = red_palette) +
    coord_flip(clip = "off") +
    labs(
      caption = sprintf("Song playbacks are considered separate instances if 
      there is a threshold of %d seconds<br>between each appearance.
      Data: 8flix.com, prepared by Dan Fellowes & Jonathan Kitt.<br>
      Visualisation: Ansgar Wolsing", threshold_seconds),
      fill = "Season") +
    theme_minimal() +
    theme(
      plot.background = element_rect(color = bg_color, fill = bg_color),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom",
      text = element_text(color = "grey90"),
      plot.margin = margin(t = 8, 4, 4, 4),
      plot.caption = element_markdown(lineheight = 1, hjust = 0.5)
    )
invisible(dev.off())
