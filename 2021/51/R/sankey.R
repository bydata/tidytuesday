pacman::p_load("tidyverse", "tidytuesdayR",  "ggtext", "ggsankey", "here", "glue")

filepath_data <- here("2021", "51", "data", "tuesdata.rds")

if (!file.exists(filepath_data)) {
  tuesdata <- tidytuesdayR::tt_load("2021-12-14")
  write_rds(tuesdata, filepath_data)
} else {
  tuesdata <- read_rds(filepath_data)
}

str(tuesdata)

tuesdata$studio_album_tracks %>% 
  count(album_name)

lyrics <- tuesdata$lyrics

lyrics %>% 
  arrange(song_id, line_number) %>% 
  distinct(song_name, section_name) %>% 
  group_by(song_name) %>% 
  mutate(section_id = row_number()) %>% 
  ungroup()

# section orders frequencies
lyrics %>% 
  arrange(song_id, line_number) %>% 
  distinct(song_name, section_name) %>% 
  group_by(song_name) %>% 
  summarize(sections = paste(section_name, collapse = "|")) %>% 
  count(sections, sort = TRUE)


# Prepare song structure data for Sankey plot
df_sankey <- lyrics %>% 
  arrange(song_id, line_number) %>% 
  select(song_id, song_name, section_name, line_number) %>% 
  group_by(song_id) %>% 
  filter(section_name != lag(section_name) | is.na(lag(section_name))) %>%
  ungroup() %>% 
  select(song_name, section_name) %>% 
  # Exclude "opening"
  filter(!section_name %in% c("Opening", "Melanie B:")) %>% 
  # Recode "Refrain" to "Chorus"
  mutate(section_name = case_when(
    section_name == "Refrain" ~ "Chorus", 
    str_detect(section_name, "^Pre-Chorus") ~ "Pre-Chorus",
    str_detect(section_name, "^Verse") ~ "Verse",
    section_name == "Spoken Break" ~ "Spoken",
    TRUE ~ section_name)
  ) %>%
  group_by(song_name) %>% 
  mutate(section_id = row_number()) %>% 
  ungroup() %>%
  pivot_wider(
    id_cols = song_name, names_from = "section_id", 
    values_from = "section_name", names_prefix = "section_") %>% 
  select(-song_name) %>% 
  make_long(section_1, section_2, section_3, section_4, section_5, section_6,
            section_7, section_8, section_9, section_10, section_11) %>% 
  filter(!is.na(node), x != "song_name") %>% 
  mutate(node = fct_inorder(node))


# Color palette
spice_colors <- c("#52449c", "#d64978", "#e0ed53", "#f5e449", "#6de4ed")


plot_titles <- list(
  #title = "SPICE SONG STRUCTURES",
  title = "**SPICE** SONG STRUCTURES",
  subtitle = glue(
    "
  Song structure refers to how a song is organized, using a combination of different sections.
  A basic song structure includes 
  <b style='color:{spice_colors[2]}'>verses</b>,
  a <b style='color:{spice_colors[1]}'>chorus</b>,
  and a <b style='color:{colorspace::darken(spice_colors[5])}'>bridge</b>.
  This sankey plot shows the sequence of sections in all **31 Spice Girls songs**
  published on their 3 studio albums.
  The **height of the segments** indicates the frequency of songs which follow the respective structure.
  "
  ),
  caption = "Source: **Genius**, collected by **Jacquie Tran** | Visualization: **Ansgar Wolsing** |
  #TidyTuesday week 51"
)

ggplot(df_sankey, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      label = node)) +
  geom_sankey(aes(fill = node),
              flow.alpha = 0.75,
              node.size = 0.3,
              alpha = 1,
              smooth = 6
  ) +
  geom_sankey_label(
    size = 2, color = "white", fill = "grey34", alpha = 0.8,
    label.r = unit(0.6, "mm"),
    family = "Raleway SemiBold") +
  scale_x_discrete(position = "top") +
  scale_fill_manual(
    values = c("Verse" = spice_colors[2], 
               "Chorus" = spice_colors[1],
               "Pre-Chorus" = spice_colors[3],
               "Intro" = spice_colors[4], 
               "Bridge" = spice_colors[5]
    )
  ) +
  guides(fill = guide_legend(title.position = "left")) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption,
       x = "From start to end \u27A4",
       y = "\u29CB",  #"\u25B7",
       fill = "Section"
  ) +
  theme_sankey(base_family = "Raleway") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey96"),
    panel.background = element_rect(color = NA, fill = NA),
    text = element_text(color = "grey29"),
    plot.title = element_markdown(
      color = "#500C5F", size = 18,
      # family = "Anton", face = "plain",
      family = "Helvetica Neue",
      margin = margin(t = 4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      size = 7, color = "grey12",
      margin = margin(t = 8, b = 18, r = 12)
    ),
    panel.spacing = margin(b = 0),
    plot.margin = margin(l = 8, t = 4, b = 4, r = 4),
    plot.caption = element_textbox_simple(
      size = 6, margin = margin(t = 12, b = 2)),
    axis.title.x = element_blank(),
    # axis.title.x.top = element_text(hjust = 0, size = 6,
    #                                 margin = margin(b = 0)),
    axis.title.y = element_text(size = 24, family = "Noto Sans Math",
                                angle = 270, vjust = 2, hjust = 0.5,
                                color = colorspace::lighten("#500C5F", 0.2),
                                #"grey49", 
                                # margin = margin(l = 8, r = 0)
                                ),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 7, face = "bold", color = "grey50"),
    legend.text = element_text(size = 6),
    legend.key.size = unit(4, "mm"),
    legend.margin = margin(t = 0),
    legend.background = element_rect(fill = NA)
  )

ggsave(here("2021", "51", "plots", "spice_sankey.png"), dpi = 200, device = ragg::agg_png,
       width = 7, height = 5)

