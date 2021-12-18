pacman::p_load("tidyverse", "tidytuesdayR", "lubridate",
               "ggtext", "ggforce", "here", "glue")

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


lyrics %>% 
  arrange(song_id, line_number) %>% 
  distinct(song_name, section_name) %>% 
  group_by(song_name) %>% 
  mutate(section_id = row_number()) %>% 
  ungroup() %>% 
  count(section_name, section_id)



data <- reshape2::melt(Titanic)
data <- gather_set_data(data, 1:4)

ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')


lyrics %>% 
  arrange(song_id, line_number) %>% 
  select(song_id, song_name, section_name, line_number) %>% 
  group_by(song_id) %>% 
  filter(section_name != lag(section_name)) %>%
  ungroup() %>% 
  select(song_name, section_name) %>% 
  # Exclude "opening"
  filter(!section_name %in% c("Opening", "Melanie B:")) %>% 
  # Recode "Refrain" to "Chorus"
  mutate(section_name = case_when(
    section_name == "Refrain" ~ "Chorus", 
    str_detect(section_name, "^Pre-Chorus") ~ "Pre-Chorus",
    TRUE ~ section_name)
    ) %>%
  # mutate(section_name = fct_recode(section_name, "I" = "Intro",
  #                                  "V" = "Verse 1",
  #                                  "V" = "Verse 2",
  #                                  "V" = "Verse 3",
  #                                  "B" = "Bridge",
  #                                  "C" = "Chorus",
  #                                  "P" = "Pre-Chorus",
  #                                  "O" = "Outro",
  #                                  "PoC" = "Post-Chorus",
  #                                  "SB" = "Spoken Break")) %>% 
  group_by(song_name) %>% 
  mutate(section_id = row_number()) %>% 
  ungroup() %>%
  pivot_wider(
    id_cols = song_name, names_from = "section_id", 
    values_from = "section_name", names_prefix = "section_") %>% 
  mutate(across(starts_with("section_"), ~replace_na(., ""))) %>% 
  select(-song_name) %>% 
  group_by(across(everything())) %>% 
  summarize(n = n()) %>% 
  arrange(-n) %>%
  gather_set_data(1:11) %>% 
  mutate(x = factor(x, levels = paste0("section_", 1:11))) %>% 
  # mutate(y = fct_recode(y, "I" = "Intro",
  #                                  "V" = "Verse 1",
  #                                  "V" = "Verse 2",
  #                                  "V" = "Verse 3",
  #                                  "B" = "Bridge",
  #                                  "C" = "Chorus",
  #                                  "P" = "Pre-Chorus",
  #                                  "P" = "Pre-Chorus 1",
  #                                  "P" = "Pre-Chorus 2",
  #                                  "O" = "Outro",
  #                                  "PoC" = "Post-Chorus",
  #                                  "SB" = "Spoken Break")) %>%
  # mutate(y = str_sub(y, 1, 1)) %>% 
  filter(!is.na(y)) %>% 
  ggplot(aes(x, id = id, split = y, value = n)) +
  geom_parallel_sets(aes(fill = section_1), 
                     na.rm = TRUE, alpha = 0.5) +
  geom_parallel_sets_axes(axis.width = 0.2, fill = "grey40") +
  geom_parallel_sets_labels(colour = "white", family = "Futura Medium",
                            size = 3, angle = 90) +
  scale_fill_manual(values = spice_colors) + 
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = "white"),
        legend.position = "bottom")

ggsave(here("2021", "51", "plots", "song_patterns.png"), 
       dpi = 200, width = 7, height = 5)  



spice_colors <- c("#A4E033", "#6C2B35", "#DACDC1", "#C43527", "#1B0205", "#8F6735")


data <- lyrics %>% 
  arrange(song_id, line_number) %>% 
  select(song_id, song_name, section_name, line_number) %>% 
  group_by(song_id) %>% 
  filter(section_name != lag(section_name)) %>%
  mutate(section_id = row_number(),
         previous_section_name = lag(section_name)) %>% 
  ungroup() %>% 
  # select(song_name, section_id, section_name, previous_section_name) %>% 
  filter(!is.na(previous_section_name)) %>% 
  select(from = previous_section_name, to = section_name)

mygraph <- graph_from_data_frame(data)
mygraph <- tidygraph::as_tbl_graph(data)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  geom_node_text(aes(label = name)) +
  theme_void()


library(ggsankey)

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


# spice_colors <- c("#363152", "#A64D6B", "#CAD270", "#A2E6EB", "#EDE17A")
spice_colors <- c("#52449c", "#d64978", "#e0ed53", "#f5e449", "#6de4ed")


plot_titles <- list(
  title = "SPICE SONG STRUCTURES",
  subtitle = "
  Song structure refers to how a song is organized, using a combination of different sections. 
  A basic song structure includes a verse, a chorus, and a bridge.
  This sankey plot shows the sequence of sections in all 31 Spice Girls songs
  published on their 3 studio albums.
  ",
  caption = "Source: **Genius**, **Jacquie Tran** | Visualization: **Ansgar Wolsing** |
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
  scale_fill_manual(
    values = c("Verse" = spice_colors[2], 
               "Chorus" = spice_colors[1],
               "Pre-Chorus" = spice_colors[3],
               "Intro" = spice_colors[4], 
               "Bridge" = spice_colors[5]
    )
  ) +
  guides(fill = guide_legend(title.position = "top")) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption,
       fill = "Section"
  ) +
  theme_sankey(base_family = "Raleway") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey96"),
    panel.background = element_rect(color = NA, fill = NA),
    text = element_text(color = "grey29"),
    plot.title = element_markdown(
      color = "#500C5F", size = 24,
      family = "Anton", face = "plain",
      margin = margin(t = 4)),
    plot.subtitle = element_textbox_simple(
      size = 9,
      margin = margin(t = 6, b = 8)
    ),
    panel.spacing = margin(b = 0),
    plot.caption = element_textbox_simple(
      size = 6, margin = margin(t = 12, b = 2)),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(4, "mm"),
    legend.margin = margin(t = 0),
    legend.background = element_rect(fill = NA)
  )

ggsave(here("2021", "51", "plots", "spice_sankey.png"), dpi = 200, device = ragg::agg_png,
       width = 7, height = 5)

