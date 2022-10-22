library(tidyverse)
library(ggstream)
library(ggtext)
library(lubridate)
library(here)

base_path <- here::here("2022", "42")

tuesdata <- tidytuesdayR::tt_load(2022, week = 42)
episodes <- tuesdata$episodes
glimpse(episodes)

dialogues <- tuesdata$stranger_things_all_dialogue
head(dialogues)

# select episode
selected_season <- 4
selected_episode <- 9

dialogues_episode <- subset(dialogues, season == selected_season & episode == selected_episode) 
glimpse(dialogues_episode)

dialogues_episode %>% 
  separate_rows(stage_direction, sep = "\\] \\[") %>% 
  mutate(stage_direction = str_remove_all(stage_direction, "[\\[\\]]")) %>% 
  count(stage_direction, sort = TRUE) %>% View()

# Prepare raw text so that each stage direction constitutes a new line
# This will help assigning speech to a character in the next step.
dialogues_episode_prep <- dialogues_episode %>% 
  select(line, raw_text, start_time, end_time) %>% 
  mutate(raw_text = str_replace_all(raw_text, "\\[", "|[")) %>% 
  separate_rows(raw_text, sep = "\\|") %>% 
  filter(str_length(raw_text) > 0) %>% 
  mutate(stage_direction = str_extract(raw_text, "\\[.+\\]"),
         stage_direction = str_remove_all(stage_direction, "[\\[\\]]"),
         speech_text = str_remove_all(raw_text, "\\[.+\\]") %>% 
           str_squish()) 
head(dialogues_episode_prep)

dialogues_episode_prep %>% 
  count(stage_direction, sort = TRUE) %>% View()


# S1E1
characters_episode <- c("Lucas", "Dustin", "Mike", "Will", "Joyce", "Callahan", "Jonathan",
                "Mr. Clarke", "Hopper", "Karen", "man", "man 1", "Ben", "man 2", 
                "woman", "Barbara", "boy", "Liz", "Lonnie", "Nancy", "Steve", "Ted", 
                "Troy", "woman 1", "woman 2", "woman 3", "woman 4", "woman on TV")

# S4E9 + S1E1
characters_episode <- c("Lucas", "Dustin", "Mike", "Will", "Joyce", "Callahan", "Jonathan",
                        "Mr. Clarke", "Hopper", "Karen", "man", "man 1", "Ben", "man 2", 
                        "woman", "Barbara", "boy", "Liz", "Lonnie", "Nancy", "Steve", "Ted", 
                        "Troy", "woman 1", "woman 2", "woman 3", "woman 4", "woman on TV", 
                        "Murray", "Eddie", "Vecna", "Robin", "Argyle", "Eleven", 
                        "Max", "Antonov", "Armstrong", "Fitzgerald", "Yuri", "Billy", 
                        "Jason", "Holly", "kids", "Mr. Munson", "volunteer")


character_word_count <- dialogues_episode_prep %>% 
  filter(str_length(speech_text) > 1) %>% 
  mutate(speaker = ifelse(stage_direction %in% characters_episode, stage_direction, NA)) %>% 
  fill(speaker, .direction = "down") %>% 
  mutate(start_time_s = as.numeric(start_time, "seconds"),
         start_time_m = start_time_s %/% 60) %>% 
  tidytext::unnest_tokens(word, speech_text, token = "words", drop = FALSE) %>% 
  count(line, speaker, raw_text, start_time, start_time_s, start_time_m, end_time, 
        stage_direction, speech_text,
        name = "n_words") %>% 
  count(start_time_m, speaker,  wt = n_words, name = "n_words") 

df_plot <- character_word_count %>% 
  nest(data = -c(speaker)) %>% 
  mutate(occurences = map_dbl(data, nrow),
         speaker = ifelse(occurences > 10, speaker, "Other")
         ) %>% 
  # filter(occurences > 3) %>% 
  unnest(cols = c(data)) %>% 
  count(start_time_m, speaker, wt = n_words, name = "n_words") %>% 
  mutate(speaker = fct_relevel(speaker, "Other", after = Inf))

# Create a custom palette with Other as grey
custom_red_palette <- c(
  colorspace::sequential_hcl(n = length(unique(df_plot$speaker)) - 1, palette = "Reds"),
  "grey60"
  )

df_plot %>% 
  ggplot(aes(start_time_m, n_words, fill = speaker)) +
  geom_stream(bw = 0.75, extra_span = 0.02) +
  annotate(
    GeomRichtext,
    x = 30, y = 400, label = "DRAFT NOT ACCURATE", size = 8, angle = 6, hjust = 0,
    fill = alpha("deeppink", 0.8), color = "white", label.padding = unit(4, "mm"),
    fontface = "bold"
  ) +
  scale_fill_manual(values = custom_red_palette) +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  labs(
    title = "Who speaks when in Stranger Things Season 4 finale?",
    fill = NULL
  ) +
  theme_void(base_family = "Merriweather") +
  theme(
    plot.background = element_rect(color = "grey2", fill = "grey2"),
    legend.position = "bottom",
    text = element_text(color = "grey92"),
    plot.title = element_text(face = "bold", size = 16),
    plot.margin = margin(rep(5, 4))
  )
ggsave(here(base_path, "plots", "stranger-things-speech-s4s9.png"), dpi = 500, 
       width = 8, height = 6)
