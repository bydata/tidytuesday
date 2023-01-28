library(tidyverse)
library(ggtext)
library(here)
library(waffle)

tuesdata <- tidytuesdayR::tt_load("2023-01-17")
artists <- tuesdata$artists

skimr::skim(artists)

artists %>% 
  filter(book == "Gardner") %>% 
  count(year)

artists %>% 
  filter(book == "Janson") %>% 
  count(year)

artists %>% 
  group_by(book, year) %>% 
  summarize(share_female = mean(artist_gender == "Female"),
            .groups = "drop") %>% 
  ggplot(aes(year, share_female, col = book)) +
  geom_step() +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_minimal()

artists %>% 
  mutate(artist_first_name = str_extract(artist_name, "[^\\s]+")) %>% 
  select(artist_name, artist_first_name) %>% 
  count(artist_first_name, sort = TRUE)


# Prepare the dataset for the waffle charts
df_waffle <- artists %>% 
  filter((year == 1936 | year == max(year)) &  book == "Gardner") %>% 
  # extract first names
  mutate(artist_first_name = str_extract(artist_name, "[^\\s]+"),
         artist_first_name_john = artist_first_name == "John") %>% 
  count(year, artist_gender, artist_first_name_john, 
        wt = space_ratio_per_page_total,
        name = "space_share") %>% 
  mutate(
    group = case_when(
      artist_first_name_john & artist_gender == "Male" ~ "John (male)",
      artist_gender == "Male" ~ "Male",
      artist_gender == "Female" ~ "Female",
      artist_gender == "N/A" ~ "Unknown"
    ),
    group = factor(group, levels = c("Male", "John (male)", "Female", "Unknown"))
  ) %>% 
  arrange(group)


bg_color <- "#8380B6"

df_waffle %>% 
  ggplot(aes(values = space_share, fill = factor(group))) +
  geom_waffle(n_rows = 10, make_proportional = TRUE, col = bg_color, size = 1.2,
              flip = FALSE, radius = unit(1, "mm")) +
  geom_label(
    data = data.frame(
      year = c(1936, 1936, 2020),
      label = c("Male artists", "Male artists named John", "Female artists"), 
      x = c(2, 2, 5), 
      y = c(8, 4.5, 7),
      color = c("grey40", "grey40", "#FA9500")
      ),
    aes(x, y, label = label, color = color), inherit.aes = FALSE,
    family = "Roboto Condensed", fontface = "bold", hjust = 0, fill = alpha("white", 0.8),
    label.size = 0
  ) + 
  geom_curve(
    data = data.frame(
      year = c(1936, 2020),
      x = c(8, 8.2),
      xend = c(9, 9.2),
      y = c(5, 6.5),
      yend = c(6.75, 5)
    ),
    aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE,
    curvature = 0.2, arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"),
    linewidth = 0.3, color = "grey20"
  ) +
  scale_fill_manual(
    values = c("grey82", "white", "#FA9500", colorspace::darken(bg_color, 0.3))) +
  scale_color_identity() +
  coord_equal() +
  facet_wrap(vars(year)) +
  labs(
    title = "1936: More Space for Johns than for Women",
    subtitle = "*John* is the most frequent name of artists identified as male
    in the 1936 edition of Gardner's *Art Through the Ages*. 
    The rectangles represent the area of both the text and the figure 
    by gender (and first name John) in the editions in 1936 and 2020.",
    caption = "Unknown refers to pieces by unknown artists.<br>
    Source: Lemus S, Stam H (2022). arthistory: Art History Textbook Data.
    Visualisation: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_void(base_family = "Crimson Text") +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    legend.position = "bottom",
    text = element_text(color = "white", lineheight = 1.1),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_textbox(
      hjust = 0.5, halign = 0.5, width = 0.95, size = 12,
      margin = margin(t = 4, b = 18)),
    plot.caption = element_markdown(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 14),
    plot.margin = margin(rep(4, 4))
  )
ggsave(here("2023", "03", "plots", "waffle-1936-2020.png"), width = 7, height = 4.5)
