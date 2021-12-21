# devtools::install_github("BlakeRMills/MetBrewer") 
pacman::p_load("tidyverse", "ggtext", "here", "MetBrewer")


misspellings <- c("Ansga", "Asgar", "Anska", "Anskar", "Oskar")
misspellings <- c("Hansgar", "Anskar", "Ansga", "Asgar", "Anska", "Oskar")
df <- tibble(spelling = misspellings, row = seq_along(misspellings))

y_offset <- 0.75

p_base <- ggplot(df) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh1")) +
  coord_cartesian(ylim = c(y_offset, nrow(df) + y_offset), clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 4, l = 4, r = 4, b = 4),
        plot.background = element_rect(color = NA, fill = "white"))

p_base + 
  geom_textbox(aes(x = 1, y = row, label = rev(toupper(spelling)), col = spelling),
                      family = "Raleway Bold",
                      box.colour = NA, fill = NA, size = 16,
                      show.legend = FALSE)
  
ggsave(here("2021", "52", "plots", "starbucks_names.png"), dpi = 150,
       width = 5, height = 3.5)


# Handwriting
p_base + 
  geom_textbox(aes(x = 1, y = row, label = rev(toupper(spelling)), col = spelling),
               family = "Indie Flower",
               box.colour = NA, fill = NA, size = 16,
               show.legend = FALSE)

ggsave(here("2021", "52", "plots", "starbucks_names-handwritten.png"), dpi = 150,
       width = 3, height = 3)


# with Starbucks color scheme

p_base <- ggplot(df) +
  scale_color_manual(
    values = scales::seq_gradient_pal("white", "black")(seq_len(nrow(df))/nrow(df)))+
  coord_cartesian(ylim = c(y_offset, nrow(df) + y_offset), clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 4, l = 4, r = 4, b = 4),
        plot.background = element_rect(color = NA, fill = "#00704A"))

p_base + 
  geom_textbox(aes(x = 1, y = row, label = rev(toupper(spelling)), col = spelling),
               family = "Indie Flower",
               box.colour = NA, fill = NA, size = 16,
               show.legend = FALSE)

ggsave(here("2021", "52", "plots", "starbucks_names-handwritten-ci.png"), dpi = 150,
       width = 4, height = 3)


