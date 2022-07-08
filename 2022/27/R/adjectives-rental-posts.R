library(tidyverse)
library(ggtext)
library(here)
library(tidytext)
library(textstem)
library(udpipe)
library(ggdist)
library(glue)
library(patchwork)

base_path <- here::here("2022", "27")

tuesdata <- tidytuesdayR::tt_load(2022, week = 27)
rent <- tuesdata$rent

## PREPRARE TEXT  -------------------------------------------------

# Stopwords
stopwords_iso_en <- stopwords::stopwords("en", "stopwords-iso")

# Tokenize words
rent_title_words <- rent %>% 
  select(post_id, year, title, price, beds) %>% 
  unnest_tokens(word, title, token = "words") %>% 
  filter(!word %in% stopwords_iso_en)

# unique words from titles
rent_title_words_unique <- unique(rent_title_words$word)
length(rent_title_words_unique)

# Download language model
dl <- udpipe_download_model(language = "english-ewt")
udmodel_en <- udpipe_load_model(file = dl$file_model)

# Annotate words to identify adjectives
words_annotated <- udpipe_annotate(udmodel_en, rent_title_words_unique)
words_annotated_df <- as.data.frame(words_annotated)
adjectives <- subset(words_annotated_df, upos == "ADJ")

# average price over all rental posts
mean_price <- mean(rent$price, na.rm = TRUE)
# number of rental posts
n_rental_posts <- nrow(subset(rent, !is.na(title)))

bg_color <- "grey97"
font_family <- "Fira Sans"

plot_subtitle = glue("Adjectives used to describe houses and apartments
in the San Francisco Bay Area in the titles of rental posts on Craigslist and 
how they are related to rental prices. Titles from 
{scales::number(n_rental_posts, big.mark = ',')} rental posts on Craiglists 
between 2000 and 2018. 
The 15 most frequent adjectives are shown.
")

p <- rent_title_words %>% 
  # keep only the adjectives - exclude some mis-coded words manually
  filter(!word %in% c("san", "pic", "flat")) %>% 
  semi_join(adjectives, by = c("word" = "token")) %>% 
  # add_count(word, name = "word_total") %>% 
  group_by(word) %>% 
  mutate(word_total = n(),
         mean_price = mean(price),
         mean_beds = mean(beds, na.rm = TRUE)) %>% 
  ungroup() %>% 
  nest(data = -c(word, word_total)) %>% 
  slice_max(word_total, n = 15) %>% 
  unnest(cols = data) %>% 
  mutate(word = fct_reorder(word, -mean_price)) %>% 
  ggplot(aes(word, price)) +
  stat_halfeye(fill_type = "segments", alpha = 0.3) +
  stat_interval() +
  stat_summary(geom = "point", fun = mean) +
  # Annotate the average number of beds
  annotate("text", x = 16, y = 0, label = "(\U00F8 bedrooms)",
           family = "Fira Sans", size = 3, hjust = 0.5) +
  stat_summary(
    aes(y = beds),
    geom = "text",
    fun.data = function(x) {
      data.frame(
        y = 0,
        label = sprintf("(%s)", scales::number(mean(ifelse(x > 0, x, NA), na.rm = TRUE), accuracy = 0.1)))},
    family = font_family, size = 2.5
    ) +
  geom_hline(yintercept = mean_price, col = "grey30", lty = "dashed") +
  annotate("text", x = 16, y = mean_price + 50, label = "Average rent",
           family = "Fira Sans", size = 3, hjust = 0) +
  scale_x_discrete(labels = toupper) +
  scale_y_continuous(breaks = seq(2500, 20000, 2500)) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  coord_flip(ylim = c(0, 10000), clip = "off") +
  # remove default legend
  guides(col = "none") +
  labs(
    title = toupper("Nice and Clean - Relatively Low Rent?"),
    subtitle = plot_subtitle,
    caption = "Axis capped at 10,000 USD.<br>
    Data: Pennington, Kate (2018). 
    Bay Area Craigslist Rental Housing Posts, 2000-2018.<br>
    Retrieved from github.com/katepennington/historic_bay_area_craigslist_housing_posts/blob/master/clean_2000_2018.csv.zip.
    <br>
    Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Rent in USD"
  ) +
  theme_minimal(base_family = font_family) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(size = 0.1, color = "grey75"),
    plot.title = element_text(family = "Fira Sans SemiBold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 4, b = 16), size = 10),
    plot.caption = element_textbox_simple(
      margin = margin(t = 12), size = 7
    ),
    plot.caption.position = "plot",
    # move the axis text a bit into the panel
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = "Fira Sans SemiBold"),
    plot.margin = margin(4, 4, 4, 4)
  )
p


## Custom plot legend

df_for_legend <- rent_title_words %>% 
  filter(word == "beautiful")

p_legend <- df_for_legend %>% 
  ggplot(aes(word, price)) +
  stat_halfeye(fill_type = "segments", alpha = 0.3) +
  stat_interval() +
  stat_summary(geom = "point", fun = mean) +
  annotate(
    "richtext",
    x = c(0.8, 0.8, 0.8, 1.4, 1.8),
    y = c(1000, 5000, 3000, 2400, 4000),
    label = c("50 % of prices<br>fall within this range", "95 % of prices", 
              "80 % of prices", "Mean", "Distribution<br>of prices"),
    fill = NA, label.size = 0, family = font_family, size = 2, vjust = 1,
  ) +
  geom_curve(
    data = data.frame(
      x = c(0.7, 0.80, 0.80, 1.225, 1.8),
      xend = c(0.95, 0.95, 0.95, 1.075, 1.8), 
      y = c(1800, 5000, 3000, 2300, 3800),
      yend = c(1800, 5000, 3000, 2300, 2500)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  # scale_y_continuous(limits = c(NA, 6500)) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  coord_flip(xlim = c(0.75, 1.3), ylim = c(0, 6000), expand = TRUE) +
  guides(color = "none") +
  labs(title = "Legend") +
  theme_void(base_family = font_family) +
  theme(plot.title = element_text(family = "Fira Sans SemiBold", size = 9,
                                  hjust = 0.075),
        plot.background = element_rect(color = "grey30", size = 0.2, fill = bg_color))
p_legend

# Insert the custom legend into the plot
p + inset_element(p_legend, l = 0.6, r = 1.0,  t = 0.99, b = 0.7, clip = FALSE)
ggsave(here(base_path, "plots", "rental-posts-adjectives-price-custom-legend.png"),
       dpi = 400, width = 7, height = 8)

