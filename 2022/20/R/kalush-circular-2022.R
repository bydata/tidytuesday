library(tidyverse)
library(ggtext)
library(here)
library(lubridate)
library(glue)

base_path <- here::here("2022", "20")


## DATA PREP -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 20)

#' Data dictionary:
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-05-17#data-dictionary

votes <- tuesdata$`eurovision-votes`
# need to remove values from Belarus -> Russia in 2019 with 0 points
votes <- votes %>% 
  filter(!(year == 2019 & semi_final == "f" & from_country == "Belarus" & to_country == "Russia"))



# reshape data with televoting and jury votes in columns
votes2016f_wide <- votes %>% 
  filter(semi_final == "f", year >= 2016) %>%
  select(year, jury_or_televoting, from_country, to_country, points) %>% 
  pivot_wider(id_cols = c(year, from_country, to_country), 
              names_from = "jury_or_televoting", names_prefix = "vote_",
              values_from = "points")

# Serbia's votes are incorrect (all zero)
# replace with correct values
serbia_votes_2022 <- 
  structure(list(
    from_country = c("rs", "rs", "rs", "rs", "rs", "rs", "rs", "rs", "rs", "rs", 
                     "rs", "rs", "rs", "rs", "rs", "rs"), 
    to_country = c("gb", "fr", "cz", "it", "lt", "ro", "fi", "se", 
                    "pt", "ua", "be", "no", "az", "md", "es", "ee"), 
    total_points = c(1L, 2L, 3L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L, 8L, 12L, 12L, 14L, 16L), 
    jury_points = c(1L, 0L, 3L, 0L, 2L, 0L, 6L, 5L, 7L, 0L, 8L, 0L, 12L, 0L, 4L, 10L), 
    televoting_points = c(0L, 2L, 0L, 4L, 3L, 5L, 0L, 1L, 0L, 7L, 0L, 8L, 0L, 12L, 10L, 6L)),
    class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -16L)) %>% 
  mutate(year = 2022,
         from_country = countrycode::countrycode(from_country, origin = "iso2c", destination = "country.name"),
         to_country = countrycode::countrycode(to_country, origin = "iso2c", destination = "country.name")) %>% 
  select(year, from_country, to_country, vote_J = jury_points, vote_T = televoting_points)

votes2016f_wide_corrected <- votes2016f_wide %>% 
  # remove incorrect records
  filter(!(year == 2022 & from_country == "Serbia")) %>% 
  bind_rows(serbia_votes_2022)

# get flag emojis in different formats
country_to_flag_emoji <- function(x) {
  x[which(x %in% c("Czech Republic"))] <- "czech_republic"
  x[which(x %in% c("San Marino"))] <- "san_marino"
  x[which(x %in% c("North Macedonia"))] <- "macedonia"
  emoji <- countrycode::countrycode(x, origin = "country.name", destination = "iso2c") %>% 
    tolower() %>% 
    emojifont::emoji()
  emoji[is.na(emoji)] <- tolower(x[is.na(emoji)]) %>% 
    emojifont::emoji()
  emoji
}


plot_df <- votes2016f_wide_corrected %>% 
  filter(year == 2022, to_country == "Ukraine") %>% 
  mutate(flag_emoji = country_to_flag_emoji(from_country)) %>% 
  pivot_longer(cols = c("vote_J", "vote_T"), names_to = "vote", values_to = "points") %>% 
  mutate(flag_emoji = fct_reorder(flag_emoji, points)) 

# countries with 0 jury points for UA
n_zero_jury_points <- length(which(plot_df$vote == "vote_J" & plot_df$points == 0))
# countries with 12 televoting points for UA
n_12_televoting_points <- length(which(plot_df$vote == "vote_T" & plot_df$points == 12))
# minimum points from televoting
min_televoting_points <- min(plot_df$points[which(plot_df$vote == "vote_T")])


ua_colors <- c("#0057B8", "#FFD700")
font_family <- "Poppins"
bg_color <- "grey70"


ragg::agg_png(here(base_path, "plots", "kalush-2022-votes.png"),
              res = 250, width = 1600, height = 1800, bg = bg_color)
plot_df %>% 
  ggplot(aes(flag_emoji, points, fill = vote)) +
  geom_hline(yintercept = seq(6, 24, 6), color = "grey40", size = 0.1) +
  geom_col(alpha = 0.95) +
  # custom y axis labels
  annotate("label", x = 2, #ceiling(length(unique(plot_df$from_country)) / 4), 
           y = c(12, 18, 24), label = c(12, 18, 24),
           family = "Noto Sans", size = 2.5, color = "grey30", fill = "grey90",
           label.size = 0.1) +
  annotate("richtext", x = 5, y = 13,
           label = glue("<b style='color:{ua_colors[1]}'>No jury votes</b>
           from <br>these countries"), hjust = 0, size = 3,
           family = font_family, label.size = 0, label.r = unit(0, "mm"),
           fill = alpha("white", 0.5)) +
  annotate("richtext", x = length(unique(plot_df$from_country)), y = 19,
           label = "**12 points** from both<br>jury and televoting", hjust = 0.92, size = 3,
           family = font_family, label.size = 0, label.r = unit(0, "mm"),
           fill = alpha("white", 0.8)) +
  scale_fill_manual(values = ua_colors) +
  guides(fill = "none") +
  coord_polar() +
  labs(
    title = toupper("The votes to Kalush Orchestra's ESC win"),
    subtitle = glue(
    "Coming in fourth after the <b style='color:{ua_colors[1]}'>jury votes</b>, 
    Ukraine won the <b style='color:{colorspace::lighten(ua_colors[2], 0.05)}'>televoting</b>
    (public) vote by a landslide in the Eurovision Song Contest 2022. 
    Ukraine received **zero points** from {n_zero_jury_points} countries' juries 
    whereas {n_12_televoting_points} countries gave **12 points** 
    (the maximum number of points) 
    in the televoting, receiving **not less than {min_televoting_points} points** from any televoting."),
    caption = "**Source:** Eurovision, prepared by Tanya Shapiro & Bob Rudis. 
    **Visualization:** Ansgar Wolsing") +
  theme_minimal(base_family = font_family) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    axis.title = element_blank(),
    axis.text = element_text(family = "Font Awesome 6 Free Regular", size = 20),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_markdown(
      family = "Roboto Condensed", face = "bold", 
      hjust = 0.5, size = 22,
      margin = margin(t = 4, b = 8)),
    plot.subtitle = element_textbox(
      width = 1, hjust = 0, halign = 0, lineheight = 1.33, size = 11,
      margin = margin(t = 2, b = 18)),
    plot.caption = element_markdown(hjust = 0.5))
invisible(dev.off())
