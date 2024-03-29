library(psych)
library(tidyverse)
library(ggtext)
library(here)

base_path <- here::here("2022", "33")

tuesdata <- tidytuesdayR::tt_load(2022, week = 33)
str(tuesdata)
characters <- tuesdata$characters
psych_stats <- tuesdata$psych_stats

#' Source & documentation: 
#' https://openpsychometrics.org/tests/characters/development/


# which shows?
table(characters$uni_name)
# number of characters per show
count(characters, uni_name, sort = TRUE)

characters %>% 
  filter(uni_name == "Stranger Things")
characters %>% 
  filter(uni_name == "Lord of the Rings")

nrow(psych_stats)
# how many different questions?
length(unique(psych_stats$question))
unique(psych_stats$question)

# Prepare data  ================================================================

min_ratings_threshold <- 10
psych_stats_scaled <- psych_stats %>% 
  # rescale the ratings 
  mutate(avg_rating_scaled = ifelse(
    str_extract(question, "[^/]+") == personality, 50 - avg_rating, avg_rating - 50),
         avg_rating_scaled = avg_rating_scaled / 50) %>% 
  # keep only characters which have at least 10 ratings in each dimension
  group_by(char_name) %>% 
  filter(min(number_ratings) >= min_ratings_threshold) %>% 
  ungroup() %>% 
  # remove emoji scales
  filter(str_length(question) > 10)
head(psych_stats_scaled)
length(unique(psych_stats_scaled$char_name))
unique(psych_stats_scaled$question)

psych_stats_scaled_wide <- psych_stats_scaled %>% 
  select(char_name, question, avg_rating_scaled, rating_sd) %>% 
  pivot_wider(id_cols = char_name, names_from = "question", values_from = "avg_rating_scaled",
              names_repair = function(x) janitor::make_clean_names(x, replace = c("/" = "_vs_")),  values_fn = mean)
head(psych_stats_scaled_wide)


# Image Processing =============================================================

## Download character pictures for selected show
show_name <- "The Simpsons"
character_image_urls <- characters$image_link[characters$uni_name == show_name]
character_image_filenames <- str_extract(character_image_urls, "\\d+\\.jpg")
character_image_path <- here(base_path, "input", "character_images", show_name)
if (!dir.exists(character_image_path)) {
  dir.create(here(base_path, "input"))
  dir.create(character_image_path)
  download.file(character_image_urls, destfile = here(character_image_path, character_image_filenames))
}

##' Create round images
##' Source: https://stackoverflow.com/questions/64597525/r-magick-square-crop-and-circular-mask
library(magick)

# here(character_image_path, character_image_filenames[1])
image_crop_circle <- function(img_file, img_path, dest_path, bgcolor = "#F0C246") {
  print(img_path)
  im <- image_read(here(img_path, img_file))
  
  # get height, width and crop longer side to match shorter side
  ii <- image_info(im)
  ii_min <- min(ii$width, ii$height)
  im1 <- image_crop(im, geometry = paste0(ii_min, "x", ii_min, "+0+0"), repage = TRUE)
  
  # create a new image with white background and black circle
  img_circle <- image_draw(image_blank(ii_min, ii_min))
  symbols(ii_min / 2, ii_min / 2, circles = (ii_min / 2) - 3, bg = "black", 
          inches = FALSE, add = TRUE)
  dev.off()
  
  img_circle_ring <- image_draw(image_blank(ii_min, ii_min))
  symbols(ii_min / 2, ii_min / 2, circles = (ii_min / 2) - 3, fg = "green", 
          inches = FALSE, add = TRUE)
  dev.off()
  
  
  # create an image composite using both images and set background as transparent
  im2 <- image_composite(im1, img_circle, operator = "copyopacity") %>% 
    image_background(bgcolor)
  
  # save processed images
  if (!dir.exists(dest_path)) {
    dir.create(dest_path)
  }
  image_write(im2, here(dest_path, img_file))
}

walk(
  character_image_filenames, 
  ~image_crop_circle(
    .x, character_image_path, 
    here(character_image_path, "processed"))
)



## Factor analysis =============================================================

# Select numeric columns and keep only a fraction

# Find the most rated personality questions
questions_most_ratings <- psych_stats %>% 
  count(question, wt = number_ratings, sort = TRUE) %>% 
  # remove emoji scales
  filter(str_length(question) > 10) %>% 
  pivot_wider(names_from = "question", values_from = "n",
              names_repair = function(x) janitor::make_clean_names(x, replace = c("/" = "_vs_"))) %>% 
  pivot_longer(cols = everything(), names_to = "question", values_to = "n")

psych_stats_scaled_wide_numeric <- data.frame(
  psych_stats_scaled_wide[, -1], row.names =  pull(psych_stats_scaled_wide, "char_name"))
# psych_stats_scaled_wide_numeric_reduced <- 
#   psych_stats_scaled_wide_numeric[, 1:120]
psych_stats_scaled_wide_numeric_reduced <- 
  psych_stats_scaled_wide_numeric[, head(questions_most_ratings$question, 100)]

# Check number of recommended factors
fa.parallel(psych_stats_scaled_wide_numeric_reduced)

# Run FA with nfactors
nfactors <- 8
f_a <- fa(psych_stats_scaled_wide_numeric_reduced, nfactors = nfactors)
f_a
# show factor loadings about cut off
print(loadings(f_a), cutoff = 0.6)

# Data frame of factor loadings with sorted factor columns
fa_loadings_df <- as_tibble(unclass(f_a$loadings), rownames = "scale") %>% 
  select(scale, order(colnames(.)))

# Show the variables with the highest loadings per factor
loading_cutoff <- 0.6
sorted <- vector("list", nfactors)
for (i in seq_len(nfactors)) {
  writeLines(paste0("*** MR", i))
  sorted[[i]] <- fa_loadings_df[, c(1, i + 1)] %>% 
    select(scale, loading = 2) %>% 
    mutate(trait = ifelse(loading < 0, str_extract(scale, ".+_vs_"), str_extract(scale, "_vs_.+")),
           trait = str_remove(trait, "_vs_")) %>% 
    filter(abs(loading) > loading_cutoff) %>% 
    arrange(-abs(loading)) %>%
    head(10)
  print(sorted[[i]])
}
loadings_per_factor <- bind_rows(sorted, .id = "factor")

# calculate factor scores
fa_scores <- factor.scores(psych_stats_scaled_wide_numeric_reduced, f_a)

fa_scores[["scores"]] %>% 
  as_tibble(rownames = "char_name") %>% 
  arrange(-MR3)

# labels for annotations
top_loadings_per_factor_labels <- loadings_per_factor %>% 
  separate(scale, into = c("end1", "end2"), sep = "_vs_") %>% 
  mutate(
    left_label = ifelse(loading > 0, end1, end2),
    right_label = ifelse(loading > 0, end2, end1),
    loading_abs = abs(loading),
    factor = paste0("MR", factor)
  ) %>% 
  arrange(-loading_abs) %>% 
  select(factor, left_label, right_label, loading, loading_abs) %>% 
  group_by(factor) %>% 
  slice_max(loading_abs, n = 4) %>% 
  ungroup()

## custom stacking of characters
characters_fa_stacked_df <- fa_scores[["scores"]] %>% 
  as_tibble(rownames = "name") %>% 
  inner_join(characters, by = "name") %>% 
  filter(uni_name == show_name) %>% 
  # add the image paths
  bind_cols(image_src = 
              sprintf("<img src='%s' width=30>", 
                      here(base_path, "input", "character_images", show_name, 
                           "processed", character_image_filenames))) %>% 
  select(name, starts_with("MR"), image_src) %>% 
  pivot_longer(cols = -c(name, image_src), names_to = "factor", values_to = "score") %>% 
  mutate(score_rounded = round(score / 5, 1) * 5) %>% 
  group_by(factor, score_rounded) %>% 
  mutate(stack_id = row_number()) %>% 
  ungroup()

# Plot all factors =============================================================

for (i in seq_len(nfactors)) {
  selected_factor <- paste0("MR", i)
  trait_labels_left <- top_loadings_per_factor_labels$left_label[top_loadings_per_factor_labels$factor == selected_factor]
  trait_labels_left <- str_replace_all(trait_labels_left, "_", " ")
  trait_labels_right <- top_loadings_per_factor_labels$right_label[top_loadings_per_factor_labels$factor == selected_factor]
  trait_labels_right <- str_replace_all(trait_labels_right, "_", " ")
  
  # set y position and alpha values for labels
  trait_labels_n <- length(trait_labels_left)
  alpha <- seq(1, 0.6, -(1 - 0.6) / (trait_labels_n - 1))
  # y_pos <- seq(6, 4.5, -(6 - 4.5) /  (trait_labels_n - 1))
  y_pos <- seq(6, 4.5, -0.5)
  y_pos <- y_pos[seq_len(trait_labels_n)]
  
  characters_fa_stacked_df %>% 
    filter(factor == selected_factor) %>% 
    ggplot(aes(x = score_rounded, y = stack_id)) +
    geom_richtext(
      aes(label = image_src),
      size = 8,
      # label.size = 0.5, label.color = "#2f64d6", fill = "white", label.r = unit(3.5, "mm"),
      label.size = 0, fill = NA
    ) + 
    annotate(
      "text",
      label = trait_labels_left,
      x = min(characters_fa_stacked_df$score_rounded),
      y = y_pos,
      alpha = alpha,
      color = "#2f64d6",
      family = "Source Sans Pro SemiBold",
      hjust = 0) +
    # Traits right-hand side
    annotate(
      "text",
      label = trait_labels_right,
      x = max(characters_fa_stacked_df$score_rounded), # - 0.5,
      y = y_pos,
      alpha = alpha,
      color = "#9c5b01",
      family = "Source Sans Pro SemiBold",
      hjust = 1 ) +
    # arrows
    annotate(
      "segment",
      x = c(
        min(characters_fa_stacked_df$score_rounded) + 0.75,
        max(characters_fa_stacked_df$score_rounded) - 0.75),
      xend = c(
        min(characters_fa_stacked_df$score_rounded) - 0.1,
        max(characters_fa_stacked_df$score_rounded) + 0.1),
      y = 6.5, yend = 6.5,
      color = c("#2f64d6", "#9c5b01"), size = 0.8,
      arrow = arrow(type = "closed", length = unit(2, "mm"))
    ) +
    coord_cartesian(ylim = c(0, 6.5)) +
    labs(
      title = "<span style='font-family: Simpsonfont; font-size: 18pt; color: black'>
      the</span><br>
      <span style='font-family: Simpsonfont; font-size: 30pt; color: black'>Simpsons</span><br>Psychometrics",
      caption = "**Data:** Open-Source Psychometrics Project, Tanya Shapiro. **Visualization:** Ansgar Wolsing"
    ) +
    theme_void(base_family = "Source Sans Pro") +
    theme(
      plot.background = element_rect(color = NA, fill = "#F0C246"),
      text = element_text(color = "grey10"),
      plot.title = element_markdown(
        family = "Source Sans Pro SemiBold", color = "grey47", size = 16, hjust = 0.5),
      plot.caption = element_markdown(),
      plot.margin = margin(8, 8, 8, 8)
    )
  ggsave(here(base_path, "plots", sprintf("factor_%s_stacked.png", selected_factor)), dpi = 500, width = 7, height = 5)
} 


# Animate ======================================================================

img_files <- here(base_path, "plots", sprintf("factor_%s_stacked.png", paste0("MR", seq_len(nfactors))))
imgs <- image_read(img_files)

anim <- image_animate(imgs, fps = 1 / 3)
image_write_video(anim, here(base_path, "plots", "simpsons-personalities.mov"), framerate = 1 / 3)




#' Simpsons (by Dennis Ludlow) font can be downloaded from https://www.dafont.com/simpsonfont.font
