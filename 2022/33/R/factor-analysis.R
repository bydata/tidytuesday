library(psych)
library(tidyverse)
library(ggtext)
library(here)

base_path <- here::here("2022", "33")

tuesdata <- tidytuesdayR::tt_load(2022, week = 33)
str(tuesdata)
characters <- tuesdata$characters
psych_stats <- tuesdata$psych_stats

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

# Prepare data
min_ratings_threshold <- 10
psych_stats_scaled <- psych_stats %>% 
  # rescale the ratings 
  mutate(avg_rating_scaled = ifelse(str_extract(question, "[^/]+") == personality, 50 - avg_rating, avg_rating - 50),
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


## Factor analysis =============================================================

# Select numeric columns and keep only a fraction
psych_stats_scaled_wide_numeric <- data.frame(
  psych_stats_scaled_wide[, -1], row.names =  pull(psych_stats_scaled_wide, "char_name"))
psych_stats_scaled_wide_numeric_reduced <- psych_stats_scaled_wide_numeric[, 1:120]

# Check number of recommended factors
fa.parallel(psych_stats_scaled_wide_numeric_reduced)

# Run FA with nfactors
nfactors <- 7
f_a <- fa(psych_stats_scaled_wide_numeric_reduced, nfactors = nfactors)
f_a
# show factor loadings about cut off
print(loadings(f_a), cutoff = 0.6)

# Data frame of factor loadings with sorted factor columns
fa_loadings_df <- as_tibble(unclass(f_a$loadings), rownames = "scale") %>% 
  select(scale, order(colnames(.)))

# Show the variables with the highest loadings per factor
loading_cutoff <- 0.6
for (i in seq_len(nfactors)) {
  writeLines(paste0("*** MR", i))
  sorted <- fa_loadings_df[, c(1, i + 1)] %>% 
    select(scale, factor = 2) %>% 
    mutate(trait = ifelse(factor < 0, str_extract(scale, ".+_vs_"), str_extract(scale, "_vs_.+")),
           trait = str_remove(trait, "_vs_")) %>% 
    filter(abs(factor) > loading_cutoff) %>% 
    arrange(-abs(factor)) %>%
    head(10)  
  print(sorted)
}

# calculate factor scores
fa_scores <- factor.scores(psych_stats_scaled_wide_numeric_reduced, f_a)

fa_scores[["scores"]] %>% 
  as_tibble(rownames = "char_name") %>% 
  arrange(-MR3)


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

# im <- image_read(here(character_image_path, character_image_filenames[1]))
# 
# # get height, width and crop longer side to match shorter side
# ii <- image_info(im)
# ii_min <- min(ii$width, ii$height)
# im1 <- magick::image_crop(im, geometry = paste0(ii_min, "x", ii_min, "+0+0"), repage = TRUE)
# 
# # create a new image with white background and black circle
# img_circle <- image_draw(image_blank(ii_min, ii_min))
# symbols(ii_min/2, ii_min/2, circles=(ii_min/2)-3, bg = "black", inches = FALSE, add = TRUE)
# dev.off()
# 
# # create an image composite using both images and set background as transparent
# im2 <- image_composite(im1, img_circle, operator = "copyopacity") %>% 
#   # image_background("transparent")
#   image_background("#F0C246")
# 
# # save processed images
# if (!dir.exists(here(character_image_path, "processed"))) {
#   dir.create(here(character_image_path, "processed"))
# }
# image_write(im2, here(character_image_path, "processed", character_image_filenames[1]))

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



## custom stacking of characters

characters_fa_stacked_df <- fa_scores[["scores"]] %>% 
  as_tibble(rownames = "name") %>% 
  inner_join(characters, by = "name") %>% 
  filter(uni_name == "The Simpsons") %>% 
  # add the image paths
  bind_cols(image_src = 
              sprintf("<img src='%s' width=30>", 
                      here(base_path, "input", "character_images", show_name, 
                           "processed", character_image_filenames))) %>% 
  mutate(MR3_r = round(MR3 / 5, 1) * 5) %>% 
  group_by(MR3_r) %>% 
  mutate(stack_id = row_number()) %>% 
  ungroup()

characters_fa_stacked_df %>% 
  ggplot(aes(x = MR3_r, y = stack_id)) +
  # geom_point(size = 16, color = "#2f64d6") +
  geom_richtext(
    aes(label = image_src),
    size = 8,
    # label.size = 0.5, label.color = "#2f64d6", fill = "white", label.r = unit(3.5, "mm"),
    label.size = 0, fill = NA
  ) + 
  annotate(
    "text",
    label = c("persistent", "motivated", "resourceful", "driven"),
    x = min(characters_fa_stacked_df$MR3_r),
    y = seq(6, 4.5, -0.5),
    alpha = seq(1, 0.5, -0.15),
    color = "#2f64d6",
    family = "Source Sans Pro SemiBold",
    hjust = 0) +
  # 
  annotate(
    "text",
    label = c("quitter", "unmotivated", "helpless", "unambitious"),
    x = max(characters_fa_stacked_df$MR3_r) - 0.5,
    y = seq(6, 4.5, -0.5),
    alpha = seq(1, 0.5, -0.15),
    color = "#FF81C1",
    family = "Source Sans Pro SemiBold",
    hjust = 0) +
  coord_cartesian(ylim = c(0, 7)) +
  labs(
    title = "<span style='font-family: Simpsonfont; font-size: 24pt; color: black'>The Simpsons</span><br>Psychometrics"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = NA, fill = "#F0C246"),
    axis.line.x = element_line(),
    axis.text.x = element_text(),
    plot.title = element_markdown(family = "Source Sans Pro SemiBold", color = "grey50", size = 16, hjust = 0.5)
  )
ggsave(here(base_path, "plots", "factor_MR3_stacked.png"), dpi = 500, width = 7, height = 5)


p <- fa_scores[["scores"]] %>% 
  as_tibble(rownames = "name") %>% 
  inner_join(characters, by = "name") %>% 
  filter(uni_name == show_name) %>% 
  # add the image paths
  bind_cols(image_src = 
              sprintf("<img src='%s' width=10>", 
                      here(base_path, "input", "character_images", show_name, character_image_filenames))) %>% 
  ggplot(aes(x = 1, y = MR3)) +
  # geom_jitter(width = 0.1, height = 0) +
  geom_richtext(
    # data = ~subset(., MR3 == max(MR3) | MR3 == min(MR3)),
    aes(label = image_src),
    label.size = 0.5, label.color = "#2f64d6", fill = "white", label.r = unit(2.5, "mm"),
    position = position_jitter(width = 0.15, height = 0) 
  ) + 
  coord_flip(xlim = c(0.7, 1.3)) +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "#F0C246"),
    axis.line.x = element_line(),
    axis.text.x = element_text()
  )
ggsave(here(base_path, "plots", "factor_MR3_beeswarm.png"), width = 7, height = 5)






