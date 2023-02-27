library(tidyverse)
library(ggforce)
library(magick)
library(colorspace)
library(ggtext)
# remotes::install_github("briandconnelly/colormod")

# Read in the data
bob_ross <- read_csv(
  "https://raw.githubusercontent.com/jwilber/Bob_Ross_Paintings/master/data/bob_ross_paintings.csv",
) 

# The first column doesn't contain data that we need, so we can remove it
bob_ross <- select(bob_ross, -1)

## Download images
image_urls <- bob_ross$img_src
image_filenames <- str_extract(image_urls, "\\d+\\.png")
image_dir <- here::here("2023", "08", "images")
if (!dir.exists(image_dir)) {
  dir.create(image_dir)
  walk2(image_urls, image_filenames, 
        ~download.file(.x, destfile = file.path(image_dir, .y)))
}

# https://www.r-bloggers.com/2019/01/extracting-colours-from-your-images-with-image-quantization/
# found at https://twitter.com/amc_corporation/status/1628571717454143488
get_colorPal <- function(im, n = 8, cs = "RGB") {
  tmp <- im %>% 
    image_resize("100") %>%
    image_equalize() %>% 
    # Quantize documentation: https://imagemagick.org/Usage/quantize/#colors
    image_quantize(max = n, colorspace = cs) %>% 
    imager::magick2cimg() %>% 
    imager::RGBtoHSV() %>% 
    as.data.frame(wide = "c") %>% 
    mutate(
      hex = hsv(scales::rescale(c.1, from = c(0, 360)), c.2, c.3),
      hue = c.1,
      sat = c.2,
      value = c.3
    ) %>%
    count(hex, hue, sat, value, sort = T) %>%
    mutate(colorspace = cs)
  
  tmp %>% select(colorspace, hex, hue, sat, value, n)
}


# read images
imgs <- map(image_filenames, ~image_read(file.path(image_dir, .x)))

# extract colours from paintings
extracted_colors_by_painting <- map(imgs, get_colorPal, n = 24)
extracted_colors_all <- extracted_colors_by_painting %>%
  bind_rows(.id = "id") 

extracted_colors_all %>%
  group_by(hex, hue, sat, value) %>%
  summarize(n = sum(n), .groups = "drop") %>%
  arrange(desc(n))

# k-means clustering of colors
k <- 14
set.seed(123)
clustered_colors <- kmeans(select(foo, hue, sat, value), k)
table(clustered_colors$cluster)
clustered_colors$centers

clustered_colors_df <- tibble(
  cluster = seq_len(k),
  as.data.frame(clustered_colors$centers),
  n = as.integer(table(clustered_colors$cluster))
)
head(clustered_colors_df)

# convert hsv to rgb and return hex colour codes
hsv_rgb <- function(hue, sat, value) {
  m <- as.matrix(c(h = hue / 360, s = sat, v = value)) %>% 
    colormod::hsv2rgb() %>% 
    t()
  rgb(m[, "red"], m[, "green"], m[, "blue"], maxColorValue = 255)
}

# generate color hex codes
clustered_colors_df$hex <- pmap_chr(select(clustered_colors_df, hue, sat, value), hsv_rgb)
head(clustered_colors_df)


# Print colors in a simple plot
clustered_colors_df %>% 
  mutate(hex = fct_reorder(hex, n)) %>% 
  ggplot(aes(hex, n, fill = hex)) +
  geom_col(show.legend = FALSE) +
  scale_fill_identity() +
  coord_flip() +
  theme_light()
ggsave(here::here("2023", "08", "bob-ross-palette-barchart.png"), width = 8.5, height = 7.5)


# Points to define the palette spline
spline_controls <- data.frame(
  x = -c(0.6, 0.1, 0.27, 0.34, 0.32, 0.9),
  y = c(0.05, 0.30, 0.34, 0.28, 0.85, 0.24)
)

color_coordinates <- data.frame(
  x = -c(0.66, 0.69, 0.69, 0.65, 0.61, 0.56, 0.51,
         0.55, 0.60, 0.60, 0.56, 0.52, 0.47, 0.42),
  y =  c(0.20, 0.27, 0.34, 0.40, 0.46, 0.51, 0.56,
         0.23, 0.28, 0.34, 0.40, 0.46, 0.51, 0.56)
)
color_coordinates$x + 0.1


color_df_plot <- data.frame(
  head(select(clustered_colors_df, hex, n), nrow(color_coordinates)),
  color_coordinates
) %>% 
  mutate(color_share = n / sum(n),
         hex = fct_reorder(hex, n)) %>% 
  arrange(hex)
  

ggplot() +
  # Shapes for the palette
  geom_bspline_closed( 
    data = spline_controls, 
    aes(x + 0.003, y + 0.007),
    fill = "grey78", col = darken("grey87", 0.2), size = 0.25, 
    alpha = 0.7) +
  geom_bspline_closed( #F7DFC5
    data = spline_controls, 
    aes(x, y),
    fill = "grey92", col = darken("grey87", 0.2), size = 0.25, 
    alpha = 0.7) +
  geom_point(
    data = color_df_plot,
    aes(x, y, fill = hex, size = color_share, 
        col = stage(after_scale = darken(fill, 0.1))),
    stroke = 1.25, shape = 21
  ) +
  
  # Custom title + subtitle
  annotate(
    GeomTextBox,
    x = -0.25, y = 0.67,
    width = 0.4,
    hjust = 0, vjust = 1,
    label = "<b style='font-size:20pt'>Bob Ross's Palette</b><br><br>
    The most frequent colours in the 403 paintings created by Bob Ross
    in his T.V. show *The Joy of Painting*. 
    The colours used in each paintings were reduced to 24 colours using 
    an Adaptive Spatial Subdivision algorithm. The resulting colours were clustered
    into 14 colour groups using k-means. The 'center' colour is displayed.
    The bubbles are sized proportional to the area covered by this 
    particular colour cluster.",
    family = "Georgia", size = 4.5, lineheight = 1.2,
    box.size = 0, fill = NA
  ) +
  scale_x_continuous(limits = c(-0.9, 0.15)) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_area(max_size = 20, labels = scales::percent_format()) + 
  coord_cartesian(clip = "off") +
  guides(size = guide_legend(
    title = "Area covered",
    title.position = "top",
    override.aes = list(color = "grey80", fill = "grey92", stroke = 0.75))) +
  labs(
    caption = "Source: Jared Wilber's data on Bob Ross Paintings, own calculations.
    Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Georgia") +
  theme(
    plot.background = element_rect(
      color = "white", fill = "white"), # desaturate("#C9F3FE", 0.6)),
    text = element_text(color = "grey18"),
    # plot.title = element_text(family = "Georgia", face = "bold", color = "grey2"),
    # plot.subtitle = element_textbox(width = 0.9, lineheight = 1.1),
    plot.caption = element_textbox(
      width = 0.9, lineheight = 1, hjust = 1, halign = 1, size = 10),
    plot.margin = margin(t = 2, b = 4, l = 0, r = 16),
    # legend.position = "bottom",
    legend.justification = "right",
    legend.title = element_text(
      family = "Georgia", face = "bold", color = "grey40", size = 12),
    legend.text = element_text(size = 12, family = "Open Sans"),
    legend.position = c(0.95, 0.3),
    legend.direction = "vertical",
    legend.key.height = unit(2, "mm")
  )
ggsave(here::here("2023", "08", "bob-ross-palette.png"), width = 8.5, height = 7.5)


library(treemapify)

# Print colors in a simple plot
clustered_colors_df %>% 
  mutate(color_share = n / sum(n),
    hex = fct_reorder(hex, n)) %>% 
  ggplot(
    aes(fill = hex, area = n, 
        subgroup = sprintf("%s (%0.1f %%)", hex, 100 * color_share))) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white", size = 1.5) +
  geom_treemap_subgroup_text(color = "grey18", family = "Roboto Condensed") +
  scale_fill_identity() +
  coord_flip() +
  theme_light()
ggsave(here::here("2023", "08", "bob-ross-palette-treemap.png"), width = 8.5, height = 7.5)


