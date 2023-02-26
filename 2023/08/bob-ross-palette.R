library(tidyverse)
library(ggforce)
library(magick)
library(colorspace)
library(ggtext)

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

# Stack all paintings into a single image
images_combined <- image_append(reduce(imgs, c), stack = TRUE)
# image_write(images_combined, here::here("2023", "08", "img-comb.png"))
# Extract colors from the combined images
colors_combined <- get_colorPal(images_combined, n = 24)

# Print colors in a simple plot
colors_combined %>% 
  mutate(hex = fct_reorder(hex, n)) %>% 
  ggplot(aes(hex, n, fill = hex)) +
  geom_col(show.legend = FALSE) +
  scale_fill_identity() +
  coord_flip() +
  theme_light()


# Points to define the palette spline
spline_controls <- data.frame(
  x = -c(0.58, 0.1, 0.27, 0.34, 0.32, 0.9),
  y = c(0.04, 0.30, 0.34, 0.28, 0.85, 0.24)
)

color_coordinates <- data.frame(
  x = -c(0.66, 0.69, 0.69, 0.65, 0.61, 0.56, 0.51,
         0.55, 0.60, 0.60, 0.56, 0.52, 0.47, 0.42),
  y =  c(0.20, 0.27, 0.34, 0.40, 0.46, 0.51, 0.56,
         0.23, 0.28, 0.34, 0.40, 0.46, 0.51, 0.56)
)

color_df_plot <- data.frame(
  head(select(colors_combined, hex, n), nrow(color_coordinates)),
  color_coordinates
) %>% 
  mutate(color_share = n / sum(n))

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
    alpha = 0.7)
  
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
    The actual colours used in the paintings were reduced to 24 colours using 
    an Adaptive Spatial Subdivision algorithm. 
    The colour bubbles are sized proportional to the area covered by this 
    particular colour.",
    family = "Georgia", size = 5, lineheight = 1.2,
    box.size = 0, fill = NA
  ) +
  scale_x_continuous(limits = c(-0.9, 0.15)) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_area(max_size = 28, labels = scales::percent_format()) + 
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
colors_combined %>% 
  mutate(hex = fct_reorder(hex, n)) %>% 
  ggplot(aes(fill = hex, area = n, subgroup = hex)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white", size = 1.5) +
  # geom_treemap_subgroup_text(color = "grey80") +
  scale_fill_identity() +
  coord_flip() +
  theme_light()
