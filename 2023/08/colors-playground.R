# read images
imgs <- map(image_filenames, ~image_read(file.path(image_dir, .x)))

# extracted_colors_by_painting <- map(imgs, get_colorPal)
# foo <- extracted_colors_by_painting %>% 
#   bind_rows(.id = "id") %>% 
#   # group_by(id) %>% 
#   # slice_max(order_by = n, n = 1) %>% 
#   # ungroup() %>% 
#   group_by(hex, hue, sat, value) %>% 
#   summarize(n = sum(n), .groups = "drop") %>% 
#   arrange(desc(n))
# 
# clustered_colors <- kmeans(select(foo, hue, sat, value), 16)
# 
# table(clustered_colors$cluster)
# count(data.frame(cluster = clustered_colors$cluster), cluster, sort = TRUE)
# clustered_colors$centers
# 
# remotes::install_github("briandconnelly/colormod")
# colormod::hsv2rgb(as.matrix(c(h = 207 / 360, s = 0.19, v = 0.80), nrow = 3))
# rgb(165, 187, 204, maxColorValue = 255)
# 
# colormod::hsv2rgb(as.matrix(c(h = 21 / 360, s = 0.37, v = 0.70), nrow = 3))
# rgb(179, 136, 112, maxColorValue = 255)
# 
# colormod::hsv2rgb(as.matrix(c(h = 271 / 360, s = 0.14, v = 0.74), nrow = 3))
# rgb(176, 162, 189, maxColorValue = 255)



bob_ross_colors <- bob_ross %>% 
  transmute(
    id = painting_index, 
    across(c(colors, color_hex), ~str_remove_all(.x, "([\\[\\]']|\\\\r\\\\n)"))) %>% 
  separate_rows(colors, color_hex, sep = ", ") %>% 
  count(colors, color_hex, sort = TRUE) %>% 
  group_by(color_hex) %>% 
  # multiple names used for #000000 and #FFFFFF, let's keep the names in a 
  # combined column
  summarize(
    colors = paste(colors, collapse = ","),
    n = sum(n)
  ) %>% 
  arrange(desc(n))
bob_ross_colors