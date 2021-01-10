beatles_lego_pal <- function(n, reverse = FALSE) {
  stopifnot(n > 0)
  pal <- c(
    "darkblue" = "#003059",
    "golden" = "#F6A801",
    "darkbrown" = "#521D0C",
    "skyblue" = "#80BEE3",
    "brown" = "#BB5B09",
    "ocher" = "#E6C88F",
    "bluegrey" = "#6B8092",
    "darkgrey" = "grey20",
    "black" = "#000000"
  )
  if (reverse) {
    pal <- rev(pal)
  }
  
  unname(pal[seq(n)])
}

beatles_lego_pal(1)


scale_color_beatles_lego <- function(...) {
  pal <- beatles_lego_pal
  discrete_scale("color", "beatles_lego", palette = pal, ...)
}
scale_fill_beatles_lego <- function(...) {
  pal <- beatles_lego_pal
  discrete_scale("fill", "beatles_lego", palette = pal, ...)
}
