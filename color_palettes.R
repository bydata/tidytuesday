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



## TATE GALLERY (Two Triangles)
# https://www.tate.org.uk/art/artworks/mukherjee-two-triangles-t14326

tate_pal <- function(n, reverse = FALSE) {
  stopifnot(n > 0)
  pal <- c(
    "gold" = "#C8961B",
    "green" = "#458847",
    "darkred" = "#701D1A",
    "darkpurple" = "#1F1839",
    "petrol" = "#34768E",
    "yellowbrown" = "#D9C497",
    "red" = "#9A2708",
    "coralle" = "#E2D1D1"
  )
  if (reverse) {
    pal <- rev(pal)
  }
  
  unname(pal[seq(n)])
}

scale_color_tate <- function(...) {
  pal <- tate_pal
  discrete_scale("color", "tate", palette = pal, ...)
}
scale_fill_tate <- function(...) {
  pal <- tate_pal
  discrete_scale("fill", "tate", palette = pal, ...)
}
