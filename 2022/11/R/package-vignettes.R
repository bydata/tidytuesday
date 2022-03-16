library(tidyverse)
library(ggtext)
library(here)
library(lubridate)

base_path <- here::here("2022", "11")


## DATA PREP -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2022-03-15")

glimpse(tuesdata$cran)
glimpse(tuesdata$bioc)

## Clean dates

cran <- tuesdata$cran %>% 
  # try different formats
  mutate(
    date2 = na_if(date, "0"),
    date2 = as_date(date),
    date2 = if_else(is.na(date2), ymd(str_sub(as.character(date), 1, 10)), date2),
    date2 = if_else(is.na(date2), as.Date(date, format = "%c"), date2),
    date = date2
    ) %>% 
  select(-date2) %>% 
  # set any date before CRAN was established to NA (1997) %>% 
  mutate(date = if_else(date <= as_date("1997-01-01"), as_date(NA), date))

bioc <- tuesdata$bioc


# Aggregate data and generate vignette type

aggregate_vignette_type <- function(df) {
  df %>% 
    filter(!is.na(date)) %>% 
    mutate(year = year(date)) %>% 
    group_by(year, package) %>% 
    summarize(rnw = max(rnw),
              rmd = max(rmd), 
              .groups = "drop") %>% 
    mutate(vignette_type = case_when(
      rnw > 0 & rmd == 0 ~ "Sweave",
      rnw == 0 & rmd > 0 ~ "RMarkdown",
      rnw > 0 & rmd > 0 ~ "Both",
      rnw == 0 & rmd == 0 ~ "None"
    ),
    vignette_type = factor(vignette_type, 
                           levels = c("Sweave", "Both", "None", "RMarkdown"))
    )
}

df_combined <- map_df(list("CRAN" = cran, "Bioconductor" = bioc), 
       aggregate_vignette_type, 
       .id = "repository")



## PLOT ------------------------------------------------------------------------
#' Building up on the plot created by @jmcastagnetto
#' Twitter: https://twitter.com/jmcastagnetto/status/1503775255810224131/photo/1
#' Github: https://github.com/jmcastagnetto/tidytuesday-kludges/tree/main/2022-03-15_r-vignettes

# Colors
pal <- c("#FFACAC", "grey65", "grey83", "#1A4155")

subtitle = glue::glue("
<b style='color:{pal[4]}'>RMarkdown</b> (Rmd) has gained continuously increasing popularity 
since vignettes in 
formats other than <b style='color:{pal[1]}'>Sweave</b> have been supported 
in R version 3.0.0 in 2013. Since 2016, the majority of newly added or updated packages
on **CRAN** had Rmd vignettes every year. On **Bioconductor**, Rmd overtook Sweave
vignettes in 2019.
")

df_combined %>%
  filter(year >= 2013, year <= 2020) %>% 
  count(repository, year, vignette_type) %>% 
  arrange(repository, year, desc(vignette_type)) %>% 
  # values for highlighted in plot
  mutate(
    highlight = (
      # repository == "Bioconductor" & year == 2013 & vignette_type == "RMarkdown" #|
      # year == 2013 & vignette_type == "Sweave" 
      vignette_type %in% c("RMarkdown", "Sweave") & (
        year %in% c(2013, 2020) | 
          repository == "CRAN" & year == 2016 | 
          repository == "Bioconductor" & year == 2019 
      )
    ),
    n2 = ifelse(highlight, n, NA)) %>%
  ggplot(aes(year, n, fill = vignette_type)) +
  geom_col(aes(), col = "white", size = 0.2,  position = "fill") +
  geom_label(# data = . %>% filter(highlight),
    aes(label = n2), position = "fill", size = 3,
    family = "Fira Sans Medium", color = "grey4", fill = "white",
    label.size = 0) +
  
  # annotation for Rmd becoming most popular
  shadowtext::geom_shadowtext(
    data = data.frame(repository = c("Bioconductor", "CRAN"), 
                      year = c(2019, 2016), n = c(0.75, 0.5)),
    aes(year, n, label = "First year\nwith more\nRmd vignettes"), 
    inherit.aes = FALSE, size = 3.5, family = "Fira Sans", hjust = 0, 
    color = "grey4", nudge_x = -0.4, lineheight = 0.9, bg.colour = "white"
  ) +
  geom_curve(
    data = data.frame(
      repository = c("Bioconductor", "CRAN"), year = c(2019, 2016), 
      n = c(0.8, 0.6)),
    aes(x = year, xend = year - 0.1, y = n - 0.2, yend = c(0.475, 0.3)),
    inherit.aes = FALSE, size = 0.2, color = "grey12", curvature = 0.1) +
  
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(vars(repository), nrow = 2) +
  labs(
    title = "RMarkdown increasingly popular for package vignettes",
    subtitle = subtitle,
    caption = "**Source:** Data retrieved by Robert Flight | **Visualization:** Ansgar Wolsing",
    x = NULL,
    y = "Share of added or updated packages",
    fill = "Vignette format"
  ) +
  theme_minimal(base_family = "Fira Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12, color = "grey40"),
    text = element_text(color = "grey32"),
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", color = "grey4"),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 4, b = 6)
    ),
    plot.caption = element_markdown(hjust = 0),
    legend.position = "right",
    legend.justification = "top",
    plot.margin = margin(t = 6, b = 4, l = 6, r = 6)
  )
ggsave(here(base_path, "plots", "vignette-formats.png"), width = 7, height = 7)


