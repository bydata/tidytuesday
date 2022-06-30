library(tidyverse)
library(ggtext)
library(here)

base_path <- here::here("2022", "26")

tuesdata <- tidytuesdayR::tt_load(2022, week = 26)
paygap <- tuesdata$paygap
# calculate year
paygap$year <- lubridate::year(as.Date(paygap$due_date))

# employer size
count(paygap, employer_size)
paygap %>% 
  filter(year <= 2022) %>% 
  filter(employer_size == "20,000 or more") %>% 
  distinct(employer_name)

# recode employer size to factor
unique(paygap$employer_size)
paygap$employer_size <- factor(
  paygap$employer_size, 
  levels = c("Less than 250", "250 to 499", "500 to 999", "1000 to 4999",
             "5000 to 19,999", "20,000 or more", "Not Provided"))

# Filter to companies with more than 20k employees and year 2022
paygap_20k_22 <- paygap %>% 
  filter(year == 2022) %>% 
  filter(employer_size == "20,000 or more") 
paygap_20k_22

# Companies with equal pay
paygap_20k_22_equal <- paygap_20k_22 %>% 
  filter(diff_median_hourly_percent == 0) %>% 
  mutate(employer_name = str_to_title(employer_name)) %>% 
  pull(employer_name)

# Top 3 companies by highest paygap
paygap_20k_22_top3 <- paygap_20k_22 %>% 
  slice_max(diff_median_hourly_percent, n = 3) %>% 
  transmute(employer_name = str_to_title(employer_name), diff_median_hourly_percent)
paygap_20k_22_top3

paygap_20k_22_top2_male <- data.frame(
  employer_name = c("Lloyds Bank", "National Westminster Bank"),
  diff_median_hourly_percent = c(40.9, 34.2)
)
  

paygap_20k_22 %>% 
  slice_min(diff_median_hourly_percent, n = 2) %>% 
  select(employer_name, diff_median_hourly_percent)

# Companies with a paygap favouring women
paygap_20k_22_top2_female <- data.frame(
  employer_name = c("NHS Professionals", "Openreach"),
  diff_median_hourly_percent = c(-14.4, -14.2)
)


ragg::agg_png(here(base_path, "plots", "beeswarm_median_2022.png"), res = 500,
              width = 7, height = 5, units = "in")
paygap_20k_22 %>%
  ggplot(aes(x = factor(1), diff_median_hourly_percent)) +
  geom_hline(yintercept = 0, col = "grey34", lty = "dashed") + 
  ggbeeswarm::geom_beeswarm(
    aes(fill = case_when(
      diff_median_hourly_percent > 0 ~ "men",
      diff_median_hourly_percent < 0 ~ "women",
      diff_median_hourly_percent == 0 ~ "equal"
      )),
    cex = 3, size = 5, shape = 21, color = "white") +
  annotate("richtext",
           x = c(1.4, 1.4), y = c(-5, 18), hjust = c(1, 0),
           label = c("These companies<br>pay more to <b style='color:#7B16EF'>women</b>",
                     "These companies<br>pay more to <b style='color:#58C1AB'>men</b>"),
           family = "Helvetica Neue",
           vjust = 0, color = "grey20",
           label.size = 0, fill = "white"
           ) +
  ggrepel::geom_text_repel(
    data = paygap_20k_22_top2_male,
    aes(x = factor(1), y = diff_median_hourly_percent,
        label = employer_name),
    direction = "x", nudge_x = -0.12,
    min.segment.length = unit(0, "cm"), segment.size = 0.1,
    family = "Helvetica Neue", size = 2.5, color = "grey20", point.padding = 0,
  ) +
  ggrepel::geom_text_repel(
    data = paygap_20k_22_top2_female,
    aes(x = factor(1), y = diff_median_hourly_percent,
        label = employer_name),
    direction = "x", nudge_x = -0.12,
    min.segment.length = unit(0, "cm"), segment.size = 0.1,
    family = "Helvetica Neue", size = 2.5, color = "grey20", point.padding = 0,
  ) +
  # equal pay
 geom_textbox(
   data = NULL,
   aes(x = 0.4, y = 0,
       label = paste("**These companies report <b style='color:#e0b23d'>equal pay</b>:**<br>\U2022 ",
                  paste(paygap_20k_22_equal, collapse = "<br>\U2022 "))),
   inherit.aes = FALSE,
   family = "Helvetica Neue", size = 2.5, color = "grey20", hjust = 0.5,
   halign = 0, vjust = 0, box.size = 0
  ) + 
  geom_segment(
    data = NULL,
    aes(x = 0.74, xend = 0.9, y = -2, yend = 0),
    size = 0.1, color = "grey20", inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c("men" = "#58C1AB", "equal" = "#e0b23d", "women" = "#7B16EF")) +
  coord_flip(ylim = c(-20, NA)) +
  guides(fill = "none") +
  labs(
    title = "Only Few Big UK Companies Reach Equal Pay",
    subtitle = "Difference in median hourly pay in companies in the United Kingdom 
    with 20,000 or more employees (2022)",
    caption = "**Source:** gender-pay-gap.service.gov.uk. **Visualisation:** Ansgar Wolsing",
    x = NULL,
    y = "Difference between median male and female hourly pay (%)"
  ) +
  theme_minimal(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(),
    plot.caption = element_markdown(margin = margin(t = 10)),
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(8, 8, 8, 8)
  )
invisible(dev.off())
