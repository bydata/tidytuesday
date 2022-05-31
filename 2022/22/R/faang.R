library(tidyverse)
library(ggtext)
library(here)
# devtools::install_github("davidsjoberg/ggbump")
library(ggbump)

base_path <- here::here("2022", "22")


## DATA PREP -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2022, week = 22)
poll <- tuesdata$poll

poll_long <- poll %>% 
  mutate(year = 2022) %>% 
  distinct(company, industry, year, rank = `2022_rank`, rq = `2022_rq`) %>% 
  bind_rows(select(poll, company, industry, year, rank, rq)) %>% 
  arrange(company, year) %>% 
  group_by(company) %>% 
  mutate(change = rank - lag(rank, default = NA)) %>% 
  ungroup()
  

faang <- c("Google", "Facebook", "Apple", "Amazon.com", "Netflix")
base_family <- "Barlow"

poll_long %>% 
  filter(company %in% faang) %>% 
  mutate(company = ifelse(company == "Amazon.com", "Amazon", company)) %>% 
  filter(!is.na(rank)) %>% 
  ggplot(aes(year, rank, col = company)) +
  geom_bump(size = 1.5) +
  geom_point(shape = 21, size = 5, stroke = 1.25, fill = "white") +
  geom_text(aes(label = rank), size = 2, family = "Barlow SemiBold") +
  geom_text(
    data = ~subset(., year == max(year) | year == min(year)),
    aes(x = ifelse(year == max(year), year + 0.15, year - 0.15),
        # move Netflix a bit
        y = ifelse(company == "Netflix" & year == max(year), rank + 2, rank),
      label = company, hjust = ifelse(year == max(year), 0, 1)),
    size = 3.5, family = "Barlow SemiBold"
  ) +
  annotate(
    GeomTextBox,
    x = 2019, y = 80, 
    label = "Facebook's reputation dropped in 2019 amid privacy concerns and
           allegations of election interference.",
    hjust = 0, color = "grey14", fill = NA, box.size = 0,
    family = base_family, size = 2.5
    ) +
  annotate(
    "segment",
    x = 2019.1, xend = 2019, y = 85.5, yend = 92, color = "grey14", size = 0.2) +
  guides(color = "none") +
  labs(
    title = "Reputation of FAANG companies",
    subtitle = "How did tech companies Facebook (Meta), Amazon, Apple, Netflix
    and Google (Alphabet), short \"FAANG\", fared from 2017 to 2022 in the annual Axios Harris Poll 100, 
    which gauges corporate awareness and reputation among the U.S. public?",
    caption = "**Source:** Axios Harris Poll 100. **Visualization:** Ansgar Wolsing"
  ) +
  scale_x_continuous(position = "top") +
  scale_y_reverse() +
  scale_color_manual(values = MetBrewer::met.brewer("Thomas")) +
  coord_cartesian(clip = "off") +
  theme_void(base_family = base_family) +
  theme(
    plot.background = element_rect(color = NA, fill = "#FAF2E3"), #"#F5EDDE"
    plot.margin = margin(t = 4, r = 40, b = 4, l = 40),
    plot.title = element_text(
      face = "bold", size = 18, hjust = 0.5, margin = margin(t = 4, b = 2)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      size = 9, width = 0.75, color = "grey12", margin = margin(t = 4, b = 16)),
    plot.caption = element_markdown(),
    axis.text.x.top = element_text(color = "grey34", margin = margin(b = 2)),
    axis.line.x.top = element_line(size = 0.1, color = alpha("grey50", 0.5))
  )
ggsave(here(base_path, "plots", "bump_faang_rank.png"), width = 7, height = 5.5)
