library(tidyverse)
library(ggtext)
library(here)
library(lubridate)
library(gganimate)

tuesdata <- tidytuesdayR::tt_load("2023-02-07")
companies <- tuesdata$big_tech_companies
stock_prices <- tuesdata$big_tech_stock_prices

# which companies?
unique(companies)

head(stock_prices)

stock_prices %>% 
  ggplot(aes(date, adj_close, color = stock_symbol)) +
  geom_line()

stock_prices %>% 
  group_by(stock_symbol) %>% 
  summarize(min(date))

start_date <- as_date("2013-01-01")
stock_prices %>% 
  inner_join(companies, by = "stock_symbol") %>% 
  filter(date >= start_date) %>% 
  group_by(stock_symbol) %>% 
  mutate(adj_close_rel = 100 * adj_close / .$adj_close[.$stock_symbol == stock_symbol & .$date == min(.$date)]) %>% 
  ungroup() %>% 
  ggplot(aes(date, adj_close_rel, color = stock_symbol)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(vars(company))

max_date <- as_date("2022-12-29")
df_plot <- stock_prices %>% 
  inner_join(companies, by = "stock_symbol") %>% 
  filter(date >= start_date) %>% 
  group_by(stock_symbol) %>% 
  mutate(adj_close_rel = 100 * adj_close / .$adj_close[.$stock_symbol == stock_symbol & .$date == min(.$date)]) %>% 
  # filter(date == max_date) %>% 
  ungroup()  %>% 
  # quick & dirty formatting of company names
  mutate(company2 = str_extract(company, "[^\\s,\\.]+"),
         company2 = ifelse(stock_symbol == "IBM", "IBM", company2),
         company2 = fct_reorder(company2, adj_close_rel),
         company2_lower = tolower(company2),
         company_label = sprintf("<img src='%s/%s.png' width='16', height='16'>", 
                                 here("2023", "06", "icons"), company2_lower)) %>% 
  select(stock_symbol, company = company2, company_label, date, adj_close_rel)


df_plot %>% 
  filter(date == max_date) %>% 
  ggplot(aes(company, adj_close_rel)) +
  geom_col() +
  geom_richtext(aes(label = company_label)) +
  coord_flip()

bar_offset <- 0.35
df_plot_anim <- df_plot %>% 
  # filter(date >= as_date("2018-01-01")) %>% 
  filter(date <= max_date) %>% 
  mutate(month = floor_date(date, "1 month")) %>% 
  arrange(date) %>% 
  # calculate summary statistic per month
  group_by(company, company_label, month) %>% 
  summarize(adj_close_rel_first = first(adj_close_rel),
            adj_close_rel_last = last(adj_close_rel),
            adj_close_rel_avg = mean(adj_close_rel), .groups = "drop") %>% 
  group_by(month) %>% 
  mutate(rank = rank(-adj_close_rel_first), ties.method = "first") %>% 
  ungroup() 

p <- df_plot_anim %>% 
  # filter(month == max(month)) %>% 
  ggplot(aes(adj_close_rel_last, rank)) +
  # use geom_rect instead of geom_col for smooth transitions of the bars
  geom_rect(
    aes(ymin = rank - bar_offset, ymax = rank + bar_offset,
        xmin = 0, xmax = adj_close_rel_last,
        col = company, fill = stage(company, after_scale = alpha(fill, 0.5)))
  ) +
  # company icons
  geom_richtext(
    aes(x = -6, label = company_label, hjust = 1),
    label.size = 0, fill = NA
  ) +
  # stock value
  geom_text(
    aes(label = paste(round(adj_close_rel_last), "$")),
    col = "grey32", size = 3, family = "Roboto Condensed", hjust = -0.15) +
  scale_x_continuous() +
  scale_y_reverse() +
  scale_fill_manual(
    values = paletteer::paletteer_dynamic("cartography::green.pal", 14), 
    aesthetics = list("fill", "color")) +
  coord_cartesian(clip = "off") +
  guides(fill = "none", color = "none") +
  labs(
    title = "Hindsight Bias",
    subtitle = "If had you invested 100 US-$ on one of these Tech stocks 10 years
    <br>ago, how much would would it be worth today?
    <br>
    <br><span style='font-size: 18pt; color: #888888; font-family: Chivo'>
    {format(frame_time, '%Y (%B)')}</span>",
    caption = "The closing price of the first day of a month after adjustments 
    for all applicable splits and dividend distributions is shown.<br>
    **Source:** Yahoo Finance. **Icons:** icons8.de, flaticon.com. 
    **Visualisation:** Ansgar Wolsing"
  )  +
  theme_void(base_family = "Roboto Condensed", base_size = 12) +
  theme(
    plot.background = element_rect(color = "grey90", fill = "grey90"),
    plot.margin = margin(t = 2, b = 2, l = 18, r = 18),
    text = element_text(color = "grey20"),
    plot.title = element_text(color = "grey2", family = "Georgia", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(lineheight = 1.1),
    plot.caption = element_textbox(size = 7, width = 1, lineheight = 1.1,
                                   margin = margin(t = 4, b = 8))
  )

p_anim <- p + 
  transition_time(month) +
  view_follow(fixed_x = FALSE, fixed_y = TRUE)

animate(p_anim, ref_frame = 1, nframes = length(unique(df_plot_anim$month)) * 5,
        res = 150, width = 720, height = 800, fps = 12)
anim_save(here("2023", "06", "hindsight-bias.gif"))
