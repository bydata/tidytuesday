pacman::p_load("tidyverse", "ggtext", "glue", "here", "gganimate")


tuesdata <- tidytuesdayR::tt_load("2022-01-11")
stressor <- tuesdata$stressor

unique(stressor$state)
unique(stressor$months)

stressor_prep <- stressor %>% 
  mutate(months = factor(months, levels = c("January-March", "April-June", 
                                            "July-September", "October-December")),
         quarter = paste0("Q", as.numeric(months)),
         year_quarter = paste(year, quarter))

stressor_prep %>% 
  group_by(stressor) %>% 
  na.omit() %>% 
  summarize(mean_stress_pct = mean(stress_pct)) %>% 
  arrange(-mean_stress_pct)

selected_stressors <- c("Varroa mites", "Pesticides")

stressor_prep %>% 
  filter(state == "United States") %>% 
  filter(stressor %in% selected_stressors) %>% 
  pivot_wider(id_cols = -c(stressor, stress_pct), names_from = "stressor",
              values_from = "stress_pct") %>% 
  ggplot(aes(`Varroa mites`, Pesticides)) +
  geom_point() +
  geom_segment(aes(x = lag(`Varroa mites`), xend = `Varroa mites`,
                   y = lag(Pesticides), yend = Pesticides))
