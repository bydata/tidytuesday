
#' Costs are adjusted for purchasing power parity (PPP), based on the midpoint of construction. 
#' This adjustment is imperfect in countries with rapid changes in the PPP rate due to high inflation, typically in the developing world. 
#' Of note, we have not adjusted for inflation so far; 
#' since all costs are converted to US dollars, you can inflation-adjust using American inflation rates.
#' (https://transitcosts.com/data/)
#' 
#' This script scrapes and parses the US annual average consumer price index from usinflationcalculator.com
#' Don't worry about the page url - the page contains up-to-date data

library(tidyverse)
library(rvest)

url <- "https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/"
page <- read_html(url)

# read table data and transform data
us_inflation <- html_node(page, css = "h3 + div table") %>% 
  html_table(header = FALSE) %>% 
  # discard the first 2 rows
  slice(-1:-2) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(avg_new = rowMeans(select(., X2:X14), na.rm = TRUE),
         avg_new = round(avg_new, 1)) %>% 
  # Data for Dec 2020 is not available yet, calculate the yearly average from 
  select(year = 1, annual_inflation = avg_new) 

# load TidyTuesday data
tuesdata <- tidytuesdayR::tt_load(2021, week = 2)
transit_cost <- tuesdata$transit_cost

# Choose a reference year - the costs from this year will remain unchanged after the adjustments, 
# The costs from other years will be adjusted to the reference year's consumer prices
reference_year <- 2020
reference_inflation <- us_inflation$annual_inflation[us_inflation$year == reference_year]
if (is.na(reference_inflation)) {
  reference_inflation <- us_inflation$annual_inflation[us_inflation$year == max(us_inflation$year, na.rm = TRUE)]
}

# adjust real costs and cost per km in millions to US inflation
transit_cost <- transit_cost %>% 
  # needs some more careful cleaning
  # real cost is of type character due to some calculations added at the bottom of the spreadsheet
  mutate(real_cost = as.numeric(real_cost)) %>% 
  # costs are adjusted by PPP rate based on the midpoint year
  left_join(us_inflation, by = "year") %>% 
  # calculate inflation rates based on reference year
  mutate(annual_inflation_to_reference = reference_inflation / annual_inflation) %>% 
  # adjust costs by inflation rate
  mutate(across(c(real_cost, cost_km_millions), 
                .fns = ~.x * annual_inflation_to_reference, 
                .names = "{col}_adj")) %>% 
  select(-annual_inflation, -annual_inflation_to_reference)
 
