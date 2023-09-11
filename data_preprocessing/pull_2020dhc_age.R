library(dplyr)
library(tidycensus)
library(tidyverse)

census_api_key("3910e99aea0a472b50f5cdc422c9a3395b3c87b3")

my_states = c("MA")

v20 <- load_variables(2020, "dhc", cache = TRUE)

# P12_003N to 025N and then P12_027N to 049N for the selected age categories
my_vars <- c(
  # male population by selected age categories (P12_003N to P12_025N)
  lapply(3:25, function(n) {paste0("P12_", sprintf("%03d", n), "N")}) %>% unlist(), 
  # female population by selected age categories (P12_027N to P12_049N)
  lapply(27:49, function(n) {paste0("P12_", sprintf("%03d", n), "N")}) %>% unlist()
)

# use the census API to collect the above variables by tract for the 2020 DHC
tract_pop = get_decennial(geography = "tract",
                 variables = my_vars,
                 state = "MA",
                 county = "025",
                 year = 2020,
                 sumfile="dhc",
                 output = "wide",
                 geometry = FALSE
                 )

tract_age <- tract_pop %>% mutate(
  zero_nine = rowSums(across(
    lapply(c(3:4, 27:28), function(n) {paste0("P12_", sprintf("%03d", n), "N")}) %>% unlist()
    )),
  ten_nineteen = rowSums(across(
    lapply(c(5:7, 29:31), function(n) {paste0("P12_", sprintf("%03d", n), "N")}) %>% unlist()
    )),
  twenty_thirtyfour = rowSums(across(
    lapply(c(8:12, 32:36), function(n) {paste0("P12_", sprintf("%03d", n), "N")}) %>% unlist()
    )),
  thirtyfive_fiftyfour = rowSums(across(
    lapply(c(13:16, 37:40), function(n) {paste0("P12_", sprintf("%03d", n), "N")}) %>% unlist()
    )),
  fiftyfive_sixtyfour = rowSums(across(
    lapply(c(17:19, 41:43), function(n) {paste0("P12_", sprintf("%03d", n), "N")}) %>% unlist()
    )),
  sixtyfive_more = rowSums(across(
    lapply(c(20:25, 44:49), function(n) {paste0("P12_", sprintf("%03d", n), "N")}) %>% unlist()
    )),
  .keep='unused'
  )

tract_age %>% write.csv('tract_age_2020dhc.csv')
