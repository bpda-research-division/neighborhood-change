library(tidycensus)
library(dplyr)
library(ggplot2)

census_api_key("3910e99aea0a472b50f5cdc422c9a3395b3c87b3")

#v21 <- load_variables(2021, "acs5/subject", cache = TRUE)

my_states = c("MA")
my_vars <- c(
  total_households = "S1901_C01_001"
  , median_household_income = "S1901_C01_012"
)

get_acs_for_year = function(yr) {
  get_acs(
    geography = "tract",
    variables = my_vars,
    state = my_states,
    county = "025",
    year = yr,
    survey="acs5",
    output = "wide",
    geometry = TRUE
  ) 
}

# to do: filter out winthrop and chelsea, standardize color scale bounds across years

for (yr in c(2010, 2016, 2021)) {
  (get_acs_for_year(yr) %>%
    ggplot(aes(fill = median_household_incomeE)) + 
    geom_sf(color = NA) + 
    scale_fill_viridis_c(option = "magma")) %>% print()
}