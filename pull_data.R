# Imports and Setup ##############
library(dplyr)
library(tidycensus)
options(tigris_use_cache = TRUE)
setwd(getSrcDirectory(function(){})[1])

# Data loading ######################
# in the future, we will read the key in from a file instead of exposing it here
census_api_key("3910e99aea0a472b50f5cdc422c9a3395b3c87b3")

v21 <- load_variables(2021, "acs5/subject", cache = TRUE)

my_states = c("MA")
mhi <- c(
  median_household_income = "S1901_C01_012"
)

inc_bckts <- c(
  "Less than $10,000" = "S1901_C01_002"
  , "$10,000 to $14,999" = "S1901_C01_003"
  , "$15,000 to $24,999" = "S1901_C01_004"
  , "$25,000 to $34,999" = "S1901_C01_005"
  , "$35,000 to $49,999" = "S1901_C01_006"
  , "$50,000 to $74,999" = "S1901_C01_007"
  , "$75,000 to $99,999" = "S1901_C01_008"
  , "$100,000 to $149,999" = "S1901_C01_009"
  , "$150,000 to $199,999" = "S1901_C01_010"
  , "More than $200,000" = "S1901_C01_011"
)

years <- c(2010, 2012, 2014, 2016, 2018)

get_acs_by_yr <- function(yr, vars, output_type) {
  ct <- get_acs(
    geography = "tract",
    variables = vars,
    state = my_states,
    county = "025",
    year = yr,
    survey="acs5",
    output = output_type,
    geometry = TRUE,
    cache_table = TRUE
  )
  ct$year <- yr
  ct
}

med_hh_income <- lapply(years, get_acs_by_yr, vars=mhi, output_type="wide") %>% bind_rows()
subcity_summary <- med_hh_income %>%
  mutate(median_household_incomeE = ifelse(startsWith(NAME, "Census Tract 98"), NaN,median_household_incomeE)) %>%
  select(-c("median_household_incomeM")) %>% sf::st_transform(4326)

total_hh <- lapply(years, get_acs_by_yr, vars=c("S1901_C01_001"), output_type="wide") %>% bind_rows()
df <- lapply(years, get_acs_by_yr, vars=inc_bckts, output_type = "tidy") %>% bind_rows() 
t <- left_join(df, total_hh %>% as_tibble() %>% select(GEOID, S1901_C01_001E, year), by=c("GEOID", "year"))
t$estimate <- round(t$S1901_C01_001E * t$estimate / 100, digits = 0)
t$moe <- round(t$S1901_C01_001E * t$moe / 100, digits = 0)
subcity_bins <- t %>% select(-c(S1901_C01_001E)) %>% sf::st_transform(4326) %>%
  mutate_at(.vars = names(df)[!names(df) %in% c("GEOID", "NAME", "geometry", "year")], 
            list(~ifelse(startsWith(NAME, "Census Tract 98"), NaN, .)))

# maybe prepare_data can accept override csv files for subcity central and city central. what about city bins?
# subcity bins is definitely required. if subcity summary not overridden, then summary expression required
# if city bins not overridden, then agg_func required. or is agg func always required because of multiselection?

# for city bins, we can use sum as agg_func. still need city summary

df %>% saveRDS(file='data/tract_hh_income_brackets_geo.rds')

# in the income case, we need to manually specify the subcity bins as well as the subcity central,
# if the subcity bins were populations rather than shares, we could at least get the city bins

# the structure of the binned subcity data is GEOID, NAME, <bincolnames>, YEAR
# the structure of the binned citywide data is <bincolnames>, YEAR
# and we need a mapping of labels to column names in the format of inc_buckets

# the structure of the central subcity data can be GEOID, NAME, value, YEAR
# the structure of the central citywide data can be value, YEAR
# and we will eventually need a mapping of the column name to the label

labor_force_bins = c(
  "Male in labor force" = "ilf_m"
  , "Female in labor force" = "ilf_f"
  , "Male not in labor force" = "nilf_m"
  , "Female not in labor force" = "nilf_f"
)

labor_force_summary = c(
  "Female labor force participation rate" = "lbf_rate_f"
)

labor_force_summary_expression <- rlang::expr(ilf_f / (ilf_f + nilf_f))

# assumes that in_csv has columns GEOID, NAME, YEAR, <bin_col_names>

prepare_data <- function(var_code, in_csv, agg_func, bin_col_names, summary_expression) {
  subcity_bins <- read.csv(in_csv)
  city_bins <- subcity_bins %>% group_by(YEAR) %>% summarise_at(bin_col_names, agg_func)
  subcity_summary <- subcity_bins %>% mutate(SUMMARY_VAL = !!summary_expression) %>% select(-all_of(bin_col_names))
  city_summary <- city_bins %>% mutate(SUMMARY_VAL = !!summary_expression) %>% select(-all_of(bin_col_names))
  
  subcity_bins %>% saveRDS(file=sprintf('data/%s_sb.rds', var_code))
  city_bins %>% saveRDS(file=sprintf('data/%s_cb.rds', var_code))
  subcity_summary %>% saveRDS(file=sprintf('data/%s_ss.rds', var_code))
  city_summary %>% saveRDS(file=sprintf('data/%s_cs.rds', var_code))
}

# In the distant future, this code could be the basis for some kind of interactive wizard where you specify
# a csv and it has you enter the labels as well as the summary expression
prepare_data('hbicn', 'data/hbic_neigh_labor_force_bins.csv', sum, unname(labor_force_bins), labor_force_summary_expression)