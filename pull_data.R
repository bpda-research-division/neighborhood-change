# Imports and Setup ##############
library(dplyr)
library(tidyr)
library(sf)
# library(tidycensus)
# options(tigris_use_cache = TRUE)

tract2020_geoms <- read_sf('geoms/boston_tracts_2020_complex.geojson') %>% mutate(GEOID = as.character(geoid20))
tract2010_geoms <- read_sf('geoms/boston_tracts_2010.geojson') %>% mutate(GEOID = as.character(GEOID10))
# neigh2020_geoms <- read_sf('geoms/boston_neighborhoods_2020bg.geojson') %>% mutate(GEOID = BlockGr202)
neigh2020_geoms <- read_sf('geoms/boston_neighborhoods_2020tract.geojson') %>% mutate(GEOID = nbhd)

# Functions ##############

# assumes that sb_csv has columns GEOID, NAME, YEAR, <bin_col_names>
prepare_data <- function(var_code, sb_csv, bin_col_names, agg_func, summary_expression, geoms, cb_csv, ss_csv, cs_csv) {
  if (missing(agg_func)) {
    stop("An agg_func is required")
  }
  
  subcity_bins <- read.csv(sb_csv)
  
  if (!missing(cb_csv)) {city_bins <- read.csv(cb_csv)} 
  else if (missing(agg_func)) {
    stop("If cb_csv is not provided, then agg_func is required")
  }
  else {
    city_bins <- subcity_bins %>% group_by(YEAR) %>% summarise_at(unname(bin_col_names), agg_func, na.rm=TRUE)
  }
  
  if (!missing(ss_csv)) {subcity_summary <- read.csv(ss_csv)}
  else if (missing(summary_expression)) {
    stop("Must either provider a summary_expression or an ss_csv")
  }
  else {
    subcity_summary <- subcity_bins %>% 
      mutate(SUMMARY_VALUE = !!summary_expression) %>% 
      select(-all_of(unname(bin_col_names))) 
  }
  
  if (!missing(cs_csv)) {city_summary <- read.csv(cs_csv)}
  else if (missing(summary_expression)) {
    stop("Must either provider a summary_expression or a cs_csv")
  }
  else {
    city_summary <- city_bins %>% mutate(SUMMARY_VALUE = !!summary_expression) %>% select(-all_of(unname(bin_col_names)))
  }
  
  pivot_long_and_rename_categories <- function(df) {
    out <- df %>% 
      pivot_longer(cols = unname(bin_col_names), 
                   names_to='CATEGORY', 
                   values_to = 'VALUE')
    out$CATEGORY <- plyr::mapvalues(out$CATEGORY, 
                                    from = unname(bin_col_names), 
                                    to = names(bin_col_names))
    out
  }
  
  subcity_bins_app <- subcity_bins %>% pivot_long_and_rename_categories() %>%
    mutate(GEOID = as.character(GEOID))
  city_bins_app <- city_bins %>% pivot_long_and_rename_categories()
  subcity_summary_app <- subcity_summary %>%
    mutate(GEOID = as.character(GEOID)) %>%
    merge(y=geoms, by.y = "GEOID", by.x = "GEOID") %>%
    st_as_sf()
  
  abbrs = list("sb", "cb", "ss", "cs")
  dfs = list(subcity_bins_app, city_bins_app, subcity_summary_app, city_summary)
  
  for (i in 1:4) {
    dfs[[i]] %>% saveRDS(file=sprintf('data/%s_%s.rds', var_code, abbrs[[i]]))
  }
}

# in the income case, we need to manually specify the subcity bins as well as the subcity central,
# if the subcity bins were populations rather than shares, we could at least get the city bins

# the structure of the binned subcity data is GEOID, NAME, <bin_col_names>, YEAR
# the structure of the binned citywide data is <bin_col_names>, YEAR
# and we need a mapping of labels to bin column names in the format of inc_buckets

# the structure of the central subcity data can be GEOID, NAME, SUMMARY_VALUE, YEAR
# the structure of the central citywide data can be SUMMARY_VALUE, YEAR
# and we will eventually need a label for each SUMMARY_VALUE

# In the distant future, this code could be the basis for some kind of interactive wizard where you specify
# a csv and it has you enter the labels as well as the summary expression

# Median Household Income ######################
# census_api_key(read.csv('extra/census_api_key.csv')$key)
# 
# v21 <- load_variables(2021, "acs5/subject", cache = TRUE)
# 
# my_states = c("MA")
# mhi <- c(
#   median_household_income = "S1901_C01_012"
# )
# 
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
# 
# years <- c(2010, 2012, 2014, 2016, 2018)
# 
# get_acs_tract_by_yr <- function(yr, vars, output_type, include_geom) {
#   ct <- get_acs(
#     geography = "tract",
#     variables = vars,
#     state = my_states,
#     county = "025",
#     year = yr,
#     survey="acs5",
#     output = output_type,
#     geometry = include_geom,
#     cache_table = TRUE
#   )
#   ct$YEAR <- yr
#   ct
# }
# 
# get_acs_place_by_year <- function(yr) {
#   data <- get_acs(
#     geography = "place",
#     variables = mhi,
#     state = my_states,
#     year = yr,
#     survey="acs5",
#     output = 'wide',
#     geometry = FALSE,
#     cache_table = TRUE
#   ) %>% subset(GEOID == '2507000')
#   data$YEAR <- yr
#   data
# }
# 
# mhi_bos <- lapply(years, get_acs_place_by_year) %>% bind_rows()
# city_summary <- mhi_bos %>% select(-c("GEOID", "NAME")) %>%
#   rename(MOE = median_household_incomeM, SUMMARY_VALUE = median_household_incomeE)
# city_summary %>% write.csv(file='data/acshhi_cs.csv', row.names=FALSE)
# 
# med_hh_income <- lapply(years, get_acs_tract_by_yr, vars=mhi, output_type="wide", include_geom=FALSE) %>% bind_rows()
# subcity_summary <- med_hh_income %>%
#   mutate(SUMMARY_VALUE = ifelse(startsWith(NAME, "Census Tract 98"), NaN,median_household_incomeE)) %>%
#   mutate(MOE = ifelse(startsWith(NAME, "Census Tract 98"), NaN,median_household_incomeM)) %>%
#   select(-c("median_household_incomeM", "median_household_incomeE"))
# subcity_summary %>% write.csv(file='data/acshhi_ss.csv', row.names=FALSE)
# # subcity_summary %>% saveRDS(file='data/acshhi_ss.rds')
# 
# total_hh <- lapply(years, get_acs_tract_by_yr, vars=c("S1901_C01_001"), output_type="wide", include_geom=FALSE) %>% bind_rows()
# df <- lapply(years, get_acs_tract_by_yr, vars=unname(inc_bckts), output_type = "tidy", include_geom=FALSE) %>% bind_rows() 
# subcity_bins <- left_join(df, 
#                total_hh %>% as_tibble() %>% select(GEOID, S1901_C01_001E, YEAR), 
#                by=c("GEOID", "YEAR")) %>%
#   mutate(across(c(estimate, moe), ~ . * S1901_C01_001E / 100)) %>%
#   mutate(across(c(estimate, moe), round, 0)) %>% 
#   select(-c(S1901_C01_001E, moe)) %>%
#   mutate_at(.vars = c("estimate"), # names(df)[!names(df) %in% c("GEOID", "NAME", "YEAR")]
#             list(~ifelse(startsWith(NAME, "Census Tract 98"), NaN, .))) %>%
#   pivot_wider(names_from = variable, values_from = estimate)
# subcity_bins %>% write.csv(file='data/acshhi_sb.csv', row.names=FALSE)
# 
prepare_data(
  var_code = 'acshhi'
  , sb_csv = 'data/acshhi_sb.csv'
  , bin_col_names = inc_bckts
  , agg_func = sum
  , geoms = tract2010_geoms
  , ss_csv = 'data/acshhi_ss.csv'
  , cs_csv = 'data/acshhi_cs.csv'
)

# HBIC Neighborhoods Labor Force ##################

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

prepare_data(
  var_code = 'hbicnlf', 
  sb_csv = 'data/hbic_neigh_labor_force_bins.csv', 
  agg_func = sum, 
  bin_col_names = labor_force_bins, 
  summary_expression = labor_force_summary_expression,
  geoms = neigh2020_geoms
)

# HBIC Neighborhoods Race and Ethnicity ##################

race_ethn_bins = c(
  "White" = "white",
  "Black/African American" = "black",
  "Hispanic/Latino" = "hisp",
  "Native American" = "native",
  "Asian/Pacific Islander" = "asian",
  "Two or More" = "two_plus",
  "Other" = "other"
)

race_ethn_summary = c(
  "Non-white share of population" = "nw_share"
)

race_ethn_summary_expression <- rlang::expr(
  (black + hisp + asian + native + two_plus + other) /
    (white + black + hisp + asian + native + two_plus + other)
)

prepare_data(
  var_code = 'hbicnre', 
  sb_csv = 'data/hbic_neigh_race_ethn_bins.csv', 
  agg_func = sum, 
  bin_col_names = race_ethn_bins, 
  summary_expression = race_ethn_summary_expression,
  geoms = neigh2020_geoms
)

# HBIC Neighborhoods Age ##################

age_bins = c(
  "0-9 years" = "zero_nine",
  "10-19 years" = "ten_nineteen",
  "20-34 years" = "twenty_thirtyfour",
  "35-54 years" = "thirtyfive_fiftyfour",
  "55-64 years" = "fiftyfive_sixtyfour",
  "65 years and over" = "sixtyfive_more"
)

age_summary = c(
  "Young adult (20-34) share of population" = "twenty_thirtyfour_share"
)

age_summary_expression <- rlang::expr(
  (twenty_thirtyfour) /
    (zero_nine + ten_nineteen + twenty_thirtyfour + 
       thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
)

prepare_data(
  var_code = 'hbicna', 
  sb_csv = 'data/hbic_neigh_age_year_bins.csv', 
  agg_func = sum, 
  bin_col_names = age_bins, 
  summary_expression = age_summary_expression,
  geoms = neigh2020_geoms
)

# HBIC Neighborhoods Educational Attainment ##################

edu_att_bins = c(
  "Less than high school" = "lhs",
  "High school or some equivalent" = "he",
  "Some college" = "sc",
  "Bachelor's or more" = "bm"
)

edu_att_summary = c(
  "Share of population with a bachelor's or more" = "bm_share"
)

edu_att_summary_expression <- rlang::expr(
  (bm) /
    (lhs + he + sc + bm)
)

prepare_data(
  var_code = 'hbicnedu', 
  sb_csv = 'data/hbic_neigh_edu_attain_bins.csv', 
  agg_func = sum, 
  bin_col_names = edu_att_bins, 
  summary_expression = edu_att_summary_expression,
  geoms = neigh2020_geoms
)

# HBIC Neighborhoods Housing Tenure ##################

houten_bins = c(
  "Owner-occupied" = "owner",
  "Renter-occupied" = "renter"
)

houten_summary = c(
  "Owner-occupied housing share" = "owner_share"
)

houten_summary_expression <- rlang::expr(
  (owner) /
    (owner + renter)
)

prepare_data(
  var_code = 'hbicnhouten', 
  sb_csv = 'data/hbic_neigh_housing_tenure_bins.csv', 
  agg_func = sum, 
  bin_col_names = houten_bins, 
  summary_expression = houten_summary_expression,
  geoms = neigh2020_geoms
)

# HBIC Neighborhoods Housing Vacancy ##################

houvac_bins = c(
  "Occupied" = "occ",
  "Vacant" = "vac"
)

houvac_summary = c(
  "Housing vacancy rate" = "vacancy_rate"
)

houvac_summary_expression <- rlang::expr(
  (vac) /
    (occ + vac)
)

prepare_data(
  var_code = 'hbicnhouvac', 
  sb_csv = 'data/hbic_neigh_housing_vacancy_bins.csv', 
  agg_func = sum, 
  bin_col_names = houvac_bins, 
  summary_expression = houvac_summary_expression,
  geoms = neigh2020_geoms
)
  
# HBIC Neighborhoods Nativity ##################

nativity_bins = c(
  "Native-born" = "native",
  "Foreign-born" = "foreign"
)

nativity_summary = c(
  "Foreign born share of population" = "foreign_share"
)

nativity_summary_expression <- rlang::expr(
  (foreign) /
    (foreign + native)
)

prepare_data(
  var_code = 'hbicnnat', 
  sb_csv = 'data/hbic_neigh_nativity_bins.csv', 
  agg_func = sum, 
  bin_col_names = nativity_bins, 
  summary_expression = nativity_summary_expression,
  geoms = neigh2020_geoms
)

# HBIC Tracts Labor Force ##################

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

prepare_data(
  var_code = 'hbictlf', 
  sb_csv = 'data/hbic_tract_labor_force_bins.csv', 
  agg_func = sum, 
  bin_col_names = labor_force_bins, 
  summary_expression = labor_force_summary_expression,
  geoms = tract2020_geoms
)

# HBIC Tracts Race and Ethnicity ##################

race_ethn_bins = c(
  "White" = "white",
  "Black/African American" = "black",
  "Hispanic/Latino" = "hisp",
  "Native American" = "native",
  "Asian/Pacific Islander" = "asian",
  "Two or More" = "two_plus",
  "Other" = "other"
)

race_ethn_summary = c(
  "Non-white share of population" = "nw_share"
)

race_ethn_summary_expression <- rlang::expr(
  (black + hisp + asian + native + two_plus + other) /
    (white + black + hisp + asian + native + two_plus + other)
)

prepare_data(
  var_code = 'hbictre', 
  sb_csv = 'data/hbic_tract_race_ethn_bins.csv', 
  agg_func = sum, 
  bin_col_names = race_ethn_bins, 
  summary_expression = race_ethn_summary_expression,
  geoms = tract2020_geoms
)
# HBIC Tracts Age ##################

age_bins = c(
  "0-9 years" = "zero_nine",
  "10-19 years" = "ten_nineteen",
  "20-34 years" = "twenty_thirtyfour",
  "35-54 years" = "thirtyfive_fiftyfour",
  "55-64 years" = "fiftyfive_sixtyfour",
  "65 years and over" = "sixtyfive_more"
)

age_summary = c(
  "Young adult (20-34) share of population" = "twenty_thirtyfour_share"
)

age_summary_expression <- rlang::expr(
  (twenty_thirtyfour) /
    (zero_nine + ten_nineteen + twenty_thirtyfour + 
       thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
)

prepare_data(
  var_code = 'hbicta', 
  sb_csv = 'data/hbic_tract_age_year_bins.csv', 
  agg_func = sum, 
  bin_col_names = age_bins, 
  summary_expression = age_summary_expression,
  geoms = tract2020_geoms
)

# HBIC Tracts Educational Attainment ##################

edu_att_bins = c(
  "Less than high school" = "lhs",
  "High school or some equivalent" = "he",
  "Some college" = "sc",
  "Bachelor's or more" = "bm"
)

edu_att_summary = c(
  "Share of population with a bachelor's or more" = "bm_share"
)

edu_att_summary_expression <- rlang::expr(
  (bm) /
    (lhs + he + sc + bm)
)

prepare_data(
  var_code = 'hbictedu', 
  sb_csv = 'data/hbic_tract_edu_attain_bins.csv', 
  agg_func = sum, 
  bin_col_names = edu_att_bins, 
  summary_expression = edu_att_summary_expression,
  geoms = tract2020_geoms
)

# HBIC Tracts Housing Tenure ##################

houten_bins = c(
  "Owner-occupied" = "owner",
  "Renter-occupied" = "renter"
)

houten_summary = c(
  "Owner-occupied housing share" = "owner_share"
)

houten_summary_expression <- rlang::expr(
  (owner) /
    (owner + renter)
)

prepare_data(
  var_code = 'hbicthouten', 
  sb_csv = 'data/hbic_tract_housing_tenure_bins.csv', 
  agg_func = sum, 
  bin_col_names = houten_bins, 
  summary_expression = houten_summary_expression,
  geoms = tract2020_geoms
)

# HBIC Tracts Housing Vacancy ##################

houvac_bins = c(
  "Occupied" = "occ",
  "Vacant" = "vac"
)

houvac_summary = c(
  "Housing vacancy rate" = "vacancy_rate"
)

houvac_summary_expression <- rlang::expr(
  (vac) /
    (occ + vac)
)

prepare_data(
  var_code = 'hbicthouvac', 
  sb_csv = 'data/hbic_tract_housing_vacancy_bins.csv', 
  agg_func = sum, 
  bin_col_names = houvac_bins, 
  summary_expression = houvac_summary_expression,
  geoms = tract2020_geoms
)

# HBIC Tracts Nativity ##################

nativity_bins = c(
  "Native-born" = "native",
  "Foreign-born" = "foreign"
)

nativity_summary = c(
  "Foreign born share of population" = "foreign_share"
)

nativity_summary_expression <- rlang::expr(
  (foreign) /
    (foreign + native)
)

prepare_data(
  var_code = 'hbictnat', 
  sb_csv = 'data/hbic_tract_nativity_bins.csv', 
  agg_func = sum, 
  bin_col_names = nativity_bins, 
  summary_expression = nativity_summary_expression,
  geoms = tract2020_geoms
)
