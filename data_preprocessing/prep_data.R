# Imports and Setup ##############
library(dplyr)
library(tidyr)
library(sf)

tract2020_geoms <- read_sf('../geoms/boston_tracts_2020_complex.geojson') %>% mutate(GEOID = as.character(geoid20))
tract2010_geoms <- read_sf('../geoms/boston_tracts_2010.geojson') %>% mutate(GEOID = as.character(GEOID10))
neigh2020_geoms <- read_sf('../geoms/boston_neighborhoods_2020tract_2.geojson') 
zca_geoms <- read_sf('../geoms/boston_zipcodeAreas_complex.geojson')

# Functions ##########

pivot_long_and_rename_categories <- function(df, bin_col_names) {
  out <- df %>% 
    pivot_longer(cols = unlist(unname(bin_col_names)), 
                 names_to='CATEGORY', 
                 values_to = 'VALUE')
  out$CATEGORY <- plyr::mapvalues(out$CATEGORY, 
                                  from = unname(bin_col_names), 
                                  to = names(bin_col_names))
  out
}

prep_data <- function(topic) {
  sb_raw <- read.csv(topic$areas_categories_csv) 
  bc <- topic$barCats
  topicCode <- topic$data_code
  
  # first, some validations of the data and parameters
  if (!all(c("YEAR", "NAME", "GEOID") %in% names(sb_raw))) {
    stop(paste("Error: areas_categories_csv for topic", topic$data_code, "must have columns YEAR, NAME, and GEOID. 
         More information: https://github.com/bpda-research-division/neighborhood-change/blob/main/ABOUT.md#preparing-tabular-data"))
  }
  
  sb <- sb_raw %>% distinct(GEOID, NAME, YEAR, .keep_all = TRUE)
  
  if (length(sb) != length(sb_raw)) {
    print(paste("Warning: areas_categories_csv for topic", topic$data_code, "has duplicate values. 
                There should be exactly one observation for each combination of GEOID, NAME, and YEAR.
                The first observation of each duplicate was kept."))
  }
  
  if (!all(bc %in% names(sb))) {
    stop(paste("Error: one or more columns in barCats for topic", topic$data_code, "are not present in areas_categories_csv. 
         More information: https://github.com/bpda-research-division/neighborhood-change/blob/main/ABOUT.md#topic-parameters"))
  }
  
  for (c in names(sb)) {
    if (!(c %in% c("YEAR", "NAME", "GEOID")) & !(c %in% bc)) {
      bc[[c]] <- c
    }
  }
  subcity_bins <- sb %>% # read.csv(topic$areas_categories_csv)
    pivot_long_and_rename_categories(bin_col_names = bc) %>% # topic$barCats
    mutate(GEOID = as.character(GEOID))
  
  dfs = list("areas_categories_df" = subcity_bins)
  if ("totalarea_categories_csv" %in% names(topic)) {
    dfs$totalarea_categories_df <- read.csv(topic$totalarea_categories_csv) %>% pivot_long_and_rename_categories(bin_col_names = bc) # topic$barCats
  }
  indicators = list()
  for (ind_name in names(topic$summary_indicators)) {
    ind_dfs = list()
    for (x in c("areas_summary", "totalarea_summary")) {
      if (paste0(x, "_csv") %in% names(topic$summary_indicators[[ind_name]])) {
        ind_dfs[[paste0(x, "_df")]] = read.csv(topic$summary_indicators[[ind_name]][[paste0(x, "_csv")]])
      }
    }
    if (length(ind_dfs) > 0) {
      indicators[[ind_name]] = ind_dfs
    }
  }
  if (length(indicators) > 0) {
    dfs$indicators = indicators
  }

  dfs %>% saveRDS(file=sprintf('../data/%s.rds', topic$data_code))
}

# Define parameters for each geography type and variable ##########

APP_CONFIG <- list(
  ## 2020 tracts ------
  "census tracts" = list(geoms = tract2020_geoms, center_lat = 42.318, center_lon = -71.075, zoom_level = 12, topics = list(
    ### pop by sex -----
    "Population" = list(
      data_code = 'hbicttp', generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_tract_totpop_sex_bins.csv', totalarea_categories_csv = 'csv/hbictpop_cb.csv',
      barTitle = "Population by sex", barhoverformat = ",.0f", 
      null_description = "little to no population",
      barCats = list("Male" = "male", "Female" = "female"),
      summary_indicators = list(
        "Total Population" = list(
          summary_expression = rlang::expr(male + female),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Population per square mile" = list(
          summary_expression = rlang::expr((male + female) / landarea_sqmiles),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Female share of population" = list(
          summary_expression = rlang::expr(female / (male + female)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses (with 2020 adjusted
        to reflect Boston's successful group quarters challenge); IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### age -----
    "Age" = list(
      data_code = "hbicta", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_tract_age_year_bins.csv',
      barTitle = "Population by age", barhoverformat = ",.0f",
      null_description = "little to no population",
      barCats = list(
        "0-9 years" = "zero_nine",
        "10-19 years" = "ten_nineteen",
        "20-34 years" = "twenty_thirtyfour",
        "35-54 years" = "thirtyfive_fiftyfour",
        "55-64 years" = "fiftyfive_sixtyfour",
        "65 years and over" = "sixtyfive_more"
       ),
     summary_indicators = list(
       "Share of population aged 20-34" = list(
         summary_expression = rlang::expr(
           (twenty_thirtyfour) /
             (zero_nine + ten_nineteen + twenty_thirtyfour +
                thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
         ),
         citywide_comparison = TRUE,
         hoverformat = ".0%", tickformat = ".0%"
       ),
       "Total population aged 20-34" = list(
         summary_expression = rlang::expr(twenty_thirtyfour),
         citywide_comparison = FALSE,
         hoverformat = ",.0f", tickformat = ""
       ),
       "Share of population aged 0-9" = list(
         summary_expression = rlang::expr(
           (zero_nine) /
             (zero_nine + ten_nineteen + twenty_thirtyfour +
                thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
         ),
         citywide_comparison = TRUE,
         hoverformat = ".0%", tickformat = ".0%"
       ),
       "Total population aged 0-9" = list(
         summary_expression = rlang::expr(zero_nine),
         citywide_comparison = FALSE,
         hoverformat = ",.0f", tickformat = ""
       ),
       "Share of population aged 55+" = list(
         summary_expression = rlang::expr(
           (fiftyfive_sixtyfour + sixtyfive_more) /
             (zero_nine + ten_nineteen + twenty_thirtyfour +
                thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
         ),
         citywide_comparison = TRUE,
         hoverformat = ".0%", tickformat = ".0%"
       ),
       "Total population aged 55+" = list(
         summary_expression = rlang::expr(fiftyfive_sixtyfour + sixtyfive_more),
         citywide_comparison = FALSE,
         hoverformat = ",.0f", tickformat = ""
       )
     ),
     source = "U.S. Census Bureau, 1950-2020 Decennial Censuses,
      IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### children by age ------
    "Children by Age" = list(
      data_code = "chilta", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/children_tract_age_bins.csv',
      barTitle = "Children by age", barhoverformat = ",.0f",
      null_description = "little to no population",
      barCats = list(
        "0-4 years" = "under5",
        "5-17 years" = "fiveto17"
      ),
      summary_indicators = list(
        "Total children (0-17)" = list(
          summary_expression = rlang::expr(under5 + fiveto17),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total children aged 5-17" = list(
          summary_expression = rlang::expr(fiveto17),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total children aged 0-4" = list(
          summary_expression = rlang::expr(under5),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      additional_null_geoms = c("25025981100"),
      source = "U.S. Census Bureau, 1980-2020 Decennial Censuses,
      IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### race & ethnicity -----
    "Race and Ethnicity" = list(
      data_code = "hbictre", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_tract_race_ethn_bins.csv',
      barTitle = "Population by race/ethnicity", barhoverformat = ",.0f",
      null_description = "little to no population",
      barCats = list(
        "White" = "white",
        "Black/African American" = "black",
        "Hispanic/Latino" = "hisp",
        "Asian/Pacific Islander" = "asian",
        "American Indian" = "native",
        "Two or More" = "two_plus",
        "Other" = "other"
      ),
      summary_indicators = list(
        "Non-white share of population" = list(
          summary_expression = rlang::expr(
            (black + hisp + asian + native + two_plus + other) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
          ),
        "Share of population, Black alone" = list(
          summary_expression = rlang::expr(
            (black) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
          ),
        "Share of population, Hispanic of any race" = list(
          summary_expression = rlang::expr(
            (hisp) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
          ),
        "Share of population, Asian alone" = list(
          summary_expression = rlang::expr(
            (asian) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
          )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis",
      note = "Note: In 1950 and 1960, the only race/ethnicity categories on the Census were White, Black, and Other."
    ),
    ### children by race & ethnicity -----
    "Children by Race and Ethnicity" = list(
      data_code = "chiltre", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/children_tract_race_bins.csv',
      barTitle = "Children by race/ethnicity", barhoverformat = ",.0f",
      null_description = "little to no population",
      barCats = list(
        "White" = "white",
        "Black/African American" = "black",
        "Hispanic/Latino" = "hisp",
        "Asian/Pacific Islander" = "aapi",
        "American Indian" = "ainh",
        "Two or More" = "twoplus",
        "Other" = "other"
      ),
      summary_indicators = list(
        "Non-white share of children" = list(
          summary_expression = rlang::expr(
            (black + hisp + aapi + ainh + twoplus + other) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of children, Black alone" = list(
          summary_expression = rlang::expr(
            (black) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of children, Hispanic of any race" = list(
          summary_expression = rlang::expr(
            (hisp) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of children, Asian alone" = list(
          summary_expression = rlang::expr(
            (aapi) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      additional_null_geoms = c("25025981100"),
      source = "U.S. Census Bureau, 1980-2020 Decennial Censuses, IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis",
      note = "Note: Two or More races did not become an option on the decennial census until 2000."
    ),
    ### nativity ------
    "Nativity" = list(
      data_code = "hbictnat", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_tract_nativity_bins.csv',
      barTitle = "Population by nativity", barhoverformat = ",.0f",
      barCats = list("Native-born" = "native", "Foreign-born" = "foreign"),
      null_description = "little to no population",
      summary_indicators = list(
        "Foreign-born share of population" = list(
          summary_expression = rlang::expr(foreign / (foreign + native)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total foreign-born population" = list(
          summary_expression = rlang::expr(foreign),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### educ attainment -----
    "Educational Attainment" = list(
      data_code = "hbictedu", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_tract_edu_attain_bins.csv',
      barTitle = "Population (25+) by educational attainment", barhoverformat = ",.0f",
      null_description = "little to no population",
      barCats = list(
        "Less than high school" = "lhs",
        "High school or some equivalent" = "he",
        "Some college" = "sc",
        "Bachelor's or more" = "bm"
      ),
      summary_indicators = list(
        "Share of population (25+) with bachelor's or more" = list(
          summary_expression = rlang::expr(bm / (lhs + he + sc + bm)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of population (25+) with less than high school" = list(
          summary_expression = rlang::expr(lhs / (lhs + he + sc + bm)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### labor force ----
    "Labor Force" = list(
      data_code = "hbictlf", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_tract_labor_force_bins.csv',
      barTitle = "Population (16+) by labor force status and sex", barhoverformat = ",.0f",
      null_description = "little to no population",
      barCats = list(
        "Male in labor force" = "ilf_m"
        , "Male not in labor force" = "nilf_m"
        , "Female in labor force" = "ilf_f"
        , "Female not in labor force" = "nilf_f"
      ),
      summary_indicators = list(
        "Female labor force participation rate (16+)" = list(
          summary_expression = rlang::expr(ilf_f / (ilf_f + nilf_f)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Male labor force participation rate (16+)" = list(
          summary_expression = rlang::expr(ilf_m / (ilf_m + nilf_m)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### ratio of income to poverty -----
    "Ratio of Income to Poverty" = list(
      data_code = "inc2povt", generalTopic = "Demographics",
      areas_categories_csv = "csv/inc2pov_tract_bins.csv",
      barTitle = "Population by Ratio of Income to Poverty", barhoverformat = ",.0f",
      null_description = "little to no population",
      additional_null_geoms = c(
        paste0("25025", c("061202", "981201", "981202", "981501", "981502")), 
        paste0("2502598", c("03", "07", "10", "11", "13", "16", "17", "18", "19"), "00")
      ),
      barCats = list(
        "Under 0.5" = "c20aa", "0.5 to 0.74" = "c20ab", "0.75 to 0.99" = "c20ac",
        "1.00 to 1.24" = "c20ad", "1.25 to 1.49" = "c20ae", "1.50 to 1.74" = "c20af",
        "1.75 to 1.84" = "c20ag", "1.85 to 1.99" = "c20ah", "2.00 and over" = "c20ai"
      ),
      summary_indicators = list(
        "Share of population under poverty line" = list(
          summary_expression = rlang::expr(
            (c20aa + c20ab + c20ac) / 
              (c20aa + c20ab + c20ac + c20ad + c20ae + c20af + c20ag + c20ah + c20ai)
            ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total population under poverty line" = list(
          summary_expression = rlang::expr(c20aa + c20ab + c20ac),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Share of population under 200% of poverty line" = list(
          summary_expression = rlang::expr(
            (c20aa + c20ab + c20ac + c20ad + c20ae + c20af + c20ag + c20ah) / 
              (c20aa + c20ab + c20ac + c20ad + c20ae + c20af + c20ag + c20ah + c20ai)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### living arrangements -------
    "Living Arrangements" = list(
      data_code = "hhgqt", generalTopic = "Demographics",
      areas_categories_csv = 'csv/hh_gq_tract_bins.csv',
      barTitle = "Population by living arrangement type", barhoverformat = ",.0f",
      null_description = "little to no population",
      additional_null_geoms = c(
        paste0("25025", c("061202", "981201", "981202", "981501", "981502")), 
        paste0("2502598", c("03", "07", "10", "11", "13", "16", "17", "18", "19"), "00")
        ),
      barCats = list(
        "Household population" = "hh",
        "Group quarters population" = "gq_adj"
      ),
      summary_indicators = list(
        "Average household size" = list(
          summary_expression = rlang::expr(hh / hu),
          citywide_comparison = TRUE, map_legend_bins = seq(0, 4, length.out = 9),
          hoverformat = ",.2f", tickformat = ""
        ),
        "Total household population" = list(
          summary_expression = rlang::expr(hh),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total group quarters population" = list(
          summary_expression = rlang::expr(gq_adj),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses (with 2020 adjusted
        to reflect Boston's successful group quarters challenge), IPUMS-NHGIS, University of
      Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### housing units ----
    "Housing Units" = list(
      data_code = "hbicthou", generalTopic = 'Housing',
      areas_categories_csv = 'csv/hbic_tract_housing_bins.csv',
      totalarea_categories_csv = 'csv/hbicthou_cb.csv',
      barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
      null_description = "little to no housing",
      barCats = list(
        "Occupied" = "occ",
        "Vacant" = "vac"
      ),
      summary_indicators = list(
        "Total housing units" = list(
          summary_expression = rlang::expr(vac + occ),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Housing units per acre" = list(
          summary_expression = rlang::expr((vac + occ) / landarea_acres),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of
      Minnesota, www.nhgis.org; Mayor's Office of Housing; BPDA Research Division Analysis"
    ),
    ### housing occupancy -----
    "Housing Occupancy" = list(
      data_code = "hbicthouvac", generalTopic = 'Housing',
      areas_categories_csv = 'csv/hbic_tract_housing_vacancy_bins.csv',
      totalarea_categories_csv = 'csv/hbicthouvac_cb.csv',
      barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
      null_description = "little to no housing",
      barCats = list(
        "Occupied" = "occ",
        "Vacant" = "vac"
      ),
      summary_indicators = list(
        "Housing vacancy rate" = list(
          summary_expression = rlang::expr(vac / (vac + occ)),
          citywide_comparison = TRUE,
          hoverformat = ".1%", tickformat = ".0%"
        ),
        "Total vacant units" = list(
          summary_expression = rlang::expr(vac),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### housing tenure ----
    "Housing Tenure" = list(
      data_code = "hbicthouten", generalTopic = 'Housing',
      areas_categories_csv = 'csv/hbic_tract_housing_tenure_bins.csv',
      barTitle = "Occupied housing units by tenure", barhoverformat = ",.0f",
      null_description = "little to no housing",
      barCats = list(
        "Owner-occupied" = "owner",
        "Renter-occupied" = "renter"
      ),
      summary_indicators = list(
        "Owner occupancy rate" = list(
          summary_expression = rlang::expr(owner / (owner + renter)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2010 Decennial Censuses, 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### commute mode ----
    "Commute Mode" = list(
      data_code = "commute_mode_tracts", generalTopic = 'Transportation',
      areas_categories_csv = 'csv/commutemode_tract_bins.csv',
      barTitle = "Workers (16+) by primary commute mode", barhoverformat = ",.0f",
      null_description = "little to no population",
      additional_null_geoms = c(25025981100),
      barCats = list(
        "Car, truck, or van" = "car_truck_van",
        "Public transit" = "public_transportation",
        "Bicycle" = "bicycle",
        "Walked" = "walked",
        "Other means" = "other_means",
        "Worked from home" = "worked_from_home"
      ),
      summary_indicators = list(
        "Share of commuters (16+) walking or biking" = list(
          summary_expression = rlang::expr((bicycle + walked) /
                                             (total_workers_16_and_over - worked_from_home)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of commuters (16+) taking public transit" = list(
          summary_expression = rlang::expr(public_transportation /
                                             (total_workers_16_and_over - worked_from_home)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of commuters (16+) walking, biking, or taking transit" = list(
          summary_expression = rlang::expr((bicycle + walked + public_transportation) /
                                             (total_workers_16_and_over - worked_from_home)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of commuters (16+) taking a car, truck, or van" = list(
          summary_expression = rlang::expr(car_truck_van /
                                             (total_workers_16_and_over - worked_from_home)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of workers (16+) working from home" = list(
          summary_expression = rlang::expr(worked_from_home / (total_workers_16_and_over)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      note = "Note: Workers who commute by taxicab are included in the 'public transit' category.",
      source = "U.S. Census Bureau, 1990-2000 Decennial Censuses, 2006-2010 and 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### vehicles available ----
    "Vehicles Available" = list(
      data_code = "vehicles_available_tracts", generalTopic = 'Transportation',
      areas_categories_csv = 'csv/vehiclesavailable_tract_bins.csv',
      barTitle = "Households by vehicles available", barhoverformat = ",.0f",
      null_description = "little to no households",
      additional_null_geoms = c(25025981100),
      barCats = list(
        "0 vehicles" = "no_vehicles",
        "1 vehicle" = "one_vehicle",
        "2 vehicles" = "two_vehicles",
        "3+ vehicles" = "three_or_more_vehicles"
      ),
      summary_indicators = list(
        "Share of households with no vehicles available" = list(
          summary_expression = rlang::expr(no_vehicles / total_occupied_units),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of households with two or more vehicles available" = list(
          summary_expression = rlang::expr((two_vehicles + three_or_more_vehicles) / total_occupied_units),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1960-2000 Decennial Censuses, 2006-2010 and 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### num of home loans ----
    "Number of Home Loans" = list(
      data_code = "numholoanstract", generalTopic = 'Loans',
      areas_categories_csv = 'csv/home_loans_tract_bins.csv',
      barTitle = "Number of home loans by type", barhoverformat = ",.0f",
      null_description = "little to no loans",
      additional_null_geoms = c(25025981202, 25025981800, 25025981300, 25025981700),
      barCats = list(
        "Home purchase" = "num_home_purchase",
        "Home improvement" = "num_home_improv",
        "Refinancing" = "num_home_refi"
      ),
      summary_indicators = list(
        "Total home purchase loans" = list(
          summary_expression = rlang::expr(num_home_purchase),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total home improvement loans" = list(
          summary_expression = rlang::expr(num_home_improv),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total home loans" = list(
          summary_expression = rlang::expr(num_home_purchase + num_home_improv + num_home_refi),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      note = "Note: All originated loans for single- and multi-family properties are included.",
      source = "Consumer Financial Protection Bureau, Home Mortgage Disclosure Act 2007-2022; BPDA Research Division Analysis"
    ),
    ### $ of home loans ----
    "Volume ($) of Home Loans" = list(
      data_code = "volholoanstract", generalTopic = 'Loans',
      areas_categories_csv = 'csv/home_loans_tract_bins.csv',
      barTitle = "Volume ($) of home loans by type", barhoverformat = ",.0f", bartickprefix = "$",
      null_description = "little to no loans",
      additional_null_geoms = c(25025981202, 25025981800, 25025981300, 25025981700),
      barCats = list(
        "Home purchase" = "vol_home_purchase",
        "Home improvement" = "vol_home_improv",
        "Refinancing" = "vol_home_refi"
      ),
      summary_indicators = list(
        "Total home purchase loan volume ($)" = list(
          summary_expression = rlang::expr(vol_home_purchase),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total home improvement loan volume ($)" = list(
          summary_expression = rlang::expr(vol_home_improv),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total home loan volume ($)" = list(
          summary_expression = rlang::expr(vol_home_purchase + vol_home_improv + vol_home_refi),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Average dollar value of home purchase loans" = list(
          summary_expression = rlang::expr(vol_home_purchase / num_home_purchase),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Average dollar value of home improvement loans" = list(
          summary_expression = rlang::expr(vol_home_improv / num_home_improv),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        )
      ),
      note = "Note: All originated loans for single- and multi-family properties are included. All monetary values are in 2022 inflation-adjusted dollars.",
      source = "Consumer Financial Protection Bureau, Home Mortgage Disclosure Act 2007-2022; BPDA Research Division Analysis"
    ),
    ### num of sm. bus. loans ----
    "Number of Small Business Loans" = list(
      data_code = "numbuloanstract", generalTopic = 'Loans',
      areas_categories_csv = 'csv/bus_loans_tract_bins.csv',
      barTitle = 'Number of Small (<$1M) Loans by Business Size', barhoverformat = ",.0f",
      barCats = list("Loans to small businesses" = "num_sml_sbus", "Loans to large businesses" = "num_sml_bbus"),
      null_description = "little to no loans",
      additional_null_geoms = c(25025981100),
      summary_indicators = list(
         "Share of small loans going to small businesses" = list(
          summary_expression = rlang::expr(num_sml_sbus/(num_sml_sbus + num_sml_bbus)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total small loans" = list(
          summary_expression = rlang::expr(num_sml_sbus + num_sml_bbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total small loans going to small businesses" = list(
          summary_expression = rlang::expr(num_sml_sbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      note = "Note: A business is considered small if it has <$1M in annual revenue.",
      source = "FFIEC, Community Reinvestment Act data, https://github.com/acforrester/community-reinvestment-act; BPDA Research Division analysis"
    ),
    ### $ of sm. bus. loans ----
    "Volume ($) of Small Business Loans" = list(
      data_code = 'volbuloanstract', generalTopic = 'Loans',
      areas_categories_csv = 'csv/bus_loans_tract_bins.csv',
      barTitle = 'Volume ($) of Small (<$1M) Loans by Business Size', barhoverformat = ",.0f", bartickprefix = "$",
      barCats = list("Loans to small businesses" = "vol_sml_sbus", "Loans to large businesses" = "vol_sml_bbus"),
      null_description = "little to no loans",
      additional_null_geoms = c(25025981100),
      summary_indicators = list(
        "Average dollar value of all small loans" = list(
          summary_expression = rlang::expr((vol_sml_sbus + vol_sml_bbus)/(num_sml_sbus + num_sml_bbus)),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Average dollar value of small loans to small businesses" = list(
          summary_expression = rlang::expr((vol_sml_sbus)/(num_sml_sbus)),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Average dollar value of small loans to large businesses" = list(
          summary_expression = rlang::expr((vol_sml_bbus)/(num_sml_bbus)),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total small loan volume ($)" = list(
          summary_expression = rlang::expr(vol_sml_sbus + vol_sml_bbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total small loan volume ($) going to small businesses" = list(
          summary_expression = rlang::expr(vol_sml_sbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total small loan volume ($) going to large businesses" = list(
          summary_expression = rlang::expr(vol_sml_bbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Share of small loan volume ($) going to small businesses" = list(
          summary_expression = rlang::expr(vol_sml_sbus/(vol_sml_sbus + vol_sml_bbus)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      note = "Note: A business is considered small if it has <$1M in annual revenue. All monetary values are in 2022 inflation-adjusted dollars.",
      source = "FFIEC, Community Reinvestment Act data, https://github.com/acforrester/community-reinvestment-act; BPDA Research Division analysis"
    )
  )
  ),
  ## neighborhoods -----
  "neighborhoods" = list(geoms = neigh2020_geoms, center_lat = 42.318, center_lon = -71.075, zoom_level = 12, topics = list(
    ### pop by sex ----
    "Population" = list(
      data_code = 'hbicntp', generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_neigh_totpop_sex_bins.csv', totalarea_categories_csv = 'csv/hbictpop_cb.csv',
      barTitle = "Population by sex", barhoverformat = ",.0f",
      barCats = list("Male" = "male", "Female" = "female"),
      summary_indicators = list(
        "Total Population" = list(
          summary_expression = rlang::expr(male + female),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Population per square mile" = list(
          summary_expression = rlang::expr((male + female) / landarea_sqmiles),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickformat = ""
        ),
        # "Average population per occupied housing unit" = list(
        #   summary_expression = rlang::expr((male + female) / occ_hu),
        #   citywide_comparison = TRUE,
        #   hoverformat = ".1f", tickformat = ""
        # ),
        "Female share of population" = list(
          summary_expression = rlang::expr(female / (male + female)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses (with 2020 adjusted
        to reflect Boston's successful group quarters challenge); IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### age ----
    "Age" = list(
      data_code = "hbicna", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_neigh_age_year_bins.csv',
      barTitle = "Population by age", barhoverformat = ",.0f",
      barCats = list(
        "0-9 years" = "zero_nine",
        "10-19 years" = "ten_nineteen",
        "20-34 years" = "twenty_thirtyfour",
        "35-54 years" = "thirtyfive_fiftyfour",
        "55-64 years" = "fiftyfive_sixtyfour",
        "65 years and over" = "sixtyfive_more"
      ),
      summary_indicators = list(
        "Share of population aged 20-34" = list(
          summary_expression = rlang::expr(
            (twenty_thirtyfour) /
              (zero_nine + ten_nineteen + twenty_thirtyfour +
                 thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total population aged 20-34" = list(
          summary_expression = rlang::expr(twenty_thirtyfour),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Share of population aged 0-9" = list(
          summary_expression = rlang::expr(
            (zero_nine) /
              (zero_nine + ten_nineteen + twenty_thirtyfour +
                 thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total population aged 0-9" = list(
          summary_expression = rlang::expr(zero_nine),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Share of population aged 55+" = list(
          summary_expression = rlang::expr(
            (fiftyfive_sixtyfour + sixtyfive_more) /
              (zero_nine + ten_nineteen + twenty_thirtyfour +
                 thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total population aged 55+" = list(
          summary_expression = rlang::expr(fiftyfive_sixtyfour + sixtyfive_more),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses,
      IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### children by age -----
    "Children by Age" = list(
      data_code = "chilna", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/children_neigh_age_bins.csv',
      barTitle = "Children by age", barhoverformat = ",.0f",
      barCats = list(
        "0-4 years" = "under5",
        "5-17 years" = "fiveto17"
      ),
      summary_indicators = list(
        "Total children (0-17)" = list(
          summary_expression = rlang::expr(under5 + fiveto17),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total children aged 5-17" = list(
          summary_expression = rlang::expr(fiveto17),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total children aged 0-4" = list(
          summary_expression = rlang::expr(under5),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1980-2020 Decennial Censuses,
      IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### race & ethnicity -----
    "Race and Ethnicity" = list(
      data_code = "hbicnre", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_neigh_race_ethn_bins.csv',
      barTitle = "Population by race/ethnicity", barhoverformat = ",.0f",
      barCats = list(
        "White" = "white",
        "Black/African American" = "black",
        "Hispanic/Latino" = "hisp",
        "Asian/Pacific Islander" = "asian",
        "American Indian" = "native",
        "Two or More" = "two_plus",
        "Other" = "other"
      ),
      summary_indicators = list(
        "Non-white share of population" = list(
          summary_expression = rlang::expr(
            (black + hisp + asian + native + two_plus + other) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of population, Black alone" = list(
          summary_expression = rlang::expr(
            (black) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of population, Hispanic of any race" = list(
          summary_expression = rlang::expr(
            (hisp) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of population, Asian alone" = list(
          summary_expression = rlang::expr(
            (asian) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of population, two or more races" = list(
          summary_expression = rlang::expr(
            (two_plus) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis",
      note = "Note: In 1950 and 1960, the only race/ethnicity categories on the Census were White, Black, and Other. Two or more races became an option in 2000."
    ),
    ### children by race & ethnicity ----
    "Children by Race and Ethnicity" = list(
      data_code = "chilnre", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/children_neigh_race_bins.csv',
      barTitle = "Children by race/ethnicity", barhoverformat = ",.0f",
      barCats = list(
        "White" = "white",
        "Black/African American" = "black",
        "Hispanic/Latino" = "hisp",
        "Asian/Pacific Islander" = "aapi",
        "American Indian" = "ainh",
        "Two or More" = "twoplus",
        "Other" = "other"
      ),
      summary_indicators = list(
        "Non-white share of children" = list(
          summary_expression = rlang::expr(
            (black + hisp + aapi + ainh + twoplus + other) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of children, Black alone" = list(
          summary_expression = rlang::expr(
            (black) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of children, Hispanic of any race" = list(
          summary_expression = rlang::expr(
            (hisp) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of children, Asian alone" = list(
          summary_expression = rlang::expr(
            (aapi) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1980-2020 Decennial Censuses, IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis",
      note = "Note: Two or More races did not become an option on the decennial census until 2000."
    ),
    ### nativity ----
    "Nativity" = list(
      data_code = "hbicnnat", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_neigh_nativity_bins.csv',
      barTitle = "Population by nativity", barhoverformat = ",.0f",
      barCats = list("Native-born" = "native", "Foreign-born" = "foreign"),
      summary_indicators = list(
        "Foreign-born share of population" = list(
          summary_expression = rlang::expr(foreign / (foreign + native)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total foreign-born population" = list(
          summary_expression = rlang::expr(foreign),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### educ attainment ----
    "Educational Attainment" = list(
      data_code = "hbicnedu", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_neigh_edu_attain_bins.csv',
      barTitle = "Population (25+) by educational attainment", barhoverformat = ",.0f",
      barCats = list(
        "Less than high school" = "lhs",
        "High school or some equivalent" = "he",
        "Some college" = "sc",
        "Bachelor's or more" = "bm"
      ),
      summary_indicators = list(
        "Share of population (25+) with bachelor's or more" = list(
          summary_expression = rlang::expr(bm / (lhs + he + sc + bm)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of population (25+) with less than high school" = list(
          summary_expression = rlang::expr(lhs / (lhs + he + sc + bm)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### labor force ----
    "Labor Force" = list(
      data_code = "hbicnlf", generalTopic = 'Demographics',
      areas_categories_csv = 'csv/hbic_neigh_labor_force_bins.csv',
      barTitle = "Population (16+) by labor force status and sex", barhoverformat = ",.0f",
      barCats = list(
        "Male in labor force" = "ilf_m"
        , "Male not in labor force" = "nilf_m"
        , "Female in labor force" = "ilf_f"
        , "Female not in labor force" = "nilf_f"
      ),
      summary_indicators = list(
        "Female labor force participation rate (16+)" = list(
          summary_expression = rlang::expr(ilf_f / (ilf_f + nilf_f)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Male labor force participation rate (16+)" = list(
          summary_expression = rlang::expr(ilf_m / (ilf_m + nilf_m)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### ratio of income to poverty -----
    "Ratio of Income to Poverty" = list(
      data_code = "inc2povn", generalTopic = "Demographics",
      areas_categories_csv = "csv/inc2pov_neigh_bins.csv",
      barTitle = "Population by Ratio of Income to Poverty", barhoverformat = ",.0f",
      null_description = "little to no population",
      additional_null_geoms = c(
        paste0("25025", c("061202", "981201", "981202", "981501", "981502")), 
        paste0("2502598", c("03", "07", "10", "11", "13", "16", "17", "18", "19"), "00")
      ),
      barCats = list(
        "Under 0.5" = "c20aa", "0.5 to 0.74" = "c20ab", "0.75 to 0.99" = "c20ac",
        "1.00 to 1.24" = "c20ad", "1.25 to 1.49" = "c20ae", "1.50 to 1.74" = "c20af",
        "1.75 to 1.84" = "c20ag", "1.85 to 1.99" = "c20ah", "2.00 and over" = "c20ai"
      ),
      summary_indicators = list(
        "Share of population under poverty line" = list(
          summary_expression = rlang::expr(
            (c20aa + c20ab + c20ac) / 
              (c20aa + c20ab + c20ac + c20ad + c20ae + c20af + c20ag + c20ah + c20ai)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of population under 200% of poverty line" = list(
          summary_expression = rlang::expr(
            (c20aa + c20ab + c20ac + c20ad + c20ae + c20af + c20ag + c20ah) / 
              (c20aa + c20ab + c20ac + c20ad + c20ae + c20af + c20ag + c20ah + c20ai)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### living arrangements -------
    "Living Arrangements" = list(
      data_code = "hhgqn", generalTopic = "Demographics",
      areas_categories_csv = 'csv/hh_gq_neigh_bins.csv',
      barTitle = "Population by living arrangement type", barhoverformat = ",.0f",
      null_description = "little to no population",
      barCats = list(
        "Household population" = "hh",
        "Group quarters population" = "gq_adj"
      ),
      summary_indicators = list(
        "Average household size" = list(
          summary_expression = rlang::expr(hh / hu),
          citywide_comparison = TRUE, map_legend_bins = seq(0, 4, length.out = 9),
          hoverformat = ",.2f", tickformat = ""
        ),
        "Total household population" = list(
          summary_expression = rlang::expr(hh),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total group quarters population" = list(
          summary_expression = rlang::expr(gq_adj),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses (with 2020 adjusted
        to reflect Boston's successful group quarters challenge), IPUMS-NHGIS, University of
      Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### housing units ----
    "Housing Units" = list(
      data_code = "hbicnhou", generalTopic = 'Housing',
      areas_categories_csv = 'csv/hbic_neigh_housing_bins.csv',
      barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
      barCats = list(
        "Occupied" = "occ",
        "Vacant" = "vac"
      ),
      summary_indicators = list(
        "Total housing units" = list(
          summary_expression = rlang::expr(vac + occ),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Housing units per acre" = list(
          summary_expression = rlang::expr((vac + occ) / landarea_acres),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of
      Minnesota, www.nhgis.org; Mayor's Office of Housing; BPDA Research Division Analysis"
    ),
    ### housing occupancy -----
    "Housing Occupancy" = list(
      data_code = "hbicnhouvac", generalTopic = 'Housing',
      areas_categories_csv = 'csv/hbic_neigh_housing_vacancy_bins.csv',
      barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
      barCats = list(
        "Occupied" = "occ",
        "Vacant" = "vac"
      ),
      summary_indicators = list(
        "Housing vacancy rate" = list(
          summary_expression = rlang::expr(vac / (vac + occ)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total vacant units" = list(
          summary_expression = rlang::expr(vac),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### housing tenure -----
    "Housing Tenure" = list(
      data_code = "hbicnhouten", generalTopic = 'Housing',
      areas_categories_csv = 'csv/hbic_neigh_housing_tenure_bins.csv',
      barTitle = "Occupied housing units by tenure", barhoverformat = ",.0f",
      barCats = list(
        "Owner-occupied" = "owner",
        "Renter-occupied" = "renter"
      ),
      summary_indicators = list(
        "Owner occupancy rate" = list(
          summary_expression = rlang::expr(owner / (owner + renter)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total owner-occupied units" = list(
          summary_expression = rlang::expr(owner),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total rental units" = list(
          summary_expression = rlang::expr(renter),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2010 Decennial Censuses, 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
      ),
    ### housing sales ------
    "Housing Sales" = list(
      data_code = "sales", generalTopic = 'Housing',
      areas_categories_csv = 'csv/sales_neighborhoods.csv', totalarea_categories_csv = 'csv/sales_citywide.csv',
      barTitle = "Number of sales transactions by unit type", barhoverformat = ",.0f",
      null_description = "few or no sales",
      barCats = list(
        "Single-family" = 'count_R1F',
        "Condominium" = 'count_RCD'
      ),
      summary_indicators = list(
        "Median condo unit price (nominal dollars)" = list(
          summary_expression = rlang::expr(median_nom_RCD),
          citywide_comparison = TRUE, disable_multiselection = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Median single-family unit price (nominal dollars)" = list(
          summary_expression = rlang::expr(median_nom_R1F),
          citywide_comparison = TRUE, disable_multiselection = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Median condo unit price (2023 inflation-adjusted dollars)" = list(
          summary_expression = rlang::expr(median_adj_RCD),
          citywide_comparison = TRUE, disable_multiselection = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Median single-family unit price (2023 inflation-adjusted dollars)" = list(
          summary_expression = rlang::expr(median_adj_R1F),
          citywide_comparison = TRUE, disable_multiselection = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total sales transactions" = list(
          summary_expression = rlang::expr(count_R1F + count_RCD),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      source = "Mayor's Office of Housing, The Warren Group, BPDA Research Division analysis",
      note = "Note: Only one neighborhood at a time can be selected for median price variables."
    ),
    ### commute mode ----
    "Commute Mode" = list(
      data_code = "commute_mode_neighs", generalTopic = 'Transportation',
      areas_categories_csv = 'csv/commutemode_neigh_bins.csv',
      barTitle = "Workers (16+) by primary commute mode", barhoverformat = ",.0f",
      null_description = "little to no population",
      barCats = list(
        "Car, truck, or van" = "car_truck_van",
        "Public transit" = "public_transportation",
        "Bicycle" = "bicycle",
        "Walked" = "walked",
        "Other means" = "other_means",
        "Worked from home" = "worked_from_home"
      ),
      summary_indicators = list(
        "Share of commuters (16+) walking or biking" = list(
          summary_expression = rlang::expr((bicycle + walked) /
                                             (total_workers_16_and_over - worked_from_home)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of commuters (16+) taking public transit" = list(
          summary_expression = rlang::expr(public_transportation /
                                             (total_workers_16_and_over - worked_from_home)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of commuters (16+) walking, biking, or taking transit" = list(
          summary_expression = rlang::expr((bicycle + walked + public_transportation) /
                                             (total_workers_16_and_over - worked_from_home)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of commuters (16+) taking a car, truck, or van" = list(
        summary_expression = rlang::expr(car_truck_van /
                                             (total_workers_16_and_over - worked_from_home)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of workers (16+) working from home" = list(
          summary_expression = rlang::expr(worked_from_home / (total_workers_16_and_over)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      note = "Note: Workers who commute by taxicab are included in the 'public transit' category.",
      source = "U.S. Census Bureau, 1990-2000 Decennial Censuses, 2006-2010 and 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### vehicles available ----
    "Vehicles Available" = list(
      data_code = "vehicles_available_neighs", generalTopic = 'Transportation',
      areas_categories_csv = 'csv/vehiclesavailable_neigh_bins.csv',
      barTitle = "Households by vehicles available", barhoverformat = ",.0f",
      null_description = "little to no households",
      barCats = list(
        "0 vehicles" = "no_vehicles",
        "1 vehicle" = "one_vehicle",
        "2 vehicles" = "two_vehicles",
        "3+ vehicles" = "three_or_more_vehicles"
      ),
      summary_indicators = list(
        "Share of households with no vehicles available" = list(
          summary_expression = rlang::expr(no_vehicles / total_occupied_units),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Share of households with two or more vehicles available" = list(
          summary_expression = rlang::expr((two_vehicles + three_or_more_vehicles) / total_occupied_units),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1960-2000 Decennial Censuses, 2006-2010 and 2016-2020 American
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    ### num of home loans ----
    "Number of Home Loans" = list(
      data_code = "numholoansnbhd", generalTopic = 'Loans',
      areas_categories_csv = 'csv/home_loans_nbhd_bins.csv',
      barTitle = "Number of home loans by type", barhoverformat = ",.0f",
      null_description = "little to no loans",
      barCats = list(
        "Home purchase" = "num_home_purchase",
        "Home improvement" = "num_home_improv",
        "Refinancing" = "num_home_refi"
      ),
      summary_indicators = list(
        "Total home purchase loans" = list(
          summary_expression = rlang::expr(num_home_purchase),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total home improvement loans" = list(
          summary_expression = rlang::expr(num_home_improv),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total home loans" = list(
          summary_expression = rlang::expr(num_home_purchase + num_home_improv + num_home_refi),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      note = "Note: All originated loans for single- and multi-family properties are included.",
      source = "Consumer Financial Protection Bureau, Home Mortgage Disclosure Act 2007-2022; BPDA Research Division Analysis"
    ),
    ### $ of home loans ----
    "Volume ($) of Home Loans" = list(
      data_code = "volholoansnbhd", generalTopic = 'Loans',
      areas_categories_csv = 'csv/home_loans_nbhd_bins.csv',
      barTitle = "Volume ($) of home loans by type", barhoverformat = ",.0f", bartickprefix = "$",
      null_description = "little to no loans",
      barCats = list(
        "Home purchase" = "vol_home_purchase",
        "Home improvement" = "vol_home_improv",
        "Refinancing" = "vol_home_refi"
      ),
      summary_indicators = list(
        "Total home purchase loan volume ($)" = list(
          summary_expression = rlang::expr(vol_home_purchase),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total home improvement loan volume ($)" = list(
          summary_expression = rlang::expr(vol_home_improv),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total home loan volume ($)" = list(
          summary_expression = rlang::expr(vol_home_purchase + vol_home_improv + vol_home_refi),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Average dollar value of home purchase loans" = list(
          summary_expression = rlang::expr(vol_home_purchase / num_home_purchase),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Average dollar value of home improvement loans" = list(
          summary_expression = rlang::expr(vol_home_improv / num_home_improv),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        )
      ),
      note = "Note: All originated loans for single- and multi-family properties are included. All monetary values are in 2022 inflation-adjusted dollars.",
      source = "Consumer Financial Protection Bureau, Home Mortgage Disclosure Act 2007-2022; BPDA Research Division Analysis"
    ),
    ### num of sm. bus. loans ----
    "Number of Small Business Loans" = list(
      data_code = "numbuloansnbhd", generalTopic = 'Loans',
      areas_categories_csv = 'csv/bus_loans_nbhd_bins.csv',
      barTitle = 'Number of Small (<$1M) Loans by Business Size', barhoverformat = ",.0f",
      barCats = list("Loans to small businesses" = "num_sml_sbus", "Loans to large businesses" = "num_sml_bbus"),
      null_description = "little to no loans",
      summary_indicators = list(
        "Share of small loans going to small businesses" = list(
          summary_expression = rlang::expr(num_sml_sbus/(num_sml_sbus + num_sml_bbus)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        ),
        "Total small loans" = list(
          summary_expression = rlang::expr(num_sml_sbus + num_sml_bbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        ),
        "Total small loans going to small businesses" = list(
          summary_expression = rlang::expr(num_sml_sbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickformat = ""
        )
      ),
      note = "Note: A business is considered small if it has <$1M in annual revenue.",
      source = "FFIEC, Community Reinvestment Act data, https://github.com/acforrester/community-reinvestment-act; BPDA Research Division analysis"
    ),
    ### $ of sm. bus. loans ----
    "Volume ($) of Small Business Loans" = list(
      data_code = 'volbuloansnbhd', generalTopic = 'Loans',
      areas_categories_csv = 'csv/bus_loans_nbhd_bins.csv',
      barTitle = 'Volume ($) of Small (<$1M) Loans by Business Size', barhoverformat = ",.0f", bartickprefix = "$",
      barCats = list("Loans to small businesses" = "vol_sml_sbus", "Loans to large businesses" = "vol_sml_bbus"),
      null_description = "little to no loans",
      summary_indicators = list(
        "Average dollar value of all small loans" = list(
          summary_expression = rlang::expr((vol_sml_sbus + vol_sml_bbus)/(num_sml_sbus + num_sml_bbus)),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Average dollar value of small loans to small businesses" = list(
          summary_expression = rlang::expr((vol_sml_sbus)/(num_sml_sbus)),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Average dollar value of small loans to large businesses" = list(
          summary_expression = rlang::expr((vol_sml_bbus)/(num_sml_bbus)),
          citywide_comparison = TRUE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total small loan volume ($)" = list(
          summary_expression = rlang::expr(vol_sml_sbus + vol_sml_bbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total small loan volume ($) going to small businesses" = list(
          summary_expression = rlang::expr(vol_sml_sbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Total small loan volume ($) going to large businesses" = list(
          summary_expression = rlang::expr(vol_sml_bbus),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = "$", tickformat = ""
        ),
        "Share of small loan volume ($) going to small businesses" = list(
          summary_expression = rlang::expr(vol_sml_sbus/(vol_sml_sbus + vol_sml_bbus)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickformat = ".0%"
        )
      ),
      note = "Note: A business is considered small if it has <$1M in annual revenue. All monetary values are in 2022 inflation-adjusted dollars.",
      source = "FFIEC, Community Reinvestment Act data, https://github.com/acforrester/community-reinvestment-act; BPDA Research Division analysis"
    )
  )
  ),
    ## Zip code areas ------
    "zip code areas" = list(geoms = zca_geoms, center_lat = 42.318, center_lon = -71.075, zoom_level = 12, topics = list(
      ### businesses by industry -----
      "Business Establishments by Industry" = list(
        data_code = 'bizind', generalTopic = 'Business Establishments',
        areas_categories_csv = 'csv/estabs_supersector_bins.csv',
        barTitle = 'Business Establishments by Broad Industry Grouping', 
        barhoverformat = ",.0f", smallbarfont = TRUE,
        barCats = list(
          "Production, Construction, and Logistics" = "prodcon",
          "Professional and Business Services" = "fireprof",
          "Health Care and Education" = "hced",
          "Food, Hospitality, and Entertainment" = "feara",
          "Retail Trade" = "retail",
          "Other Services" = "other"
        ),
        summary_indicators = list(
          "Total business establishments" = list(
            summary_expression = rlang::expr(prodcon + fireprof + hced + feara + retail + other + x),
            citywide_comparison = FALSE,
            hoverformat = ",.0f", tickformat = ""
          ),
          "Retail Trade share of establishments" = list(
            summary_expression = rlang::expr(retail/(prodcon + fireprof + hced + feara + retail + other + x)),
            citywide_comparison = TRUE,
            hoverformat = ".0%", tickformat = ".0%"
          ),
          "Food, Hospitality, and Entertainment share of establishments" = list(
            summary_expression = rlang::expr(feara/(prodcon + fireprof + hced + feara + retail + other + x)),
            citywide_comparison = TRUE,
            hoverformat = ".0%", tickformat = ".0%"
          ),
          "Professional and Business Services share of establishments" = list(
            summary_expression = rlang::expr(fireprof/(prodcon + fireprof + hced + feara + retail + other + x)),
            citywide_comparison = TRUE,
            hoverformat = ".0%", tickformat = ".0%"
          ),
          "Production, Construction, and Logistics share of establishments" = list(
            summary_expression = rlang::expr(prodcon/(prodcon + fireprof + hced + feara + retail + other + x)),
            citywide_comparison = TRUE,
            hoverformat = ".0%", tickformat = ".0%"
          )
        ),
        source = "U.S. Census Bureau, Zip Code Business Patterns; BPDA Research Division analysis"
      ),
      
      ### businesses by size ------
      "Business Establishments by Size" = list(
        data_code = 'bizemp', generalTopic = 'Business Establishments',
        areas_categories_csv = 'csv/estabs_empsize_bins.csv',
        barTitle = 'Business Establishments by Number of Employees', barhoverformat = ",.0f",
        barCats = list("1 to 4" = "micro", "5 to 9" = "sm5to9",
          "10 to 49" = "sm10to49", "50 or more" = "large"
        ),
        summary_indicators = list(
          "Total small and micro business establishments (1-49 employees)" = list(
            summary_expression = rlang::expr(micro + sm5to9 + sm10to49),
            citywide_comparison = FALSE,
            hoverformat = ",.0f", tickformat = ""
          ),
          "Micro business (1-4 employees) share of establishments" = list(
            summary_expression = rlang::expr((micro)/(micro + sm5to9 + sm10to49 + large)),
            citywide_comparison = TRUE,
            hoverformat = ".0%", tickformat = ".0%"
          ),
          "Small business (5-49 employees) share of establishments" = list(
            summary_expression = rlang::expr((sm5to9 + sm10to49)/(micro + sm5to9 + sm10to49 + large)),
            citywide_comparison = TRUE,
            hoverformat = ".0%", tickformat = ".0%"
          )
        ),
        source = "U.S. Census Bureau, Zip Code Business Patterns; BPDA Research Division analysis"
      ),
      
      ### num of sm. bus. loans ------
      "Numbers of Small Business Loans" = list(
        data_code = 'loannsb', generalTopic = 'Business Loans',
        areas_categories_csv = 'csv/loans_estabs_zcas_bins.csv',
        barTitle = 'Number of Small (<$1M) Loans by Business Size', barhoverformat = ",.0f",
        barCats = list("Loans to small businesses" = "num_sml_sbus", "Loans to large businesses" = "num_sml_bbus"),
        summary_indicators = list(
          "Number of small loans per 1000 business establishments" = list(
            summary_expression = rlang::expr((num_sml_sbus+num_sml_bbus)*1000/ESTAB),
            citywide_comparison = TRUE,
            hoverformat = ",.0f", tickformat = ""
          ),
          "Share of small loans going to small businesses" = list(
            summary_expression = rlang::expr(num_sml_sbus/(num_sml_sbus + num_sml_bbus)),
            citywide_comparison = TRUE,
            hoverformat = ".0%", tickformat = ".0%"
          ),
          "Total small loans" = list(
            summary_expression = rlang::expr(num_sml_sbus + num_sml_bbus),
            citywide_comparison = FALSE,
            hoverformat = ",.0f", tickformat = ""
          ),
          "Total small loans going to small businesses" = list(
            summary_expression = rlang::expr(num_sml_sbus),
            citywide_comparison = FALSE,
            hoverformat = ",.0f", tickformat = ""
          )
        ),
        note = "Note: A business is considered small if it has <$1M in annual revenue.",
        source = "FFIEC, Community Reinvestment Act data, https://github.com/acforrester/community-reinvestment-act; U.S. Census Bureau, Zip Code Business Patterns; BPDA Research Division analysis"
      ),
      ### $ of sm. bus. loans ------
      "Volume ($) of Small Business Loans" = list(
        data_code = 'loanvsb', generalTopic = 'Business Loans',
        areas_categories_csv = 'csv/loans_estabs_zcas_bins.csv',
        barTitle = 'Volume ($) of Small (<$1M) Loans by Business Size', barhoverformat = ",.0f", bartickprefix = "$",
        barCats = list("Loans to small businesses" = "vol_sml_sbus", "Loans to large businesses" = "vol_sml_bbus"),
        summary_indicators = list(
          "Average dollar value of all small loans" = list(
            summary_expression = rlang::expr((vol_sml_sbus + vol_sml_bbus)/(num_sml_sbus + num_sml_bbus)),
            citywide_comparison = TRUE,
            hoverformat = ",.0f", tickprefix = "$", tickformat = ""
          ),
          "Average dollar value of small loans to small businesses" = list(
            summary_expression = rlang::expr((vol_sml_sbus)/(num_sml_sbus)),
            citywide_comparison = TRUE,
            hoverformat = ",.0f", tickprefix = "$", tickformat = ""
          ),
          "Average dollar value of small loans to large businesses" = list(
            summary_expression = rlang::expr((vol_sml_bbus)/(num_sml_bbus)),
            citywide_comparison = TRUE,
            hoverformat = ",.0f", tickprefix = "$", tickformat = ""
          ),
          "Total small loan volume ($)" = list(
            summary_expression = rlang::expr(vol_sml_sbus + vol_sml_bbus),
            citywide_comparison = FALSE,
            hoverformat = ",.0f", tickprefix = "$", tickformat = ""
          ),
          "Total small loan volume ($) going to small businesses" = list(
            summary_expression = rlang::expr(vol_sml_sbus),
            citywide_comparison = FALSE,
            hoverformat = ",.0f", tickprefix = "$", tickformat = ""
          ),
          "Total small loan volume ($) going to large businesses" = list(
            summary_expression = rlang::expr(vol_sml_bbus),
            citywide_comparison = FALSE,
            hoverformat = ",.0f", tickprefix = "$", tickformat = ""
          ),
          "Share of small loan volume ($) going to small businesses" = list(
            summary_expression = rlang::expr(vol_sml_sbus/(vol_sml_sbus + vol_sml_bbus)),
            citywide_comparison = TRUE,
            hoverformat = ".0%", tickformat = ".0%"
          )
        ),
        note = "Note: A business is considered small if it has <$1M in annual revenue. All monetary values are in 2022 inflation-adjusted dollars.",
        source = "FFIEC, Community Reinvestment Act data, https://github.com/acforrester/community-reinvestment-act; BPDA Research Division analysis"
      )
      )
    )
)

# Prep data #######

# first, some validations of APP_CONFIG

for (geo_name in names(APP_CONFIG)) {
  if (
    !("GEOID" %in% names(APP_CONFIG[[geo_name]]$geoms)) 
    | # if the geoms do not have a GEOID attribute, or if that attribute is not unique, throw the error
    length(APP_CONFIG[[geo_name]]$geoms$GEOID) != length(unique(APP_CONFIG[[geo_name]]$geoms$GEOID))
    ) {
    stop(paste("Error: the specified geoms for", geo_name, "must have a GEOID attribute that uniquely identifies each feature. 
         More information: https://github.com/bpda-research-division/neighborhood-change/blob/main/ABOUT.md#preparing-geographic-features"))
  }
}

topic_codes <- APP_CONFIG %>% lapply(function(geo_type) geo_type$topics %>% lapply(function(topic) topic$data_code))
if (length(topic_codes) != length(unique(topic_codes))) {
  stop("Error: Duplicate data_code values detected - each data_code must be unique to a given topic and geographic unit.
       More information: https://github.com/bpda-research-division/neighborhood-change/blob/main/ABOUT.md#topic-parameters")
}

# # You can either prep data for individual topics...
# prep_data(APP_CONFIG[['zip code areas']]$topics[['Numbers of Small Business Loans']])
# prep_data(APP_CONFIG[['zip code areas']]$topics[['Volume ($) of Small Business Loans']])
# prep_data(APP_CONFIG[['census tracts']]$topics[['Children by Age']])
# prep_data(APP_CONFIG[['census tracts']]$topics[['Children by Race and Ethnicity']])
# prep_data(APP_CONFIG[['neighborhoods']]$topics[['Children by Age']])
# prep_data(APP_CONFIG[['neighborhoods']]$topics[['Housing Sales']])
#prep_data(APP_CONFIG[['census tracts']]$topics[['Commute Mode']])
#prep_data(APP_CONFIG[['neighborhoods']]$topics[['Commute Mode']])

prep_data(APP_CONFIG[['census tracts']]$topics[['Number of Home Loans']])
prep_data(APP_CONFIG[['census tracts']]$topics[['Volume ($) of Home Loans']])
prep_data(APP_CONFIG[['census tracts']]$topics[['Number of Small Business Loans']])
prep_data(APP_CONFIG[['census tracts']]$topics[['Volume ($) of Small Business Loans']])

prep_data(APP_CONFIG[['neighborhoods']]$topics[['Number of Home Loans']])
prep_data(APP_CONFIG[['neighborhoods']]$topics[['Volume ($) of Home Loans']])
prep_data(APP_CONFIG[['neighborhoods']]$topics[['Number of Small Business Loans']])
prep_data(APP_CONFIG[['neighborhoods']]$topics[['Volume ($) of Small Business Loans']])

#prep_data(APP_CONFIG[['census tracts']]$topics[['Vehicles Available']])
#prep_data(APP_CONFIG[['neighborhoods']]$topics[['Vehicles Available']])
# prep_data(APP_CONFIG[['neighborhoods']]$topics[['Housing Units']])
# prep_data(APP_CONFIG[['census tracts']]$topics[['Housing Units']])

# # ...or prep data for all topics
# for (geo_type in APP_CONFIG) {
#   for (topic in geo_type$topics) {
#     prep_data(topic)
#   }
# }

# always save config after making changes to it
APP_CONFIG %>% saveRDS(file='../config/APP_CONFIG.rds')
